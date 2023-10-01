%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @doc Middleman dealing with the query process.

-module(msc_mm_prepare).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, From},
             {request, #{action := prepare} = Packet},
             {prepare, From},
             #{client_flags := ClientFlags} = Data) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_com_stmt_prepare_ok:decode(ClientFlags),
                           msmp_packet_error:decode(ClientFlags)])),
           encoder := msmp_codec:encode(
                        msmp_com_stmt_prepare:encode(ClientFlags))},
     nei({send, #{packet => Packet, sequence => 0}})};

handle_event({call, _}, {request, _}, _, _) ->
    {keep_state_and_data, postpone};

handle_event(internal,
             {recv, #{packet := #{action := error} = Packet}},
             {prepare, From},
             Data) ->
    {next_state,
     authenticated,
     Data,
     [pop_callback_module,
      {reply,
       From,
       {error, maps:without([action], Packet)}}]};

handle_event(
  internal,
  {recv,
   #{packet := #{action := com_stmt_prepare_ok,
                 statement_id := StatementId,
                 num_params := 0,
                 num_columns := 0}}},
  {prepare, From},
  #{prepared := Prepared} = Data)
  when not(is_map_key(StatementId, Prepared)) ->
    {next_state,
     authenticated,
     Data#{prepared := Prepared#{StatementId => #{params => [],
                                                  columns => []}}},
     [pop_callback_module, {reply, From, {ok, StatementId}}]};

handle_event(
  internal,
  {recv,
   #{packet := #{action := com_stmt_prepare_ok,
                 statement_id := StatementId} = Packet}},
  _,
  #{prepared := Prepared} = Data)
  when not(is_map_key(StatementId, Prepared)) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        msmp_column_definition:decode()),
           prepare => maps:with(
                        [statement_id,
                         num_params,
                         num_columns],
                        Packet),
           prepared := Prepared#{StatementId => #{params => [],
                                                  columns => []}}}};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  {prepare, From},
  #{prepare := #{statement_id := StatementId,
                 num_params := 1,
                 num_columns := 0},
    prepared := Prepared} = Data) ->
    #{StatementId := #{params := Params} = Definition} = Prepared,
    {next_state,
     authenticated,
     maps:without(
       [prepare],
       Data#{prepared :=
                 Prepared#{StatementId :=
                               Definition#{params := lists:reverse(
                                                       [Packet | Params])}}}),
     [pop_callback_module, {reply, From, {ok, StatementId}}]};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  _,
  #{prepare := #{statement_id := StatementId,
                 num_params := NumParams} = Prepare,
    prepared := Prepared} = Data)
  when NumParams > 0 ->
    #{StatementId := #{params := Params} = Definition} = Prepared,
    {keep_state,
     Data#{prepare := Prepare#{num_params := NumParams - 1},
           prepared :=
               Prepared#{StatementId :=
                             Definition#{params := [Packet | Params]}}}};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  _,
  #{prepare := #{statement_id := StatementId,
                 num_columns := NumColumns} = Prepare,
    prepared := Prepared} = Data)
  when NumColumns > 1 ->
    #{StatementId := #{columns := Columns} = Definition} = Prepared,
    {keep_state,
     Data#{prepare := Prepare#{num_columns := NumColumns - 1},
           prepared :=
               Prepared#{StatementId :=
                             Definition#{columns := [Packet | Columns]}}}};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  {prepare, From},
  #{prepare := #{statement_id := StatementId,
                 num_columns := 1},
    prepared := Prepared} = Data) ->
    #{StatementId := #{columns := Columns,
                       params := Params} = Definition} = Prepared,
    {next_state,
     authenticated,
     maps:without(
       [prepare],
       Data#{prepared :=
                 Prepared#{StatementId :=
                               Definition#{columns := lists:reverse(
                                                        [Packet | Columns]),
                                           params := lists:reverse(Params)}}}),
       [pop_callback_module, {reply, From, {ok, StatementId}}]};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  _,
  #{client_flags := ClientFlags} = Data) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_packet_eof:decode(ClientFlags)]))},
     nei({add_definition, Packet})};

handle_event(
  internal,
  {add_definition, Packet},
  _,
  #{prepare :=
        #{columns :=
              #{definitions := Definitions} = Columns} = Prepare} = Data) ->
    {keep_state,
     Data#{prepare :=
               Prepare#{columns :=
                            Columns#{definitions :=
                                         [maps:without(
                                            [action],
                                            Packet) |
                                          Definitions]}}}};

handle_event(
  internal,
  {recv, #{packet := #{action := text_resultset, row := Row}}},
  _,
  #{prepare := #{rows := Rows} = Prepare} = Data) ->
    {keep_state, Data#{prepare := Prepare#{rows := [Row | Rows]}}};

handle_event(internal,
             {recv, #{packet := #{action := eof}}},
             {prepare, From},
             #{prepare := #{columns := #{definitions := Columns},
                            rows := Rows}} = Data) ->
    {next_state,
     authenticated,
     maps:without([prepare], Data),
     [pop_callback_module,
      {reply, From, {lists:reverse(Columns), lists:reverse(Rows)}}]};

handle_event(EventType, EventContent, State, Data) ->
    msc_mm_common:handle_event(EventType,
                               EventContent,
                               State,
                               Data).
