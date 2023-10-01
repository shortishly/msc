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

-module(msc_mm_execute).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, From},
             {request, #{action := execute,
                         statement_id := StatementId} = Packet},
             {execute, From},
             #{prepared := Prepared,
               client_flags := ClientFlags} = Data)
  when is_map_key(StatementId, Prepared)->
    #{StatementId := #{params := Params}} = Prepared,

    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_column_count:decode(),
                           msmp_packet_ok:decode(ClientFlags),
                           msmp_packet_error:decode(ClientFlags)])),
           execute => #{columns => #{definitions => []},
                        rows => []},
           encoder := msmp_codec:encode(
                        msmp_com_stmt_execute:encode(ClientFlags))},
     nei({send,
          #{packet => Packet#{flags => 0,
                              iteration_count => 1,
                              new_params_bind_flag => 1,
                              types => Params},
            sequence => 0}})};

handle_event({call, _}, {request, _}, _, _) ->
    {keep_state_and_data, postpone};

handle_event(internal,
             {recv, #{packet := #{action := error} = Packet}},
             {execute, From},
             Data) ->
    {next_state,
     authenticated,
     maps:without([execute], Data),
     [pop_callback_module,
      {reply,
       From,
       {error, maps:without([action], Packet)}}]};

handle_event(internal,
             {recv, #{packet := #{action := ok} = Packet}},
             {execute, From},
             Data) ->
    {next_state,
     authenticated,
     maps:without([execute], Data),
     [pop_callback_module,
      {reply,
       From,
       {ok, maps:without([action], Packet)}}]};

handle_event(
  internal,
  {recv,
   #{packet := #{action := column_count, column_count := ColumnCount}}},
  _,
  #{execute := #{columns := Columns} = Execute} = Data) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        msmp_column_definition:decode()),
           execute := Execute#{columns := Columns#{count => ColumnCount}}}};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  _,
  #{execute := #{columns := #{definitions := Definitions,
                              count := ColumnCount}}})
    when length(Definitions) + 1 < ColumnCount ->
    {keep_state_and_data, nei({add_definition, Packet})};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  _,
  _) ->
    {keep_state_and_data,
     [nei({add_definition, Packet}), nei(ready_for_resultset)]};

handle_event(
  internal,
  ready_for_resultset,
  _,
  #{client_flags := ClientFlags,
    execute := #{columns := #{definitions := Definitions}}} = Data) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_binary_resultset_row:decode(
                             lists:reverse(Definitions)),
                           msmp_packet_eof:decode(ClientFlags)]))}};

handle_event(
  internal,
  {add_definition, Packet},
  _,
  #{execute :=
        #{columns :=
              #{definitions := Definitions} = Columns} = Execute} = Data) ->
    {keep_state,
     Data#{execute :=
               Execute#{columns :=
                            Columns#{definitions :=
                                         [maps:without(
                                            [action],
                                            Packet) |
                                          Definitions]}}}};

handle_event(
  internal,
  {recv, #{packet := #{action := binary_resultset_row, row := Row}}},
  _,
  #{execute := #{rows := Rows} = Execute} = Data) ->
    {keep_state, Data#{execute := Execute#{rows := [Row | Rows]}}};

handle_event(internal,
             {recv, #{packet := #{action := eof}}},
             {execute, From},
             #{execute := #{columns := #{definitions := Columns},
                            rows := Rows}} = Data) ->
    {next_state,
     authenticated,
     maps:without([execute], Data),
     [pop_callback_module,
      {reply, From, {lists:reverse(Columns), lists:reverse(Rows)}}]};

handle_event(EventType, EventContent, State, Data) ->
    msc_mm_common:handle_event(EventType,
                               EventContent,
                               State,
                               Data).
