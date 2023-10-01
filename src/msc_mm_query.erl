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

-module(msc_mm_query).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, From},
             {request, #{action := query} = Packet},
             {query, From},
             #{client_flags := ClientFlags} = Data) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_column_count:decode(),
                           msmp_packet_ok:decode(ClientFlags),
                           msmp_packet_error:decode(ClientFlags)])),
           query => #{columns => #{definitions => []},
                      rows => []},
           encoder := msmp_codec:encode(msmp_com_query:encode(ClientFlags))},
     nei({send, #{packet => Packet, sequence => 0}})};

handle_event({call, _}, {request, _}, _, _) ->
    {keep_state_and_data, postpone};

handle_event(
  internal,
  {recv,
   #{packet := #{action := column_count, column_count := ColumnCount}}},
  _,
  #{query := #{columns := Columns} = Query} = Data) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        msmp_column_definition:decode()),
           query := Query#{columns := Columns#{count => ColumnCount}}}};

handle_event(internal,
             {recv, #{packet := #{action := error} = Packet}},
             {query, From},
             Data) ->
    {next_state,
     authenticated,
     maps:without([query], Data),
     [pop_callback_module,
      {reply,
       From,
       {error, maps:without([action], Packet)}}]};

handle_event(internal,
             {recv, #{packet := #{action := ok} = Packet}},
             {query, From},
             Data) ->
    {next_state,
     authenticated,
     maps:without([query], Data),
     [pop_callback_module,
      {reply,
       From,
       {ok, maps:without([action], Packet)}}]};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  _,
  #{query := #{columns := #{definitions := Definitions,
                            count := ColumnCount}}})
    when length(Definitions) + 1 < ColumnCount ->
    {keep_state_and_data, nei({add_definition, Packet})};

handle_event(
  internal,
  {recv, #{packet := #{action := column_definition} = Packet}},
  _,
  #{client_flags := ClientFlags} = Data) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_text_resultset_row:decode(),
                           msmp_packet_eof:decode(ClientFlags)]))},
     nei({add_definition, Packet})};

handle_event(
  internal,
  {add_definition, Packet},
  _,
  #{query := #{columns := #{definitions := Defs} = Cols} = Query} = Data) ->
    {keep_state,
     Data#{query := Query#{columns := Cols#{definitions := Defs ++
                                                [maps:without(
                                                   [action],
                                                   Packet)]}}}};

handle_event(
  internal,
  {recv, #{packet := #{action := text_resultset_row, row := Row}}},
  _,
  #{query := #{columns := #{definitions := Definitions}}}) ->
    {keep_state_and_data,
     nei({add_row,
          lists:map(
            fun
                ({Definition, Column}) ->
                    {<<>>, Decoded} = (msmp_text:decode(Definition))(Column),
                    Decoded
            end,
            lists:zip(Definitions, Row))})};

handle_event(internal,
             {add_row, Row},
             _,
             #{query := #{rows := Rows} = Query} = Data) ->
    {keep_state,
     Data#{query := Query#{rows := [Row | Rows]}}};

handle_event(internal,
             {recv, #{packet := #{action := eof}}},
             {query, From},
             #{query := #{columns := #{definitions := Columns},
                          rows := Rows}} = Data) ->
    {next_state,
     authenticated,
     maps:without([query], Data),
     [pop_callback_module,
      {reply,
       From,
       {lists:map(
          fun
              (Column) ->
                  maps:with([catalog, schema, name, table, type], Column)
          end,
          Columns),
        lists:reverse(Rows)}}]};

handle_event(EventType, EventContent, State, Data) ->
    msc_mm_common:handle_event(EventType,
                               EventContent,
                               State,
                               Data).
