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

%% @doc An example binlog dump receiver using ETS.

-module(msc_binlog_ets).


-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([start/0]).


start() ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [], []).


callback_mode() ->
    handle_event_function.


init([]) ->
    process_flag(trap_exit, true),
    {ok,
     ready,
     #{mapped => #{},
       requests => gen_statem:reqids_new()}}.


handle_event(
  {call, From},
  {table_map, #{table_id := TableId} = Mapping},
  _,
  #{mapped := Mapped} = Data)  when is_map_key(TableId, Mapped) ->
    {keep_state,
     Data#{mapped := Mapped#{TableId => Mapping}},
     {reply, From, ok}};

handle_event(
  {call, From},
  {table_map, #{table_id := TableId} = Mapping},
  _,
  #{mapped := Mapped} = Data) ->
    _ = new_table(Mapping),
    {keep_state,
     Data#{mapped := Mapped#{TableId => Mapping}},
     {reply, From, ok}};

handle_event(
  {call, From},
  {write_rows, #{rows := Rows, table_id := TableId}},
  _,
  #{mapped := Mapped}) when is_map_key(TableId, Mapped) ->
    #{TableId := Mapping} = Mapped,
    ets:insert(table_name(Mapping),
               [insert_or_update_tuple(Row, Mapping) || Row <- Rows]),
    {keep_state_and_data, {reply, From, ok}};

handle_event({call, From}, _Event, _State, _Data) ->
    {keep_state_and_data, {reply, From, ok}}.


new_table(Mapping) ->
    ets:new(table_name(Mapping),
            [{keypos, keypos(Mapping)}, protected, named_table]).


table_name(#{table := Table, database := Database}) ->
    binary_to_atom(<<Database/bytes, "_", Table/bytes>>).


keypos(#{metadata := #{simple_primary_key := [Key]}}) ->
    Key + 1;

keypos(#{metadata := #{simple_primary_key := _}}) ->
    1.


insert_or_update_tuple(
  Tuple,
  #{metadata := #{simple_primary_key := [_]}}) ->
    Tuple;

insert_or_update_tuple(
  Tuple,
  #{metadata := #{simple_primary_key := Composite}} = Mapping) ->
    list_to_tuple(
      [key(Tuple, Mapping) |
       lists:filtermap(
         fun
             ({Position, Value}) ->
                 case lists:member(Position, Composite) of
                     true ->
                         false;

                     false ->
                         {true, Value}
                 end
         end,
         lists:zip(
           lists:seq(0, tuple_size(Tuple) - 1),
           tuple_to_list(Tuple)))]).


key(Tuples, KeyPositions) when is_list(Tuples) ->
    [?FUNCTION_NAME(Tuple, KeyPositions) || Tuple <- Tuples];

key(Tuple, #{metadata := #{simple_primary_key := [Primary]}}) ->
    element(Primary + 1, Tuple);

key(Tuple, #{metadata := #{simple_primary_key := Composite}}) ->
    list_to_tuple([element(Position + 1, Tuple) || Position <- Composite]).
