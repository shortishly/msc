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


%% @doc Middleman.

-module(msc_mm).


-export([binlog_dump/1]).
-export([callback_mode/0]).
-export([handle_event/4]).
-export([init/1]).
-export([query/1]).
-export([recv/1]).
-export([register_replica/1]).
-export([start_link/1]).
-import(msc_statem, [nei/1]).
-import(msc_statem, [send_request/1]).
-include_lib("kernel/include/logger.hrl").


start_link(Arg) ->
    gen_statem:start_link(?MODULE, [Arg], envy_gen:options(?MODULE)).


recv(#{message := Message} = Arg) ->
    send_request(
      maps:without(
        [message],
        maybe_label(
          Arg#{request => {?FUNCTION_NAME, Message}}))).


query(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


register_replica(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


binlog_dump(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


send_request(Arg, Action) ->
    ?FUNCTION_NAME(Arg, Action, config(Action)).


send_request(Arg, Action, Config) ->
    send_request(
      maps:without(
        keys(Config),
        maybe_label(
          Arg#{request => {request, args(Action, Arg, Config)}}))).


config(binlog_dump) ->
    [{flags, 2},
     {binlog_pos, 4},
     {server_id, 200},
     call_back,
     {binlog_filename, <<>>}];

config(query) ->
    [{parameter_count, 0}, {parameter_set, 1}, query];

config(register_replica) ->
    [{port, 3306},
     {user, <<>>},
     {host, <<>>},
     {server_id, 200},
     {password, <<>>},
     {recovery_rank, <<0, 0, 0, 0>>},
     {master_id, 0}].


keys(Config) ->
    lists:map(
      fun
          ({Key, _}) ->
              Key;

          (Key) ->
              Key
      end,
      Config).


args(Action, Arg, Config) ->
    lists:foldl(
      fun
          ({Parameter, Default}, A) ->
              A#{Parameter => maps:get(Parameter, Arg, Default)};

          (Parameter, A) ->
              A#{Parameter => maps:get(Parameter, Arg)}
      end,
      #{action => Action},
      Config).


maybe_label(#{requests := _, label := _} = Arg) ->
    Arg;

maybe_label(#{requests := _} = Arg) ->
    Arg#{label => ?MODULE};

maybe_label(Arg) ->
    Arg.


init([Arg]) ->
    process_flag(trap_exit, true),
    {ok,
     unready,
     #{requests => gen_statem:reqids_new(),
       decoder => msmp_codec:decode(
                    msmp_handshake:decode()),
       encoder => msmp_codec:encode(
                    msmp_handshake_response:encode()),
       config => Arg},
     nei(peer)}.


callback_mode() ->
    handle_event_function.


handle_event(internal, peer, _, Data) ->
    case msc_sup:get_child(hd(get('$ancestors')), socket) of
        {_, PID, worker, _} when is_pid(PID) ->
            {keep_state, Data#{socket => PID}};

        {_, _, _, _} = Reason ->
            {stop, Reason};

        false ->
            {stop, peer_not_found}
    end;

handle_event({call, From},
             {request, #{action := binlog_dump}},
             authenticated,
             Data) ->
    {next_state,
     {binlog_dump, From},
     Data,
     [{push_callback_module, msc_mm_binlog_dump}, postpone]};

handle_event({call, From},
             {request, #{action := query}},
             authenticated,
             Data) ->
    {next_state,
     {query, From},
     Data,
     [{push_callback_module, msc_mm_query}, postpone]};

handle_event({call, From},
             {request, #{action := register_replica}},
             authenticated,
             Data) ->
    {next_state,
     {register_replica, From},
     Data,
     [{push_callback_module, msc_mm_register_replica}, postpone]};

handle_event({call, From}, {request, Packet}, authenticated, _) ->
    {keep_state_and_data,
     [{reply, From, ok},
      nei({send, #{packet => Packet, sequence => 0}})]};

handle_event({call, _}, {request, _}, _, _) ->
    {keep_state_and_data, [nei(connect), postpone]};

handle_event(internal,
             connect,
             _,
             #{requests := Requests, socket := Socket} = Data) ->
    {keep_state,
     Data#{requests := msc_socket:connect(
                         #{server_ref => Socket,
                           requests => Requests})},
     {push_callback_module, msc_mm_auth}};

handle_event(EventType, EventContent, State, Data) ->
    msc_mm_common:handle_event(EventType,
                               EventContent,
                               State,
                               Data).
