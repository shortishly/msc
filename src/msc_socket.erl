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

%% @doc Responsible for dealing with a TCP socket, backing off on
%% failure, upgrading to TLS and sending raw protocol to a middleman.

-module(msc_socket).


-export([callback_mode/0]).
-export([connect/1]).
-export([handle_event/4]).
-export([init/1]).
-export([send/1]).
-export([start/1]).
-export([start_link/1]).
-export([terminate/3]).
-export([upgrade/1]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").


start_link(Arg) ->
    gen_statem:start_link(?MODULE, [Arg], envy_gen:options(?MODULE)).


start(Arg) ->
    gen_statem:start(?MODULE, [Arg], [{debug, [trace]}]).


connect(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).

send(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).

upgrade(Arg) ->
    send_request(Arg, ?FUNCTION_NAME).


send_request(Arg, Action) ->
    ?FUNCTION_NAME(Arg, Action, config(Action)).


send_request(Arg, Action, Config) ->
    send_request(
      maps:without(
        keys(Config),
        maybe_label(
          Arg#{request => {request, args(Action, Arg, Config)}}))).


maybe_label(#{requests := _, label := _} = Arg) ->
    Arg;

maybe_label(#{requests := _} = Arg) ->
    Arg#{label => ?MODULE};

maybe_label(Arg) ->
    Arg.


config(send) ->
    [message];

config(upgrade) ->
    [];

config(connect) ->
    [].


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


send_request(#{label := _} = Arg) ->
    msc_statem:send_request(Arg);

send_request(Arg) ->
    msc_statem:send_request(Arg#{label => ?MODULE}).


init([Arg]) ->
    process_flag(trap_exit, true),
    {ok,
     disconnected,
     #{requests => gen_statem:reqids_new(),
       config => Arg,
       telemetry => #{}},
     nei(peer)}.


callback_mode() ->
    handle_event_function.


handle_event(internal, peer, _, Data) ->
    case msc_sup:get_child(hd(get('$ancestors')), mm) of
        {_, PID, worker, _} when is_pid(PID) ->
            {keep_state, Data#{peer => PID}};

        {_, _, _, _} = Reason ->
            {stop, Reason};

        false ->
            {stop, peer_not_found}
    end;

handle_event({call, {Peer, _} = From},
             {request,
              #{action := send,
                message := Message}},
             connected,
             #{peer := Peer}) ->
    {keep_state_and_data,
     [nei({send, Message}), {reply, From, ok}]};

handle_event({call, {Peer, _} = From},
             {request, #{action := upgrade}},
             connected,
             #{peer := Peer}) ->
    {keep_state_and_data, nei({upgrade, From})};

handle_event({call, {Peer, _} = From},
             {request, #{action := connect}},
             _,
             Data) ->
    {keep_state, Data#{peer => Peer}, [{reply, From, ok}, nei(connect)]};

handle_event(internal,
             {telemetry, EventName, Measurements},
             _,
             _) ->
    {keep_state_and_data,
     nei({telemetry, EventName, Measurements, #{}})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             Data) ->
    ok = telemetry:execute(
           [msc, socket, EventName],
           Measurements,
           maps:merge(
             maps:with([peer, socket, telemetry], Data),
             Metadata)),
    keep_state_and_data;

handle_event(internal,
             {error, #{event := EventName,  reason := Reason}},
             _,
             Data) ->
    {next_state,
     limbo,
     Data,
     [nei({telemetry,
           error,
           #{count => 1},
           #{event => EventName, reason => Reason}}),

      {state_timeout,
       timer:seconds(
         backoff:rand_increment(
           msc_config:backoff(rand_increment))),
       {backoff, #{action => EventName, reason => Reason}}}]};

handle_event(internal,
             {send, #{packet := _, sequence := _} = Decoded},
             _,
             #{encoder := Encoder}) ->
    {keep_state_and_data, nei({send, Encoder(Decoded)})};

handle_event(internal, {send, <<>>}, _, _) ->
    keep_state_and_data;

handle_event(internal, {send = EventName, Data}, _, #{tls := TLS}) ->
    case ssl:send(TLS, Data) of
        ok ->
            {keep_state_and_data,
             nei({telemetry,
                  EventName,
                  #{bytes => iolist_size(Data)}})};

        {error, Reason} ->
            {keep_state_and_data,
             nei({error, #{reason => Reason, event => EventName}})}
    end;

handle_event(internal, {send = EventName, Data}, _, #{socket := Socket}) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            {keep_state_and_data,
             nei({telemetry,
                  EventName,
                  #{bytes => iolist_size(Data)}})};

        {error, Reason} ->
            {keep_state_and_data,
             nei({error, #{reason => Reason, event => EventName}})}
    end;

handle_event(info,
             {tcp_closed, Socket},
             _,
             #{socket := Socket}) ->
    stop;

handle_event(info,
             {tcp, Socket, Received},
             _,
             #{socket := Socket, partial := Partial} = Data) ->
    {keep_state,
     Data#{partial := <<>>},
     [nei({telemetry,
           recv,
           #{bytes => iolist_size(Received)},
           #{}}),
      nei({recv, iolist_to_binary([Partial, Received])})]};

handle_event(info,
             {ssl_error, TLS, {tls_alert, _} = Reason},
             _,
             #{tls := TLS}) ->
    {stop, Reason};

handle_event(info,
             {ssl_closed, TLS},
             _,
             #{tls := TLS}) ->
    stop;

handle_event(info,
             {ssl, TLS, Received},
             _,
             #{tls := TLS, partial := Partial} = Data) ->
    {keep_state,
     Data#{partial := <<>>},
     [nei({telemetry,
           recv,
           #{bytes => iolist_size(Received)},
           #{}}),
      nei({recv, iolist_to_binary([Partial, Received])})]};

handle_event(info, {'DOWN', _, process, Peer, noproc}, _, #{peer := Peer}) ->
    stop;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             [nei({telemetry,
                   reqids_size,
                   #{value => gen_statem:reqids_size(Updated)}}),
              nei({response, #{label => Label, reply => Reply}})]};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end;

handle_event(internal, {response, #{label := msc_mm, reply :=  ok}}, _, _) ->
    keep_state_and_data;

handle_event(
  internal,
  {recv,
   <<Length:24/little, Sequence:8, Packet:Length/bytes, Remainder/bytes>>},
  _,
  #{peer := Peer,
    requests := Requests} = Data) ->
    {keep_state,
     Data#{requests := msc_mm:recv(
                         #{server_ref => Peer,
                           message => <<Length:24/little,
                                        Sequence:8,
                                        Packet:Length/bytes>>,
                           requests => Requests})},
      nei({recv, Remainder})};

handle_event(internal, {recv, <<>>}, _, _) ->
    keep_state_and_data;

handle_event(internal, {recv, Partial}, _, #{partial := <<>>} = Data) ->
    {keep_state, Data#{partial := Partial}};

handle_event(internal,
             connect,
             _,
             #{config := #{host := Host, port := Port}}) ->
    {keep_state_and_data,
     nei({connect,
          #{family => inet,
            port => Port,
            addr => addr(Host)}})};

handle_event(internal,
             {connect = EventName,
              #{family := Family, port := Port, addr := Addr} = SockAddr},
             _,
             #{telemetry := Telemetry} = Data) ->

    case gen_tcp:connect(SockAddr,
                         [{mode, binary}],
                         msc_config:timeout(EventName)) of
        {ok, Socket} ->
            {next_state,
             connected,
             Data#{partial => <<>>,
                   socket => Socket,
                   telemetry :=
                       Telemetry#{net => #{peer => #{name => Addr,
                                                     port => Port},
                                           sock => #{family => Family}}}},
             nei({telemetry, EventName, #{count => 1}})};

        {error, Reason} ->
            {keep_state_and_data,
             nei({error, #{reason => Reason, event => EventName}})}
    end;

handle_event(internal,
             {upgrade, From},
             _,
             #{socket := Socket} = Data) ->
    case ssl:connect(Socket, [{verify, verify_none}]) of
        {ok, TLS} ->
            {keep_state, Data#{tls => TLS}, {reply, From, ok}};

        {error, Reason} = Error ->
            {stop_and_reply, Reason, {reply, From, Error}}
    end;

handle_event(state_timeout, {backoff, _}, limbo, _) ->
    stop.


terminate(_Reason, _State, #{tls := TLS, socket := Socket}) ->
    _ = ssl:close(TLS),
    _ = gen_tcp:close(Socket);

terminate(_Reason, _State, #{socket := Socket}) ->
    _ = gen_tcp:close(Socket);

terminate(_Reason, _State, _Data) ->
    ok.


addr(Hostname) ->
    case inet:gethostbyname(binary_to_list(Hostname)) of
        {ok, #hostent{h_addr_list = Addresses}} ->
            pick_one(Addresses);

        {error, _} = Error ->
            Error
    end.


pick_one(L) ->
    lists:nth(rand:uniform(length(L)), L).
