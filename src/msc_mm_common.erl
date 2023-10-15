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

%% @doc Internal events that are common for all middlemen.

-module(msc_mm_common).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event(internal,
             {send = Command, #{packet := #{} = Packet} = Decoded},
             _,
             #{requests := Requests,
               encoder := Encoder,
               socket := Socket} = Data) ->
    Encoded = Encoder(Decoded),
    ?LOG_DEBUG(#{decoded => Decoded, encoded => Encoded}),
    {keep_state,
     Data#{requests := msc_socket:send(
                         #{server_ref => Socket,
                           label => {?MODULE, Command},
                           message => Encoded,
                           requests => Requests})},
     nei({telemetry,
          Command,
          maps:merge(
            #{count => 1},
            maps:with([action], Packet))})};

handle_event(internal,
             {send = Command, Decoded},
             _,
             #{requests := Requests,
               encoder := Encoder,
               socket := Socket} = Data) ->
    Encoded = Encoder(Decoded),
    ?LOG_DEBUG(#{decoded => Decoded, encoded => Encoded}),
    {keep_state,
     Data#{requests := msc_socket:send(
                         #{server_ref => Socket,
                           label => {?MODULE, Command},
                           message => Encoded,
                           requests => Requests})}};

handle_event(internal,
             upgrade = Command,
             _,
             #{requests := Requests,
               socket := Socket} = Data) ->
    {keep_state,
     Data#{requests := msc_socket:upgrade(
                         #{server_ref => Socket,
                           label => {?MODULE, Command},
                           requests => Requests})},
     nei({telemetry, Command, #{count => 1}})};

handle_event(internal, {response, #{label := {?MODULE, _}, reply := ok}}, _, _) ->
    keep_state_and_data;

handle_event(internal, {response, #{label := msc_socket, reply := ok}}, _, _) ->
    keep_state_and_data;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, Reply}, Label, Updated} ->
            {keep_state,
             Data#{requests := Updated},
             nei({response, #{label => Label, reply => Reply}})};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end;

handle_event({call, From},
             {recv = Command,
              <<Length:24/little, _:8, _:Length/bytes>> = Encoded},
             _,
             #{decoder := Decoder}) ->
    ?LOG_DEBUG(#{encoded => Encoded}),

    case Decoder(Encoded) of
        {<<>>,
         #{packet := #{action := log_event,
                       header := #{event_type := Action}}} = Decoded} ->
            ?LOG_DEBUG(#{decoded => Decoded}),
            {keep_state_and_data,
             [{reply, From, ok},
              nei({telemetry,
                   Command,
                   #{count => 1, action => Action}}),
              nei({Command, Decoded})]};

        {<<>>, #{packet := Packet} = Decoded} ->
            ?LOG_DEBUG(#{decoded => Decoded}),
            {keep_state_and_data,
             [{reply, From, ok},
              nei({telemetry,
                   Command,
                   maps:merge(
                     #{count => 1},
                     maps:with([action], Packet))}),
              nei({Command, Decoded})]};

        nomatch ->
            ?LOG_WARNING(#{nomatch => Encoded}),
            {keep_state_and_data,
             [{reply, From, ok},
              nei({telemetry, Command, #{error => 1}})]}
    end;

handle_event(internal,
             {telemetry, EventName, Measurements},
             _,
             _) ->
    {keep_state_and_data,
     nei({telemetry, EventName, Measurements, #{}})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             _) when is_atom(EventName) ->
    {keep_state_and_data,
     nei({telemetry, [EventName], Measurements, Metadata})};

handle_event(internal,
             {telemetry, EventName, Measurements, Metadata},
             _,
             Data) ->
    ok = telemetry:execute([msc, mm] ++ EventName,
                           Measurements,
                           maps:merge(
                             maps:with([operator, client_flags], Data),
                             Metadata)),
    keep_state_and_data.
