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

%% @doc Middleman dealing with the register replica process.

-module(msc_mm_register_replica).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.

handle_event({call, From},
             {request, #{action := register_replica} = Packet},
             {register_replica, From},
             #{client_flags := ClientFlags} = Data) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(msmp_packet_ok:decode(ClientFlags)),
           encoder := msmp_codec:encode(msmp_register_replica:encode())},
     nei({send, #{packet => Packet, sequence => 0}})};


handle_event(internal,
             {recv, #{packet := #{action := ok} = Packet}},
             {register_replica, From},
             Data) ->
    {next_state,
     authenticated,
     Data,
     [pop_callback_module,
      {reply, From, {ok, maps:without([action], Packet)}}]};


handle_event(EventType, EventContent, State, Data) ->
    msc_mm_common:handle_event(EventType,
                               EventContent,
                               State,
                               Data).
