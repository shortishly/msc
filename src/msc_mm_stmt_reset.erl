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

-module(msc_mm_stmt_reset).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, From},
             {request, #{action := stmt_reset,
                         statement_id := StatementId} = Packet},
             {stmt_reset, From},
             #{client_flags := ClientFlags,
               prepared := Prepared} = Data)
  when is_map_key(StatementId, Prepared) ->
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_packet_ok:decode(ClientFlags),
                           msmp_packet_error:decode(ClientFlags)])),
           encoder := msmp_codec:encode(
                        msmp_com_stmt_reset:encode(ClientFlags))},
     nei({send, #{packet => Packet, sequence => 0}})};

handle_event(internal,
             {recv, #{packet := #{action := error} = Packet}},
             {stmt_reset, From},
             Data) ->
    {next_state,
     authenticated,
     Data,
     [pop_callback_module,
      {reply,
       From,
       {error, maps:without([action], Packet)}}]};

handle_event(internal,
             {recv, #{packet := #{action := ok} = Packet}},
             {stmt_reset, From},
             Data) ->
    {next_state,
     authenticated,
     Data,
     [pop_callback_module,
      {reply,
       From,
       {ok, maps:without([action], Packet)}}]};

handle_event(EventType, EventContent, State, Data) ->
    msc_mm_common:handle_event(EventType,
                               EventContent,
                               State,
                               Data).
