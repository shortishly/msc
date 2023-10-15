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

%% @doc Middleman dealing with the binlog dump process.

-module(msc_mm_binlog_dump).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, From},
             {request, #{action := Action,
                         call_back := CallBack} = Packet},
             {Action, From},
             #{client_flags := ClientFlags} = Data)
  when Action == binlog_dump;
       Action == binlog_dump_gtid ->
    Mapped = #{mapped => #{}},
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_binlog_network_stream:decode(Mapped),
                           msmp_packet_eof:decode(ClientFlags),
                           msmp_packet_error:decode(ClientFlags)])),
           binlog_dump => Mapped,
           call_back => CallBack,
           encoder := msmp_codec:encode(
                        case Action of
                            binlog_dump ->
                                msmp_binlog_dump:encode();

                            binlog_dump_gtid ->
                                msmp_binlog_dump_gtid:encode()
                        end)},
     nei({send, #{packet => maps:without([call_back], Packet), sequence => 0}})};

handle_event(internal,
             {recv, #{packet := #{action := error} = Packet}},
             {Action, From},
             _)
  when Action == binlog_dump;
       Action == binlog_dump_gtid ->
    {stop_and_reply,
     normal,
     {reply,
      From,
      {error, maps:without([action], Packet)}}};

handle_event(
  internal,
  {recv,
   #{packet :=
         #{header := #{event_type := format_description = EventType},
           event := Event,
           action := log_event}}},
  {Action, From},
  #{call_back := CallBack, requests := Requests} = Data)
  when Action == binlog_dump;
       Action == binlog_dump_gtid ->
    {keep_state,
     Data#{requests := gen_statem:send_request(
                         CallBack,
                         {EventType, Event},
                         ?MODULE, Requests)},
     {reply, From, ok}};

handle_event(
  internal,
  {recv,
   #{packet := #{header := #{event_type := table_map = EventType},
                 event := #{table_id := TableId} = Event,
                 action := log_event}}},
  _,
  #{binlog_dump := #{mapped := Mapped} = Binlog,
    client_flags := ClientFlags,
    call_back := CallBack,
    requests := Requests} = Data) ->
    Updated = Binlog#{mapped := Mapped#{TableId => maps:without([table_id], Event)}},
    {keep_state,
     Data#{decoder := msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_binlog_network_stream:decode(Updated),
                           msmp_packet_eof:decode(ClientFlags),
                           msmp_packet_error:decode(ClientFlags)])),
           requests := gen_statem:send_request(
                         CallBack,
                         {EventType, Event},
                         ?MODULE, Requests),
           binlog_dump := Updated}};

handle_event(
  internal,
  {recv, #{packet := #{header := #{event_type := gtid = EventType,
                                   server_id := ServerId},
                       event := Event,
                       action := log_event}}},
  _,
  #{call_back := CallBack, requests := Requests} = Data) ->
    {keep_state,
     Data#{requests := gen_statem:send_request(
                         CallBack,
                         {EventType, Event#{server_id => ServerId}},
                         ?MODULE, Requests)}};

handle_event(
  internal,
  {recv, #{packet := #{header := #{event_type := EventType},
                       event := Event,
                       action := log_event}}},
  _,
  #{call_back := CallBack, requests := Requests} = Data) ->
    {keep_state,
     Data#{requests := gen_statem:send_request(
                         CallBack,
                         {EventType, Event},
                         ?MODULE, Requests)}};

handle_event(internal, {response, #{label := ?MODULE, reply := ok}}, _, _) ->
    keep_state_and_data;

handle_event(internal,
             {response,
              #{label := ?MODULE, reply := {error, Reason}}},
             _,
             _) ->
    {stop, Reason};

handle_event(internal,
             {recv, #{packet := #{action := eof}}},
             _,
             _) ->
    stop;

handle_event(EventType, EventContent, State, Data) ->
    msc_mm_common:handle_event(EventType,
                               EventContent,
                               State,
                               Data).
