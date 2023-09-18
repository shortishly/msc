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

%% @doc Middleman dealing with the authentication process.

-module(msc_mm_auth).


-export([callback_mode/0]).
-export([handle_event/4]).
-import(msc_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("public_key/include/public_key.hrl").


callback_mode() ->
    handle_event_function.


handle_event({call, From}, {request, _}, {error, _} = Error, _) ->
    {stop_and_reply, normal, {reply, From, Error}};

handle_event({call, _}, {request, _}, _, _) ->
    {keep_state_and_data, postpone};

handle_event(internal,
             {recv,
              #{packet := #{action := handshake,
                            character_set := CharacterSet,
                            capability_flags_1 := LowerFlags,
                            capability_flags_2 := UpperFlags,
                            auth_plugin_name := ClientPluginName} = Handshake,
                sequence := Sequence} = Received},
             _,
             #{config := #{user := User,
                           database := _Database,
                           password := Password}} = Data) ->

    case client_flags(maps:merge(LowerFlags, UpperFlags)) of

        #{ssl := true} = ClientFlags ->
            {next_state,
             authenticating,
             Data#{encoder => msmp_codec:encode(
                                msmp_ssl_request:encode()),
                   decoder => msmp_codec:decode(
                                scran_branch:alt(
                                  [msmp_packet_ok:decode(ClientFlags),
                                   msmp_auth_switch_request:decode(),
                                   msmp_packet_error:decode(ClientFlags)])),
                   client_flags => ClientFlags,
                   character_set => CharacterSet,
                   handshake => Received},
             [nei({send,
                   #{packet => #{action => ssl_request,
                                 client_flags => ClientFlags,
                                 max_packet_size => msc_config:maximum(packet_size),
                                 character_set => CharacterSet},
                     sequence => Sequence + 1}}),
              nei(upgrade)]};

        #{ssl := false} = ClientFlags ->
            {next_state,
             authenticating,
             Data#{client_flags => ClientFlags,
                   character_set => CharacterSet,
                   handshake => Received,
                   encoder => msmp_codec:encode(
                                msmp_handshake_response:encode()),
                   decoder => msmp_codec:decode(
                                scran_branch:alt(
                                  [msmp_packet_ok:decode(ClientFlags),
                                   msmp_auth_switch_request:decode(),
                                   msmp_auth_more_data:decode(),
                                   msmp_packet_error:decode(ClientFlags)]))},
             nei({send,
                  #{packet => #{action => handshake_response,
                                client_flags => ClientFlags,
                                max_packet_size => msc_config:maximum(packet_size),
                                character_set => CharacterSet,
                                username => User,
                                auth_response => msmp_handshake_response:auth_response(
                                                   Handshake,
                                                   Password),
                                connect_attrs => connect_attrs(),
                                client_plugin_name => ClientPluginName},
                    sequence => Sequence + 1}})}
    end;

handle_event(internal,
             {response,
              #{label := {msc_mm_common, upgrade},
                reply := {error, Reason}}},
             _,
             _) ->
    {stop, Reason};

handle_event(
  internal,
  {response,
   #{label := {msc_mm_common, upgrade}, reply := ok}},
  _,
  #{handshake :=
        #{packet :=
              #{action := handshake,
                character_set := CharacterSet,
                auth_plugin_name := ClientPluginName} = Handshake,
          sequence := Sequence},
    character_set := CharacterSet,
    client_flags := ClientFlags,
    config := #{user := User,
                database := _Database,
                password := Password}} = Data) ->
    {keep_state,
     Data#{client_flags => ClientFlags,
           character_set => CharacterSet,
           encoder => msmp_codec:encode(
                        msmp_handshake_response:encode()),
           decoder => msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_packet_ok:decode(ClientFlags),
                           msmp_auth_switch_request:decode(),
                           msmp_auth_more_data:decode(),
                           msmp_packet_error:decode(ClientFlags)]))},
     nei({send,
          #{packet => #{action => handshake_response,
                        client_flags => ClientFlags,
                        max_packet_size => msc_config:maximum(packet_size),
                        character_set => CharacterSet,
                        username => User,
                        auth_response => msmp_handshake_response:auth_response(
                                           Handshake,
                                           Password),
                        connect_attrs => connect_attrs(),
                        client_plugin_name => ClientPluginName},
            sequence => Sequence + 2}})};

handle_event(
  internal,
  {recv,
   #{packet := #{status := perform_full_authentication,
                 action := auth_more_data},
     sequence := Sequence}},
  _,
  #{handshake := #{packet := #{auth_plugin_name := caching_sha2_password}},
    client_flags := #{ssl := true},
    config := #{password := Password}} = Data) ->
    {keep_state,
     Data#{encoder := msmp_codec:encode(
                        msmp_string_null_terminated:encode())},
     nei({send, #{sequence => Sequence + 1, packet => Password}})};

handle_event(
  internal,
  {recv,
   #{packet := #{status := perform_full_authentication,
                 action := auth_more_data},
     sequence := Sequence}},
  _,
  #{handshake := #{packet := #{auth_plugin_name := caching_sha2_password}},
    client_flags := #{ssl := false}} = Data) ->
    %% Clear text: request public key from server so that we can
    %% encrypt scrambled password.
    %%
    %% sql-common/client_authentication.cc
    %%
    {keep_state,
     Data#{encoder := msmp_codec:encode(
                        msmp_integer_fixed:encode(1))},
     nei({send, #{sequence => Sequence + 1, packet => 2}})};

handle_event(
  internal,
  {recv,
   #{packet := #{public_key := PublicKey,
                 action := auth_more_data},
     sequence := Sequence}},
  _,
  #{handshake := #{packet := #{auth_plugin_data_part_1 := PartOne,
                               auth_plugin_data_part_2 := <<PartTwo:12/bytes, 0>>,
                               auth_plugin_name := caching_sha2_password}},
    config := #{password := Password},
    client_flags := #{ssl := false}} = Data) ->
    %% Clear text: scramble password with original handshake, encrypt
    %% with public key.
    %%
    %% mysys/crypt_genhash_impl.cc
    %% sql-common/client_authentication.cc
    %%
    {keep_state,
     Data#{encoder := msmp_codec:encode(narcs_combinator:rest())},
     nei({send,
          #{sequence => Sequence + 1,
            packet => public_key:encrypt_public(
                        exor(<<Password/bytes, 0>>,
                             <<PartOne/bytes, PartTwo/bytes>>),
                        pem_decode(PublicKey),
                        [{rsa_padding, rsa_pkcs1_oaep_padding}])}})};

handle_event(
  internal,
  {recv,
   #{packet := #{status := fast_auth_success,
                 action := auth_more_data}}},
  _,
  #{handshake := #{packet := #{auth_plugin_name := caching_sha2_password}}}) ->
    keep_state_and_data;

handle_event(internal,
             {recv, #{packet := #{action := ok}}},
             authenticating,
             Data) ->
    {next_state,
     authenticated,
     maps:without([handshake], Data),
     pop_callback_module};

handle_event(internal,
             {recv, #{packet := #{action := error} = Packet}},
             authenticating,
             Data) ->
    {next_state, {error, maps:without([action], Packet)}, Data};

handle_event(
  internal,
  {recv,
   #{packet := #{action := auth_switch_request,
                 plugin_name := PluginName} = AuthSwitchRequest,
     sequence := Sequence}},
  authenticating,
  #{client_flags := ClientFlags,
    character_set := CharacterSet,
    config := #{user := User,
                database := _Database,
                password := Password}} = Data) ->
    {keep_state,
     Data#{decoder => msmp_codec:decode(
                        scran_branch:alt(
                          [msmp_packet_ok:decode(ClientFlags),
                           msmp_packet_error:decode(ClientFlags)]))},
     nei({send,
          #{packet => #{action => handshake_response,
                        client_flags => ClientFlags,
                        max_packet_size => msc_config:maximum(packet_size),
                        character_set => CharacterSet,
                        username => User,
                        auth_response => msmp_handshake_response:auth_response(
                                           AuthSwitchRequest,
                                           Password),
                        connect_attrs => connect_attrs(),
                        client_plugin_name => PluginName},
            sequence => Sequence + 1}})};

handle_event(EventType, EventContent, State, Data) ->
    msc_mm_common:handle_event(EventType,
                               EventContent,
                               State,
                               Data).


client_flags(HandshakeFlags) ->
    maps:map(
      fun
          (Name, Status) ->
              Status andalso msc_config:client_flag(Name)
      end,
      HandshakeFlags).


connect_attrs() ->
    #{}.


pem_decode([#'RSAPublicKey'{} = PublicKey]) ->
    PublicKey;

pem_decode([#'SubjectPublicKeyInfo'{} = KeyInfo]) ->
    public_key:pem_entry_decode(KeyInfo);

pem_decode(Encoded) ->
    ?FUNCTION_NAME(public_key:pem_decode(Encoded)).


exor(To, Pattern) when size(To) =< size(Pattern) ->
    crypto:exor(To, binary:part(Pattern, {0, size(To)})).
