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


-module(msc).


-export([diff/0]).
-export([diff/2]).
-export([priv_dir/0]).
-export([start/0]).


start() ->
    application:ensure_all_started(?MODULE).


priv_dir() ->
    code:priv_dir(?MODULE).

diff() ->
    ?FUNCTION_NAME(
       #{interactive => false,
         ssl => false,
         compress => false,mfa => false,
         connect_attrs => true,deprecate_eof => true,
         local_files => true,long_flag => true,
         long_password => false,plugin_auth => true,
         plugin_auth_lenenc_client_data => true,
         protocol_41 => true,query_attributes => false,
         remember_options => true,session_track => true,
         transactions => true,
         capability_extension => true,reserved => false,
         zstd_compression => false,
         connect_with_db => false,
         ssl_verify_cert => false,
         optional_resultset_metadata => false,
         can_handle_expired_passwords => false,
         ps_multi_results => false,multi_results => false,
         multi_statements => false,reserved2 => false,
         ignore_sigpipe => false,ignore_space => false,
         odbc => false,no_schema => false,
         found_rows => false},
#{can_handle_expired_passwords => true,
                                 capability_extension => true,
                                 compress => false,
                                 connect_attrs => true,
                                 connect_with_db => false,
                                 deprecate_eof => false,
                                 found_rows => false,
                                 ignore_sigpipe => false,
                                 ignore_space => false,
                                 interactive => true,
                                 local_files => true,
                                 long_flag => true,
                                 long_password => false,
                                 mfa => false,
                                 multi_results => true,
                                 multi_statements => true,
                                 no_schema => false,
                                 odbc => false,
                                 optional_resultset_metadata => false,
                                 plugin_auth => true,
                                 plugin_auth_lenenc_client_data => true,
                                 protocol_41 => true,
                                 ps_multi_results => true,
                                 query_attributes => false,
                                 remember_options => false,
                                 reserved => false,
                                 reserved2 => true,
                                 session_track => true,
                                 ssl => false,
                                 ssl_verify_cert => false,
                                 transactions => true,
                                 zstd_compression => false}).


diff({K, V, I}, B) ->
    case B of
        #{K := V} ->
            ?FUNCTION_NAME(maps:next(I), B);

        #{K := V2} ->
            [{K, V, V2} | ?FUNCTION_NAME(maps:next(I), B)]
    end;

diff(none, _) ->
    [];

diff(A, B) ->
    ?FUNCTION_NAME(maps:next(maps:iterator(A)), B).
