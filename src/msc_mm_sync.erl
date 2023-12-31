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


%% @doc Synchronous middleman.

-module(msc_mm_sync).


-export([binlog_dump/1]).
-export([execute/1]).
-export([operator/1]).
-export([prepare/1]).
-export([query/1]).
-export([register_replica/1]).
-export([stmt_close/1]).
-export([stmt_reset/1]).


binlog_dump(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


execute(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


operator(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


prepare(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


query(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


register_replica(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


stmt_close(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


stmt_reset(Arg) ->
    receive_response(?FUNCTION_NAME, Arg).


receive_response(Function, Arg) ->
    case gen_statem:receive_response(msc_mm:Function(Arg)) of
        {reply, Reply} ->
            Reply;

        {error, _} = Error ->
            Error
    end.
