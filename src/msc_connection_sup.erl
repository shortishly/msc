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

%% @doc Supervise a socket with their middleman.

-module(msc_connection_sup).


-behaviour(supervisor).
-export([init/1]).
-export([start_link/1]).
-import(msc_sup, [worker/1]).


start_link(Arg) ->
    supervisor:start_link(?MODULE, [Arg]).


init([Arg]) ->
    {ok,
     {#{auto_shutdown => any_significant},
      children(Arg)}}.


children(Arg) ->
    [worker(
       #{m => M,
         restart => temporary,
         signficant => true,
         args => [Arg]}) || M <- workers()].


workers() ->
    [msc_socket, msc_mm].
