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

%% @doc Supervise connections.

-module(msc_connections_sup).


-behaviour(supervisor).
-export([init/1]).
-export([start_child/1]).
-export([start_link/0]).
-import(msc_sup, [supervisor/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(URI) ->
    supervisor:start_child(?MODULE, [msc_uri:parse(URI)]).


init([]) ->
    {ok, {#{strategy => simple_one_for_one}, children()}}.


children() ->
    [supervisor(#{m => msc_connection_sup, restart => temporary})].
