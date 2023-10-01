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


-module(msc_simple_query_SUITE).


-compile(export_all).
-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    common:all(?MODULE).


init_per_suite(Config) ->
    {ok, _} = msc:start(),
    URI = <<"mysql://root:secret@localhost:3306/test">>,
    {ok, Supervisor} = msc_connections_sup:start_child(URI),
    MM = msc_sup:get_child_pid(Supervisor, mm),
    [{mm, MM} | Config].


end_per_suite(_Config) ->
    ok.

two_plus_two_test(Config) ->
    MM = ?config(mm, Config),
    ?assertMatch(
       {[#{name := <<"2 + 2">>,
           table := <<>>,
           schema := <<>>,
           catalog := <<"def">>}],
         [[4]]},
       msc_mm_sync:query(
         #{server_ref => MM,
           query => <<"select 2 + 2">>})).
