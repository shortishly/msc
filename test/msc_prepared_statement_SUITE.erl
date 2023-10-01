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


-module(msc_prepared_statement_SUITE).


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

tinyint_test(Config) ->
    MM = ?config(mm, Config),
    Table = alpha(5),

    Value = -6,

    {ok, _} = msc_mm_sync:query(
                #{server_ref => MM,
                  query => io_lib:format(
                             "create table ~s ("
                             "k serial, "
                             "i tinyint, "
                             "u tinyint unsigned)",
                             [Table])}),

    {ok, Insert} = msc_mm_sync:prepare(
                     #{server_ref => MM,
                       query => io_lib:format(
                                  "insert into ~s (i) values (?)",
                                  [Table])}),

    {ok, #{last_insert_id := Id}} = msc_mm_sync:execute(
                                     #{server_ref => MM,
                                       statement_id => Insert,
                                       parameters => [Value]}),

    ok = msc_mm_sync:stmt_close(
           #{server_ref => MM,
             statement_id => Insert}),

    {ok, Select} = msc_mm_sync:prepare(
                     #{server_ref => MM,
                       query => io_lib:format(
                                  "select i from ~s where k = ?",
                                  [Table])}),

    ?assertMatch(
       {_, [[Value]]},
       msc_mm_sync:execute(
         #{server_ref => MM,
           statement_id => Select,
           parameters => [Id]})),

    ?assertMatch(
       ok,
       msc_mm_sync:stmt_close(
         #{server_ref => MM,
           statement_id => Select})),

    {ok, _} = msc_mm_sync:query(
                #{server_ref => MM,
                  query => io_lib:format(
                             "drop table ~s",
                             [Table])}).


alpha(N) ->
    list_to_binary(pick(N, lists:seq($a, $z))).


pick(N, Pool) ->
    ?FUNCTION_NAME(N, Pool, []).


pick(0, _, A) ->
    A;

pick(N, Pool, A) ->
    ?FUNCTION_NAME(N - 1,
                   Pool,
                   [lists:nth(rand:uniform(length(Pool)), Pool) | A]).
