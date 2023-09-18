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


-module(msc_tests).


-export([t/1]).
-export([t/2]).
-include_lib("eunit/include/eunit.hrl").


t(FUT, TestFilename) ->
    {ok, Tests} = file:consult(TestFilename),
    lists:map(
      t(FUT),
      Tests).


t(F) ->
    fun
        ({{exception, Class, Term}, Input} = Test) ->
            {nm(Test), ?_assertException(Class, Term, apply(F, Input))};

        ({match, Expected, Input} = Test) ->
            {nm(Test), ?_assertMatch(Expected, F(Input))};

        ({Expected, Input} = Test) ->
            {nm(Test),
             ?_assertEqual(
                Expected,
                case F(Input) of
                    {<<>>, Expected} ->
                        Expected;

                    Expected ->
                        Expected;

                    Otherwise ->
                        ?debugVal(scran_debug:pp(F), -1),
                        ?debugVal(Otherwise, -1),
                        ?debugVal(Expected, -1),
                        ?debugVal(Input, -1),
                        Otherwise
                end)}
      end.


nm(Test) ->
    iolist_to_binary(io_lib:fwrite("~p", [Test])).
