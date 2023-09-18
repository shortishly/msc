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

%% @doc URI handling for the mysql scheme.

-module(msc_uri).


-export([parse/1]).


parse(URI) when is_list(URI) ->
    ?FUNCTION_NAME(list_to_binary(URI));

parse(URI) when is_binary(URI) ->
    maps:fold(
      fun
          (userinfo, UserInfo, A) ->
              case string:split(UserInfo, ":") of
                  [User, Password] ->
                      A#{user => User,
                         password => Password};

                  [User] ->
                      A#{user => User}
              end;

          (scheme, <<"mysql">>, A) ->
                 A;

          (_, <<>>, A) ->
              A;

          (path, <<"/", Name/bytes>>, A) ->
              A#{database => Name};

          (K, V, A) ->
              A#{K => V}
      end,
      #{},
      uri_string:parse(URI)).
