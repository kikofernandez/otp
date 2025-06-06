%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2016-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%% Supervisor for config processes.
%%

-module(diameter_config_sup).
-moduledoc false.

-behaviour(supervisor).

%% interface
-export([start_link/0,        %% supervisor start
	 start_child/1]).     %% config start

-export([init/1]).

-define(NAME, ?MODULE).  %% supervisor name

%% start_link/0

start_link() ->
    SupName = {local, ?NAME},
    supervisor:start_link(SupName, ?MODULE, []).

%% start_child/1

start_child(T) ->
    supervisor:start_child(?NAME, [T]).

%% init/1

init([]) ->
    Mod = diameter_config,
    Flags = {simple_one_for_one, 0, 1},
    ChildSpec = {Mod,
                 {Mod, start_link, []},
                 temporary,
                 1000,
                 worker,
                 [Mod]},
    {ok, {Flags, [ChildSpec]}}.
