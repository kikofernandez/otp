%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(mailbox).

-include("c_types.hrl").

-export([expect/1, check/2]).

-doc """
Indicate to the mailbox type system that the receive should expect message `Msg`.

This annotation is needed to guide the mailbox type checker on receives.
""".
-spec expect(Msg :: string()) -> ok.
expect(Msg) when is_list(Msg) -> ok.


-doc """
Perform mailbox typing in the module.
""".
-spec check(File, Opts :: []) -> ok | {error, Error :: atom() | list()} when
      File :: file:filename().
check(File, Opts) ->
    case compile:file(File, [to_core, binary]) of
        error ->
            io:format("Error. Check if the file ~s exists.", [File]),
            {error, file_not_found};
        {ok, _Mod, Core} when not is_list(Core), not is_binary(Core) ->
            Env = mailbox_env:from_core(Core, Opts),
            Defs = mailbox_env:fetch_defs(Core),
            mailbox_check:check_module(Env, Defs, Opts);
        _ ->
            {error, unexpected_result}
    end.
