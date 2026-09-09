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

%%
%% End-to-end tests for mailbox:check/2, driven by fixture modules in
%% mailbox_check_SUITE_data/. Each fixture is a standalone .erl file
%% exercising ONE checker concern:
%%
%%   accept_*  the file must type-check          -> check/2 returns ok
%%   reject_*  the file must fail type-checking   -> check/2 returns {error, _}
%%
%% To add a case, drop a new accept_*.erl or reject_*.erl into the data
%% dir and add its name to accept_cases/0 or reject_cases/0 below.
%%
%% NOTE: check/2 compiles the fixture with the *running system's*
%% compiler. Fixtures here are plain Erlang (no `-mailbox`) so they
%% compile under any OTP. Fixtures using `-mailbox` would require the
%% patched parser and are intentionally not included in this suite.
%%
-module(mailbox_check_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([accept/1, reject/1]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [{group, accept}, {group, reject}].

groups() ->
    [{accept, [parallel], [accept]},
     {reject, [parallel], [reject]}].

init_per_suite(Config) ->
    %% Ensure the mailbox modules are available.
    case code:ensure_loaded(mailbox) of
        {module, mailbox} -> Config;
        _ -> {skip, "mailbox application not available"}
    end.

end_per_suite(_Config) ->
    ok.

%%% ===================================================================
%%% Fixture inventory
%%% ===================================================================

accept_cases() ->
    ["accept_literal_atom",
     "accept_literal_integer",
     "accept_numeric_tower",
     "accept_union_member",
     "accept_tuple",
     "accept_list",
     "accept_case",
     "accept_apply_local",
     "accept_var_passthrough",
     "accept_unspec_dynamic",
     "accept_union_guard_dispatch",
     "accept_union_guard_used",
     "accept_union_narrow_return",
     "accept_list_guard_used",
     "accept_record_guard",
     "accept_map_guard",
     "accept_atom_guard_narrow",
     "accept_binary_guard_narrow",
     "accept_tuple_union_guard",
     "accept_unused_narrowed_arg"].

reject_cases() ->
    ["reject_wrong_atom",
     "reject_atom_vs_integer",
     "reject_not_in_union",
     "reject_tuple_element",
     "reject_case_branch",
     "reject_apply_return",
     "reject_list_element",
     "reject_guard_contradicts_spec",
     "reject_guard_outside_union"].

%%% ===================================================================
%%% Test bodies
%%% ===================================================================

accept(Config) ->
    lists:foreach(
      fun(Name) ->
              File = fixture(Config, Name),
              case mailbox:check(File, []) of
                  ok ->
                      ok;
                  Other ->
                      ct:fail({expected_accept, Name, got, Other})
              end
      end, accept_cases()).

reject(Config) ->
    lists:foreach(
      fun(Name) ->
              File = fixture(Config, Name),
              case mailbox:check(File, []) of
                  {error, _} ->
                      ok;
                  Other ->
                      ct:fail({expected_reject, Name, got, Other})
              end
      end, reject_cases()).

%%% ===================================================================
%%% Helpers
%%% ===================================================================

fixture(Config, Name) ->
    DataDir = ?config(data_dir, Config),
    File = filename:join(DataDir, Name ++ ".erl"),
    true = filelib:is_regular(File),
    File.
