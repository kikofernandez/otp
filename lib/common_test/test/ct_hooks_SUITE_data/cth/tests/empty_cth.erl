%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

%%% Common Test Example Suite Callback module.
%%%
%%% This module gives an example of a common test CTH (Common Test Hook).
%%% There are many ways to add a CTH to a test run, you can do it either in
%%% the command line using -ct_hook, in a test spec using
%%% {ct_hook,M} or in the suite it self by returning ct_hook
%%% from either suite/0, init_per_suite/1, init_per_group/2 and
%%% init_per_testcase/2. The scope of the CTH is determined by where is it
%%% started. If it is started in the command line or test spec then it will
%%% be stopped at the end of all tests. If it is started in init_per_suite,
%%% it will be stopped after end_per_suite and so on. See terminate
%%% documentation for a table describing the scoping machanics. 
%%%
%%% All of callbacks except init/1 in a CTH are optional.

-module(empty_cth).

%% CT Hooks
-export([id/1]).
-export([init/2]).

-export([post_all/3]).
-export([post_groups/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/4]).
-export([post_init_per_group/5]).
-export([pre_end_per_group/4]).
-export([post_end_per_group/5]).

-export([pre_init_per_testcase/4]).
-export([post_init_per_testcase/5]).
-export([pre_end_per_testcase/4]).
-export([post_end_per_testcase/5]).

-export([on_tc_fail/4]).
-export([on_tc_skip/4]).

-export([terminate/1]).

-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

-type config() :: proplists:proplist().
-type reason() :: term().
-type skip_or_fail() :: {skip, reason()} |
                        {auto_skip, reason()} |
                        {fail, reason()} | 
			{'EXIT',reason()}.

-record(state, { id = ?MODULE :: term()}).

%% Called after groups/0.
%% You can change the return value in this function.
-spec post_groups(Suite :: atom(), Groups :: list()) -> list().
post_groups(Suite,Groups) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, post_groups,
				     [Suite,Groups]}}),
    ct:log("~w:post_groups(~w) called", [?MODULE,Suite]),
    Groups.

%% Called after all/0.
%% You can change the return value in this function.
-spec post_all(Suite :: atom(),
               Tests :: list(),
               Groups :: term()) ->
    list().
post_all(Suite,Tests,Groups) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, post_all,
				     [Suite,Tests,Groups]}}),
    ct:log("~w:post_all(~w) called", [?MODULE,Suite]),
    Tests.

%% Always called before any other callback function. Use this to initiate
%% any common state. It should return an state for this CTH.
-spec init(Id :: term(), Opts :: proplists:proplist()) ->
    {ok, State :: #state{}}.
init(Id, Opts) ->
    gen_event:notify(?CT_EVMGR_REF, #event{ name = cth, node = node(),
					    data = {?MODULE, init, [Id, Opts]}}),
    ct:log("~w:init called", [?MODULE]),
    {ok,Opts}.

%% The ID is used to uniquly identify an CTH instance, if two CTH's
%% return the same ID the seconds CTH is ignored. This function should NOT 
%% have any side effects as it might be called multiple times by common test.
-spec id(Opts :: proplists:proplist()) ->
    Id :: term().
id(Opts) ->
    gen_event:notify(?CT_EVMGR_REF, #event{ name = cth, node = node(),
					    data = {?MODULE, id, [Opts]}}),
    ct:log("~w:id called", [?MODULE]),
    ct_test_support:unique_timestamp().

%% Called before init_per_suite is called. Note that this callback is
%% only called if the CTH is added before init_per_suite is run (eg. in a test
%% specification, suite/0 function etc).
%% You can change the config in the this function.
-spec pre_init_per_suite(Suite :: atom(),
		     Config :: config(),
		     State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_suite(Suite,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, pre_init_per_suite,
				     [Suite,Config,State]}}),
    ct:log("~w:pre_init_per_suite(~w) called", [?MODULE,Suite]),
    {Config, State}.

%% Called after init_per_suite.
%% you can change the return value in this function.
-spec post_init_per_suite(Suite :: atom(),
			  Config :: config(),
			  Return :: config() | skip_or_fail(),
			  State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
post_init_per_suite(Suite,Config,Return,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, post_init_per_suite,
				     [Suite,Config,Return,State]}}),
    ct:log("~w:post_init_per_suite(~w) called", [?MODULE,Suite]),
    {Return, State}.

%% Called before end_per_suite. The config/state can be changed here,
%% though it will only affect the *end_per_suite function.
-spec pre_end_per_suite(Suite :: atom(),
		    Config :: config() | skip_or_fail(),
		    State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
pre_end_per_suite(Suite,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, pre_end_per_suite,
				     [Suite,Config,State]}}),
    ct:log("~w:pre_end_per_suite(~w) called", [?MODULE,Suite]),
    {Config, State}.

%% Called after end_per_suite. Note that the config cannot be
%% changed here, only the status of the suite.
-spec post_end_per_suite(Suite :: atom(),
			 Config :: config(),
			 Return :: term(),
			 State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_suite(Suite,Config,Return,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, post_end_per_suite,
				     [Suite,Config,Return,State]}}),
    ct:log("~w:post_end_per_suite(~w) called", [?MODULE,Suite]),
    {Return, State}.

%% Called before each init_per_group.
%% You can change the config in this function.
-spec pre_init_per_group(Suite :: atom(),
                         Group :: atom(),
                         Config :: config(),
                         State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_group(Suite,Group,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, pre_init_per_group,
				     [Suite,Group,Config,State]}}),
    ct:log("~w:pre_init_per_group(~w,~w) called", [?MODULE,Suite,Group]),
    {Config, State}.

%% Called after each init_per_group.
%% You can change the return value in this function.
-spec post_init_per_group(Suite :: atom(),
                          Group :: atom(),
			  Config :: config(),
			  Return :: config() | skip_or_fail(),
			  State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
post_init_per_group(Suite,Group,Config,Return,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, post_init_per_group,
				     [Suite,Group,Config,Return,State]}}),
    ct:log("~w:post_init_per_group(~w,~w) called", [?MODULE,Suite,Group]),
    {Return, State}.

%% Called after each end_per_group. The config/state can be changed here,
%% though it will only affect the *end_per_group functions.
-spec pre_end_per_group(Suite :: atom(),
                        Group :: atom(),
			Config :: config() | skip_or_fail(),
			State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
pre_end_per_group(Suite,Group,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, pre_end_per_group,
				     [Suite,Group,Config,State]}}),
    ct:log("~w:pre_end_per_group(~w~w) called", [?MODULE,Suite,Group]),
    {Config, State}.

%% Called after each end_per_group. Note that the config cannot be
%% changed here, only the status of the group.
-spec post_end_per_group(Suite :: atom(),
                         Group :: atom(),
			 Config :: config(),
			 Return :: term(),
			 State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_group(Suite,Group,Config,Return,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, post_end_per_group,
				     [Suite,Group,Config,Return,State]}}),
    ct:log("~w:post_end_per_group(~w,~w) called", [?MODULE,Suite,Group]),
    {Return, State}.

%% Called before init_per_testcase/2 for each test case.
%% You can change the config in this function.
-spec pre_init_per_testcase(Suite :: atom(),
                            TC :: atom(),
                            Config :: config(),
                            State :: #state{}) ->
    {config() | skip_or_fail(), NewState :: #state{}}.
pre_init_per_testcase(Suite,TC,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, pre_init_per_testcase,
				     [Suite,TC,Config,State]}}),
    ct:log("~w:pre_init_per_testcase(~w,~w) called", [?MODULE,Suite,TC]),
    {Config, State}.

%% Called after init_per_testcase/2, and before the test case.
-spec post_init_per_testcase(Suite :: atom(),
                             TC :: atom(),
			     Config :: config(),
			     Return :: config() | skip_or_fail(),
			     State :: #state{}) ->
				    {config() | skip_or_fail(), NewState :: #state{}}.
post_init_per_testcase(Suite,TC,Config,Return,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, post_init_per_testcase,
				     [Suite,TC,Config,Return,State]}}),
    ct:log("~w:post_init_per_testcase(~w,~w) called", [?MODULE,Suite,TC]),
    {Return, State}.

%% Called before end_per_testacse/2. No skip or fail allowed here,
%% only config additions.
-spec pre_end_per_testcase(Suite :: atom(),
                           TC :: atom(),
                           Config :: config(),
                           State :: #state{}) ->
    {config(), NewState :: #state{}}.
pre_end_per_testcase(Suite,TC,Config,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, pre_end_per_testcase,
				     [Suite,TC,Config,State]}}),
    ct:log("~w:pre_end_per_testcase(~w,~w) called", [?MODULE,Suite,TC]),
    {Config, State}.

%% Called after end_per_testcase/2 for each test case. Note that
%% the config cannot be changed here, only the status of the test case.
-spec post_end_per_testcase(Suite :: atom(),
                            TC :: atom(),
			    Config :: config(),
			    Return :: term(),
			    State :: #state{}) ->
    {ok | skip_or_fail(), NewState :: #state{}}.
post_end_per_testcase(Suite,TC,Config,Return,State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, post_end_per_testcase,
				     [Suite,TC,Config,Return,State]}}),
    ct:log("~w:post_end_per_testcase(~w,~w) called", [?MODULE,Suite,TC]),
    {Return, State}.

%% Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_tc if the suite, group or test case failed.
%% This function should be used for extra cleanup which might be needed.
%% It is not possible to modify the config or the status of the test run.
-spec on_tc_fail(Suite :: atom(),
                 TC :: init_per_suite | end_per_suite |
		       init_per_group | end_per_group | atom() |
		       {Function :: atom(), GroupName :: atom()},
		 Reason :: term(), State :: #state{}) -> NewState :: #state{}.
on_tc_fail(Suite, TC, Reason, State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, on_tc_fail,
				     [Suite,TC,Reason,State]}}),
    ct:log("~w:on_tc_fail(~w,~w) called", [?MODULE,Suite,TC]),
    State.

%% Called when a test case is skipped by either user action
%% or due to an init function failing. Test case can be
%% end_per_suite, init_per_group, end_per_group and the actual test cases. 
-spec on_tc_skip(Suite :: atom(),
                 TC :: end_per_suite |
		       init_per_group | end_per_group | atom() |
		       {Function :: atom(), GroupName :: atom()},
		 {tc_auto_skip, {failed, {Mod :: atom(), Function :: atom(), Reason :: term()}}} |
		 {tc_user_skip, {skipped, Reason :: term()}},
		 State :: #state{}) -> NewState :: #state{}.
on_tc_skip(Suite, TC, Reason, State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, on_tc_skip,
				     [Suite,TC,Reason,State]}}),
    ct:log("~w:on_tc_skip(~w,~w) called", [?MODULE,Suite,TC]),
    State.

%% Called when the scope of the CTH is done, this depends on
%% when the CTH was specified. This translation table describes when this
%% function is called.
%%
%%  | Started in          |     terminate called    |
%%  |---------------------|-------------------------|
%%  | command_line        | after all tests are run |
%%  | test spec           | after all tests are run |
%%  | suite/0             | after SUITE is done     |
%%  | init_per_suite/1    | after SUITE is done     |
%%  | init_per_group/2    | after group is done     |
%%  |-----------------------------------------------|
%%
-spec terminate(State :: #state{}) ->
    term().
terminate(State) ->
    gen_event:notify(
      ?CT_EVMGR_REF, #event{ name = cth, node = node(),
			     data = {?MODULE, terminate, [State]}}),
    ct:log("~w:terminate called", [?MODULE]),
    ok.
