%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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

%% To able to generate nice crash reports we need a catch on the highest level.
%% This code can't be purged so a code change is not possible.
%% And hence this a simple module.

-module(mnesia_sp).
-moduledoc false.

-export([init_proc/4]).

init_proc(Who, Mod, Fun, Args) ->
    mnesia_lib:verbose("~p starting: ~p~n", [Who, self()]),
    try
        apply(Mod, Fun, Args)
    catch
        exit:Reason when Reason =:= shutdown; Reason =:= kill; Reason =:= normal ->
            mnesia_monitor:terminate_proc(Who, Reason, Args),
	    exit(Reason);
        _:Reason:ST ->
	    mnesia_monitor:terminate_proc(Who, {Reason,ST}, Args),
	    exit(Reason)
    end.




