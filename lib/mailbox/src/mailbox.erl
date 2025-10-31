%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

-moduledoc """
Erlang Mailbox type checker.
""".

-include("../include/mailbox.hrl").
-include_lib("kernel/include/logger.hrl").

-export([check/2]).

-doc """
Perform mailbox type checking on the given `File`.

`Options` control which phases of the mailbox checker
print debugging information.
""".
-spec check(File, Options) -> Success | Error when
      Options :: checker_options(),
      File    :: file:filename(),
      Success :: [],
      Error   :: errors().
check(File, Options) ->    
    Analysis = init(File, Options),    
            
    ?LOG_DEBUG(#{stage => preprocessor,
                 msg   => "Running preprocessor",
                 file  => File}),
    {ok, Forms} = epp:parse_file(File, []),

    ?LOG_DEBUG(#{stage => linter,
                 msg => "Linting module for well-formedness"}),
    Analysis1 = mailbox_lint:check(Forms, Analysis),
    analysis:errors(Analysis1).

-spec init(File :: file:filename(), #option{}) -> #analysis{}.
init(File, #option{}=Options) ->
    _ = mailbox_options:set_config(Options),
    Analysis = analysis:set_file(#analysis{}, File),
    analysis:set_mode(Analysis, mailbox_options:mode(Options)).
