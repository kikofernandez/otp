%%
%% %CopyrightBegin%
%%
%% Copyright the University of Glasgow 2025. All Rights Reserved.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Macro definitions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% e.g., -new([]).
-define(ERR_MISSING_MAILBOX_NAME, err_missing_name).

%% e.g., -new({mb, [pong/2]}). when pong/2 is not defined.
-define(ERR_MISSING_BOUND_FUNCTION, err_missing_mailbox_bound_fun).

%% e.g., -new({foo_mailbox, []}).
-define(ERR_MISSING_MAILBOX_EMPTY_BOUND, err_mailbox_empty_bound).

%% e.g., recv pattern matches on unsupported pattern
-define(ERR_RECV_PATTERN_UNSUPPORTED, err_unsupported_recv_pattern).

-define(ERR_DESCRIPTOR_MARKERS, err_descriptors_markers).

-define(ERR_UNSUPPORTED_CATCH_CLAUSES, err_unsupported_catch_clauses).

%% defines modes:
%% 'debug' will print debugging information.
%% 'check' ignores debugging info
%%
-define(CHECK_MODE, check).
-define(DEBUG_MODE, debug).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% New Erlang Attributes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% We introduce two new Erlang attributes, `-new` and `-use`.
%% Both of them are used as follows:
%%
%% ```
%% -new({ping_mailbox, [ping/1]}).
%% ```
%%
%% This indicates that the function `ping/1` must re-interpret its type information
%% to consider the `ping_interface/0` type as a mailbox interface. Without this
%% information (and binding) there is no way to tell which interface the process
%% should conform to.
%%
%% A mailbox interface simply states the kind of messages that this process expects
%% to receive. Since Erlang does not have mailbox interfaces in their language of
%% types, we re-use the type specs as interfaces when we see these bindings (`-new`
%% and `-use`). As part of the interface, there must be a `pid()`.
%%
%% The reason for requiring `pid()` as part of the interface is that, in the
%% original paper "Mailbox Types" by Simon Fowler et al 2023, mailboxes are first
%% class citizens. Erlang does not have mailbox as values, but `pid()` can be
%% considered not only a reference to a process, but also the implicit mailbox of
%% the process. This consideration means that any type or spec that was initially
%% written simply as `pid()` should now be improved with the kind of messages that
%% it expects. For example, if we had
%%
%% ```erlang
%% -type ping_interface() :: pid().
%% ```
%%
%% and the function that uses this type expects to receive messages `start` and
%% `pong`, then those should be re-written as follows:
%%
%% ```erlang
%% -type ping_interface() :: pid() | start() | pong().
%% -type start() :: {start}.
%% -type pong() :: {pong}.
%% ```
%%
%% The semantic meaning is that there is a mailbox interface `ping_interface()`
%% that expects messages `start()` and `pong()`. If in the body of the function we
%% try to pattern match on anything else, the program is ill-typed (an error is
%% statically detected).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Attribute that indicates the creation of a mailbox
-define(NEW_MB, new).

%% Attribute that indicates the usage of a mailbox
-define(USE_MB, use).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Record definitions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(analysis, { % represents current file under analysis
                    file     = "undefined" :: file:filename(),
                    warnings = []          :: [],
                    errors   = []          :: [{error(), dynamic()}],

                    % contains erl_syntax_lib:analyze_forms/1
                    forms    = []          :: list(),

                    % mode of operation
                    mode     = check       :: mode()
                  }).

-record(option, {mode = ?CHECK_MODE :: mode(),
                 phase = [all] :: [checker_phase()]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Types for records
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type analysis() :: #analysis{}.



-doc "Options of the mailbox checker".
-type checker_options() :: #option{}.

-doc "Enable debug prinouts".
-type checker_option() :: mode_option()         %
                        | checker_phases().     %
-type mode_option() :: {mode, mode()}.

-doc "States which phases to perform. mostly useful for debugging".
-type checker_phases() :: {phase, [checker_phase()]}.
-type checker_phase() :: all.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Type Definitions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type errors() :: [error()].
-type error()  :: ?ERR_MISSING_MAILBOX_NAME |
                  ?ERR_MISSING_MAILBOX_EMPTY_BOUND |
                  ?ERR_MISSING_BOUND_FUNCTION |
                  ?ERR_DESCRIPTOR_MARKERS |
                  ?ERR_RECV_PATTERN_UNSUPPORTED |
                  ?ERR_UNSUPPORTED_CATCH_CLAUSES.
-type mode()   :: ?CHECK_MODE | ?DEBUG_MODE.


