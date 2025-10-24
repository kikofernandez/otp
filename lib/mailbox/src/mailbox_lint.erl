-module(mailbox_lint).

-moduledoc """
Erlang Mailbox linter.

Checks well-formedness of the mailbox definitions.
""".

-include("../include/mailbox.hrl").
-include_lib("kernel/include/logger.hrl").

-export([check/2]).

-define(IS_MAILBOX_DECL(Attr), (Attr == ?NEW_MB orelse Attr == ?USE_MB)).

-doc """
Check well-formedness of mailbox attributes using 'new' and 'use'.

The pass is a simple linter for syntactically ill mailbox definitions.

This pass rejects ill-typed mailbox, such as:

    ```
    -module(test).
    -new([]).  % error
    -new({test_mailbox, []}). % error
    -new({test_mailbox, [foo/0]}). % ok
    foo() -> ok.
    ```
""".
-spec check(Forms, Analysis) -> #analysis{} when
      Forms :: [erl_parse:abstract_form() | {'error', erl_scan:error_info() | erl_parse:error_info()} | {'eof', erl_anno:location()}],
      Analysis :: #analysis{}.
check(Forms, Analysis) when is_list(Forms) ->
    Info = erl_syntax_lib:analyze_forms(Forms),
    AnalysisWithForms = analysis:set_forms(Analysis, Info),
    check_attributes(Info, AnalysisWithForms).

-spec check_attributes(Forms, Analysis) -> Result when
      Forms    :: [erl_syntax_lib:info_pair()],
      Analysis :: #analysis{},
      Result   :: #analysis{}.
check_attributes(Forms, #analysis{}=Analysis) when is_list(Forms) ->
    case proplists:get_value(attributes, Forms) of
        Attributes when is_list(Attributes) ->
            ?LOG_DEBUG(#{?MODULE => ?LINE, message => "checking attributes"}),
            lists:foldl(fun check_attr/2, Analysis, Attributes);
        _ ->
            Analysis
    end.

-doc """
Check well-formedness of attributes `-use` and `-new`.

This pass rejects ill-formed attributes, such as the following, among others:

    ```
    -new([]).  % error
    -new({test_mailbox, []}). % error
    -new({test_mailbox, [foo/0]}). % ok
    ```
""".
-spec check_attr({Attr, Form}, Analysis) -> Result when
      Attr     :: atom(),
      Form     :: dynamic(),
      Analysis :: #analysis{},
      Result   :: #analysis{}.
check_attr({Attr, {MailboxName, []}}, Analysis) when ?IS_MAILBOX_DECL(Attr) ->
    Error = ?ERR_MISSING_MAILBOX_EMPTY_BOUND,
    analysis:push_error(Error, {MailboxName, []}, Analysis);
check_attr({Attr, {MailboxName, [_|_]=FnAttrs}}, Analysis) when ?IS_MAILBOX_DECL(Attr) ->
    FunctionDeclarations = proplists:get_value(functions, analysis:forms(Analysis), []),
    case FnAttrs -- FunctionDeclarations of
        [] ->
            %% all mailbox uses and new bound to existing functions
            Analysis;
        Errors ->
            Error = ?ERR_MISSING_BOUND_FUNCTION,
            analysis:push_error(Error, {MailboxName, Errors}, Analysis)
    end;
check_attr({Attr, []}=Form, Analysis) when ?IS_MAILBOX_DECL(Attr) ->
    Error = ?ERR_MISSING_MAILBOX_NAME,
    analysis:push_error(Error, Form, Analysis);
check_attr({Attr, _}, Analysis) when not ?IS_MAILBOX_DECL(Attr) ->
    Analysis.
