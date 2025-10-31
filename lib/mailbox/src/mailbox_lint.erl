-module(mailbox_lint).

-moduledoc """
Erlang Mailbox linter.

Checks well-formedness of the mailbox definitions.
""".

-include("../include/mailbox.hrl").
-include_lib("kernel/include/logger.hrl").

-export([check/2]).

%% check if the attribute is a mailbox declaration
-define(IS_MAILBOX_DECL(Attr), (Attr == ?NEW_MB orelse Attr == ?USE_MB)).

%% checks if the AST form is a literal. see Abstract Format in erlang.org
-define(IS_LITERAL(Lit), ((element(1, Lit) == char) orelse
                          (element(1, Lit) == float) orelse
                          (element(1, Lit) == integer) orelse
                          (element(1, Lit) == atom) orelse
                          (element(1, Lit) == string))).

%% check if AST node is a function
-define(IS_FUN(Form), (element(1, Form) == function)).

-define(IS_ERROR_MARKER(Form), ((element(1, Form) == 'error') orelse (element(1, Form) == 'eof'))).

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

It also checks valid receive pattern clauses.
""".
-spec check(Forms, Analysis) -> #analysis{} when
      Forms :: [erl_parse:abstract_form() | {'error', erl_scan:error_info() | erl_parse:error_info()} | {'eof', erl_anno:location()}],
      Analysis :: #analysis{}.
check(Forms, Analysis0) when is_list(Forms) ->
    Info = erl_syntax_lib:analyze_forms(Forms),
    Analysis1 = analysis:set_forms(Analysis0, Info),

    AnalysedErrors = check_errors(Info, Analysis1),
    AnalysedAttrs = check_attributes(Info, AnalysedErrors),
    Analysis = lists:foldl(fun check_form/2, AnalysedAttrs, Forms),
    analysis:set_errors(lists:reverse(analysis:errors(Analysis)), Analysis).

-doc "Check Erlang forms for well-formedness and accepted mailbox forms".
-spec check_form(Form, Analysis) -> #analysis{} when
      Form        :: erl_parse:abstract_form() | ErrorMarker,
      ErrorMarker :: {'error', erl_scan:error_info() | erl_parse:error_info()} | {'eof', erl_anno:location()},
      Analysis    :: #analysis{}.
check_form(Form, Analysis) when ?IS_FUN(Form) ->
    ?LOG_DEBUG(#{?MODULE => ?LINE, message => "checking functions"}),
    check_fun(Form, Analysis);
check_form(Form, Analysis) when ?IS_ERROR_MARKER(Form) ->
    Analysis;
check_form(_Form, Analysis) ->
    %% TODO: process other forms.
    Analysis.

-doc """
Adds error_markers or eof markers to the list of errors.
""".
-spec check_errors(Forms, Analysis) -> Result when
      Forms    :: [erl_syntax_lib:info_pair()],
      Analysis :: #analysis{},
      Result   :: #analysis{}.
check_errors(Forms, #analysis{}=Analysis) when is_list(Forms) ->
    case proplists:get_value(errors, Forms) of
        Attributes when is_list(Attributes) ->
            ?LOG_DEBUG(#{?MODULE => ?LINE, message => "checking errors"}),
            lists:foldl(fun check_error/2, Analysis, Attributes);
        _ ->
            Analysis
    end.

-doc "Check error descriptors markers".
-spec check_error({'error' | 'eof', dynamic()}, #analysis{}) -> #analysis{}.
check_error({'error', _}=E, Analysis) ->
    Error = ?ERR_DESCRIPTOR_MARKERS,
    analysis:push_error(Error, E, Analysis);
check_error({'eof', _}=E, Analysis) ->
    Error = ?ERR_DESCRIPTOR_MARKERS,
    analysis:push_error(Error, E, Analysis).

-doc """
Check attributes well-formedness.
""".
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

-doc "Check function well-formedness".
-spec check_fun({function,_,_,_,Clauses :: list()}, #analysis{}) -> #analysis{}.
check_fun({function,_Anno,_Name,_Arity,Clauses}, Analysis) when is_list(Clauses) ->
    check_clauses(Clauses, Analysis).

-doc "Check function clauses well-formedness".
-spec check_clauses(Clauses :: [tuple()], #analysis{}) -> #analysis{}.
check_clauses(Clauses, Analysis) when is_list(Clauses) ->
    lists:foldl(fun check_clause/2, Analysis, Clauses).

-doc "Check function clause well-formedness".
-spec check_clause({clause, _, PatternSeqArgs, Guards, Body}, #analysis{}) -> #analysis{} when
      PatternSeqArgs :: list(),
      Guards         :: list(),
      Body           :: list().
check_clause({clause, _Anno, PatternSeq, Guards, Body}, Analysis) when is_list(PatternSeq) ->
    Analysis1 = check_patterns(PatternSeq, Analysis),
    Analysis2 = check_guards(Guards, Analysis1),
    check_exprs(Body, Analysis2).

-doc "Check expressions well-formedness".
-spec check_exprs([tuple()], #analysis{}) -> #analysis{}.
check_exprs(Forms, Analysis) when is_list(Forms) ->
    lists:foldl(fun check_expr/2, Analysis, Forms).

-doc "Check well-formedness of expressions".
-spec check_expr(Expr :: tuple() | list(), #analysis{}) -> #analysis{}.
check_expr(Exprs, Analysis) when is_list(Exprs) ->
    check_exprs(Exprs, Analysis);
check_expr({'receive', _Anno, CaseClauses}, Analysis) when is_list(CaseClauses) ->
    check_recv_clauses(CaseClauses, Analysis);
check_expr({'receive', _Anno, CaseClauses, AfterExpr, AfterBody}, Analysis) when is_list(CaseClauses),
                                                                                 is_list(AfterBody) ->
    Analysis1 = check_recv_clauses(CaseClauses, Analysis),
    check_exprs([AfterExpr | AfterBody], Analysis1);
check_expr({var,_Anno,_VarName}, Analysis) ->
    Analysis;
check_expr({'block', _Anno, Body}, Analysis) when is_list(Body) ->
    check_exprs(Body, Analysis);
check_expr({'case', _Anno, Expr, CaseClauses}, Analysis) when is_list(CaseClauses) ->
    Analysis1 = check_expr(Expr, Analysis),
    check_case_clauses(CaseClauses, Analysis1);
check_expr({'catch',_Anno,Expr}, Analysis) ->
    check_expr(Expr, Analysis);
check_expr({cons,_Anno,HeadList,List}, Analysis) when is_list(List) ->
    check_exprs([HeadList | List], Analysis);
check_expr({'fun',_Anno,{clauses, FunClauses}}, Analysis) when is_list(FunClauses) ->
    check_clauses(FunClauses, Analysis);
check_expr({named_fun,_Anno,_Name,FunClauses}, Analysis) when is_list(FunClauses) ->
    check_clauses(FunClauses, Analysis);
check_expr({call, _Anno, {atom, _, Name}, Args}, Analysis) when is_list(Args),
                                                                (Name == spawn orelse Name == self) ->
    check_exprs(Args, Analysis);
check_expr({call, _Anno, _Name, Args}, Analysis) when is_list(Args) ->
    check_exprs(Args, Analysis);

%% check_expr({call, _Anno, {atom, _, spawn},
%%             [{atom, _, M}, {atom, _, F}, Args]}, Analysis) when is_list(Args),
%%                                                                 is_atom(M),
%%                                                                 is_atom(F) ->
%%     check_exprs(Args, Analysis);
check_expr({call,_Anno,{remote,_AnnoRemote,ModuleExpr,FunExpr,Args}}, Analysis) when is_list(Args) ->
    check_exprs([ModuleExpr, FunExpr | Args], Analysis);
check_expr({'if',_Anno,IfClauses}, Analysis) when is_list(IfClauses) ->
    check_if_clauses(IfClauses, Analysis);
check_expr({lc,_Anno,Body,Qualifiers}, Analysis) when is_list(Qualifiers) ->
    Analysis1 = check_qualifier(Qualifiers, Analysis),
    check_expr(Body, Analysis1);
check_expr({mc,_Anno,Body,Qualifiers}, Analysis) when is_list(Qualifiers) ->
    Analysis1 = check_qualifier(Qualifiers, Analysis),
    check_expr(Body, Analysis1);
check_expr({map,_Anno,Associations}, Analysis) when is_list(Associations) ->
    check_associations(Associations, Analysis);
check_expr({map,_Anno,MapExpr,Associations}, Analysis) when is_list(Associations) ->
    check_associations(Associations, check_expr(MapExpr, Analysis));
check_expr({match,_Anno,Pattern,Expr}, Analysis) ->
    Analysis1 = check_pattern(Pattern, Analysis),
    check_expr(Expr, Analysis1);
check_expr({maybe_match,_Anno,Pattern,Expr}, Analysis) ->
    check_expr(Expr, check_expr(Pattern, Analysis));
check_expr({'maybe',_Anno,Body}, Analysis) ->
    check_exprs(Body, Analysis);
check_expr({'maybe',_Anno,Body,{'else',_AnnoElse,Clauses}}, Analysis) when is_list(Clauses) ->
    Analysis1 = check_exprs(Body, Analysis),
    check_if_clauses(Clauses, Analysis1);
check_expr({op,_Anno,_Op,Expr1, Expr2}, Analysis) ->
    check_expr(Expr2, check_expr(Expr1, Analysis));
check_expr({op, _Anno, _Op, Expr}, Analysis) ->
    check_expr(Expr, Analysis);
check_expr({record,_Anno,_Name, RecordFields}, Analysis) when is_list(RecordFields) ->
    check_record_fields(RecordFields, Analysis);
check_expr({record_field,_Anno,Expr,_Name,Field}, Analysis) ->
    Analysis1 = check_expr(Expr, Analysis),
    check_expr(Field, Analysis1);
check_expr({record_index,_Anno,_Name,Field}, Analysis) ->
    check_expr(Field, Analysis);
check_expr({record,_Anno,Expr,_Name,RecordFields}, Analysis) when is_list(RecordFields) ->
    check_record_fields(RecordFields, check_expr(Expr, Analysis));
check_expr({tuple,_Anno,Exprs}, Analysis) when is_list(Exprs) ->
    check_exprs(Exprs, Analysis);
check_expr({'try',_Anno,Body,CaseClauses,CatchClauses,AfterBody}, Analysis) when is_list(CaseClauses),
                                                                                 is_list(CatchClauses),
                                                                                 is_list(AfterBody) ->
    Analysis1 = check_exprs(Body, Analysis),
    Analysis2 = check_case_clauses(CaseClauses, Analysis1),
    Analysis3 = check_catch_clauses(CatchClauses, Analysis2),
    check_exprs(AfterBody, Analysis3);
check_expr(Lit, Analysis)  when ?IS_LITERAL(Lit) ->
    check_lit(Lit, Analysis);
check_expr(_, Analysis) ->
    Analysis.

-doc "Check well-formedness of catch clauses".
-spec check_catch_clauses(Clauses :: [tuple()], Analysis :: #analysis{}) -> #analysis{}.
check_catch_clauses(Clauses, Analysis) when is_list(Clauses) ->
    analysis:push_error(?ERR_UNSUPPORTED_CATCH_CLAUSES, {error, Clauses}, Analysis).

-doc "Check well-formedness of record fields".
-spec check_record_fields(RecordFields :: [tuple()], Analysis :: #analysis{}) -> #analysis{}.
check_record_fields(RecordFields, Analysis) when is_list(RecordFields) ->
    lists:foldl(fun check_record_field/2, Analysis, RecordFields).


-doc "Check well-formedness of record fields".
-spec check_record_field(RecordField, Analysis) -> #analysis{} when
      RecordField :: {record_field,_,Field :: atom(), Expr :: tuple()},
      Analysis :: #analysis{}.
check_record_field({record_field,_Anno,Field,Expr}, Analysis) when is_atom(Field) ->
    check_expr(Expr, Analysis).

-doc "Check well-formedness of pattern sequences".
-spec check_patterns(Patterns :: list(), #analysis{}) -> #analysis{}.
check_patterns(Patterns, Analysis) when is_list(Patterns) ->
    lists:foldl(fun check_pattern/2, Analysis, Patterns).

%% TODO
check_guards(_, Analysis) ->
    Analysis.

%% TODO
check_if_clauses(Clauses, Analysis) when is_list(Clauses) ->
    Analysis.

%% TODO
check_qualifier(Qualifiers, Analysis) when is_list(Qualifiers) ->
    Analysis.

%% TODO
check_associations(Associations, Analysis) when is_list(Associations) ->
    Analysis.

%% TODO
check_pattern(_Pattern, Analysis) ->
    Analysis.

%% TODO: check.
check_case_pattern(_Pattern, Analysis) ->
    Analysis.

check_case_clauses(Clauses, Analysis) when is_list(Clauses) ->
    lists:foldl(fun check_case_clause/2, Analysis, Clauses).

check_case_clause({clause, _, [Pattern], _Guards, Body}, Analysis) ->
    Analysis1 = check_case_pattern(Pattern, Analysis),
    check_exprs(Body, Analysis1).


-doc "Check well-formedness of accepted receive clauses".
-spec check_recv_clauses(Clauses :: list(), #analysis{}) -> #analysis{}.
check_recv_clauses(Clauses, Analysis) when is_list(Clauses) ->
    lists:foldl(fun check_recv_clause/2, Analysis, Clauses).

-doc "Check well-formedness of accepted singleton receive clause".
-spec check_recv_clause(Clauses :: tuple(), #analysis{}) -> #analysis{}.
check_recv_clause({clause, _Anno, Pattern, _Guards, Body}, Analysis) ->
    Analysis1 = check_recv_patterns(Pattern, Analysis),
    check_exprs(Body, Analysis1).

-doc "Check well-formedness of accepted patterns in receive clauses".
-spec check_recv_patterns(Form :: list(), #analysis{}) -> #analysis{}.
check_recv_patterns(Patterns, Analysis) when is_list(Patterns) ->
    lists:foldl(fun check_recv_pattern/2, Analysis, Patterns).

-doc "Check well-formedness of accepted patterns in receive clauses".
-spec check_recv_pattern(Form :: tuple() | nil | [tuple()], #analysis{}) -> #analysis{}.
check_recv_pattern(Lit, Analysis) when ?IS_LITERAL(Lit) ->
    check_lit(Lit, Analysis);
check_recv_pattern({var, _Anno, _Name}, Analysis) ->
    Analysis;
check_recv_pattern(Pattern, Analysis) when is_tuple(Pattern) ->
    Anno = erl_syntax:get_pos(Pattern),
    analysis:push_error(?ERR_RECV_PATTERN_UNSUPPORTED, {error, {'receive', Anno, Pattern}}, Analysis).

-doc "Check well-formedness of atomic literals".
-spec check_lit(Form :: tuple(), #analysis{}) -> #analysis{}.
check_lit({atom, _, Value}, Analysis)
  when Value =:= true; Value =:= false; Value =:= ok; Value =:= self; is_atom(Value) ->
  Analysis;
check_lit({float, _, Value}, Analysis) when is_float(Value) ->
  Analysis;
check_lit({integer, _, Value}, Analysis) when is_integer(Value) ->
  Analysis;
check_lit({string, _, Value}, Analysis) when is_list(Value) ->
  Analysis.

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
