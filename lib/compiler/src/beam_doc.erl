%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2028. All Rights Reserved.
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
%% Purpose : Generate documentation as per EEP-48
%% Tool to convert an Erlang program in its abstract form to EEP-48 format.
%%
%% Example:
%%
%% 1> compile:file(test, [beam_docs]).
%%

-module(beam_doc).

-feature(maybe_expr, enable).

-export([main/4, format_error/1]).

-import(lists, [foldl/3, all/2, map/2, filter/2, reverse/1, join/2]).

-include_lib("kernel/include/eep48.hrl").

-moduledoc false.

-doc "
Internal record used when transforming Erlang abstract syntax into EEP-48
documentation format.

- exported_functions are a filter to document only the exported functions
- exported_types are a filter to document only the exported types
- callbacks are always implicitly exported
".
-record(docs, {cwd                 :: file:filename(),             % Cwd
               filename            :: file:filename(),
               curr_filename       :: file:filename(),
               opts                :: [opt()],

               module              :: module(),
               deprecated = #{}    :: map(),

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% EXPORT TRACKING
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               %% tracks exported functions from multiple `-export([...])`
               exported_functions = sets:new() :: sets:set({FunName :: atom(), Arity :: non_neg_integer()}),

               %% tracks exported type from multiple `-export_type([...])`
               exported_types     = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),
               type_defs          = #{}        :: #{{TypeName :: atom(), Arity :: non_neg_integer()} := erl_anno:anno()},


               % user defined types that need to be shown in the documentation. these are types that are not
               %% exported but that the documentation needs to show because exported functions referred to them.
               user_defined_types = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),

               types_from_exported_funs = #{} :: #{{TypeName :: atom(), Arity :: non_neg_integer()} := [erl_anno:anno()]},

               %% on analysing the AST, and upon finding a spec of a exported
               %% function, the types from the spec are added to the field
               %% below. if the function to which the spec belongs to is hidden,
               %% we purge types from this field. if the function to which the
               %% specs belong to are not hidden, they are added to
               %% user_defined_types. Essentially, `last_read_user_types` is a
               %% queue that accumulates types until they can be promoted to
               %% `user_defined_types` or purged (removed).
               %%
               %% RATIONALE / DESIGN
               %%
               %% this field keeps track of these types until we reach the
               %% function definition, which means that we already know if the
               %% function sets `-doc false.`. upon having this information, we
               %% can discard the user defined types when the function uses
               %% `-doc false.` (hidden), since that means that the function
               %% should not be displayed in the docs. if the function is not
               %% hidden, we add the user defined types to the field
               %% `user_defined_types` as these can be (non-)exported types. if
               %% the types are exported, the docs will show the type
               %% definition. if the types are not exported, the type definition
               %% will be shown as not exported.
               last_read_user_types = #{},

               %% hidden_types = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),

               type_dependency = digraph:new() :: digraph:graph(),

               % keeps track of `-compile(export_all)`
               export_all         = false :: boolean(),
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% END
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% DOCUMENTATION TRACKING
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               % Function/type/callback local doc. either none of some string was added
               doc    = none  :: none
                               | {DocText :: unicode:chardata(), Anno :: erl_anno:anno()} ,

               %% track if the doc was never added (none), marked hidden (-doc hidden)
               %% or entered (-doc "..."). If entered, doc_status = set, and doc = "...".
               %% this field is needed because one we do the following:
               %%
               %% -doc hidden.
               %% -doc "This is a hidden function".
               %%
               %% Alternatively, one can merge `doc` and `doc_status` as:
               %%
               %% doc = none | {hidden, "" | none} | "".
               %%
               %% The order in which `-doc hidden.` and `-doc "documentation here"` is written
               %% is not defined, so one cannot assume that the following order:
               %%
               %% -doc "This is a hidden function".
               %% -doc hidden.
               %%
               %% Because of this, we use two fields to keep track of documentation.
               doc_status = none :: none  | {hidden, erl_anno:anno()} | set,
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% END
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               %% slogan :: none | {FunName, ListOfVars, Arity}
               slogan             = none  :: none
                                           | {FunName    :: atom(),
                                              ListOfVars :: [atom()],
                                              Arity      :: non_neg_integer()},

               % Function/type/callback local meta.
               %% exported => boolean(), keeps track of types that are private but used in public functions
               %% thus, they must be considered as exported for documentation purposes.
               %% only useful when processing types. thus, it must be remove from functions and callbacks.
               meta   = #{exported => false} :: map(),


               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% STATEFUL RESULT OF ANALYSIS
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               %% Useful resulting state. The state fields before this line are intermediate state
               ast_fns = [] :: list(),
               ast_types = [] :: list(),
               ast_callbacks = [] :: list(),

               warnings = [] :: warnings()
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% END
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              }).

-type internal_docs() :: #docs{}.
-type opt() :: {warn_missing_doc, boolean()}.
-type kfa() :: {Kind :: function | type | callback, Name :: atom(), Arity :: arity()}.
-type warnings() :: [{file:filename(),
                      [{erl_anno:location(), beam_doc, warning()}]}].
-type warning() :: {missing_doc, kfa()} | missing_moduledoc |
                   {hidden_type_used_in_exported_fun, {atom(), arity()}}.

-define(DEFAULT_MODULE_DOC_LOC, 1).
-define(DEFAULT_FORMAT, <<"text/markdown">>).

-doc "
Transforms an Erlang abstract syntax form into EEP-48 documentation format.
".
-spec main(file:filename(), file:filename(), [erl_parse:abstract_form()], [opt()]) ->
          {ok, #docs_v1{}, [{file:filename(),warnings()}]}.
main(Dirname, Filename, AST, Opts) ->
    State = new_state(Dirname, Filename, Opts),
    {ModuleDocAnno, ModuleDoc} = extract_moduledoc(AST),
    DocFormat = extract_docformat(AST),
    {State1, AST1} = extract_exported_types(AST, State),
    Docs = extract_documentation(AST1, State1),
    DocV1 = #docs_v1{},
    Meta = extract_meta(AST, DocV1#docs_v1.metadata),
    Result = DocV1#docs_v1{ format = DocFormat,
                            anno = ModuleDocAnno,
                            metadata = Meta,
                            module_doc = create_module_doc(ModuleDoc),
                            docs = process_docs(Docs) },

    ModuleDocWarning =
        case proplists:get_value(warn_missing_doc, Opts, false) of
            true when ModuleDoc =:= none ->
                [{Filename,[{?DEFAULT_MODULE_DOC_LOC, beam_doc, missing_moduledoc}]}];
            _false ->
                []
        end,

   {ok, Result, ModuleDocWarning ++ Docs#docs.warnings }.

-spec format_error(warning()) -> io_lib:chars().
format_error({hidden_type_used_in_exported_fun, {Type, Arity}}) ->
    io_lib:format("hidden type '~p/~p' used in exported function",
                  [Type, Arity]);
format_error({missing_doc, {Kind, Name, Arity}}) ->
    io_lib:format("missing -doc for ~w ~tw/~w", [Kind, Name, Arity]);
format_error(missing_moduledoc) ->
    io_lib:format("missing -moduledoc", []).

process_docs(#docs{ast_callbacks = AstCallbacks, ast_fns = AstFns, ast_types = AstTypes}) ->
    AstTypes ++ AstCallbacks ++ AstFns.

extract_meta(AST, MetaData) ->
    foldl(fun ({attribute, _ModuleDocAnno, moduledoc, MetaDoc}, Meta) when is_map(MetaDoc) ->
                  maps:merge(Meta, MetaDoc);
              (_, Meta) -> Meta
          end, MetaData, AST).

extract_exported_types(AST, State) ->
   {State1, AST1} = extract_exported_types(AST, State, []),
   {State1, reverse(AST1)}.
extract_exported_types([], State, NewAST) ->
   {State, NewAST};
extract_exported_types([{attribute,_ANNO,module, Module} | T], State, NewAST) ->
    extract_exported_types(T, State#docs{ module = Module }, NewAST);
extract_exported_types([{attribute,_ANNO,export_type,ExportedTypes} | T], State, NewAST) ->
   extract_exported_types(T, update_export_types(State, ExportedTypes), NewAST);
extract_exported_types([{attribute,_ANNO,compile, export_all} | T], State, NewAST) ->
   extract_exported_types(T, update_export_all(State, true), NewAST);
extract_exported_types([{attribute, Anno, deprecated, {F, A}} | T], State, NewAST) ->
    extract_exported_types([{attribute, Anno, deprecated, {F, A, undefined}} | T], State, NewAST);
extract_exported_types([{attribute, _, deprecated, {F, A, Reason}} | T], State, NewAST) ->
    Deprecations = (State#docs.deprecated)#{ {function, F, A} => Reason },
    extract_exported_types(T, State#docs{ deprecated = Deprecations }, NewAST);
extract_exported_types([{attribute, Anno, deprecated_type, {F, A}} | T], State, NewAST) ->
    extract_exported_types([{attribute, Anno, deprecated_type, {F, A, undefined}} | T], State, NewAST);
extract_exported_types([{attribute, _, deprecated_type, {F, A, Reason}} | T], State, NewAST) ->
    Deprecations = (State#docs.deprecated)#{ {type, F, A} => Reason },
    extract_exported_types(T, State#docs{ deprecated = Deprecations }, NewAST);
extract_exported_types([Other | AST], State, NewAST) ->
   extract_exported_types(AST, State, [Other | NewAST]).

%% extract_hidden_types()


-spec extract_moduledoc(AST :: [erl_parse:abstract_form()]) ->
          ModuleDoc :: {erl_anno:anno(), binary() | none | hidden}.
extract_moduledoc([]) ->
    {?DEFAULT_MODULE_DOC_LOC, none};
extract_moduledoc([{attribute, ModuleDocAnno, moduledoc, false}| _AST]) ->
    extract_moduledoc([{attribute, ModuleDocAnno, moduledoc, hidden}| _AST]);
extract_moduledoc([{attribute, ModuleDocAnno, moduledoc, hidden}| _AST]) ->
    {ModuleDocAnno, hidden};
extract_moduledoc([{attribute, ModuleDocAnno, moduledoc, ModuleDoc}| _AST]) when is_list(ModuleDoc) ->
    {ModuleDocAnno, unicode:characters_to_binary(string:trim(ModuleDoc))};
extract_moduledoc([_| AST]) ->
    extract_moduledoc(AST).

extract_docformat([]) ->
    ?DEFAULT_FORMAT;
extract_docformat([{attribute, _ModuleDocAnno, moduledoc, MetaFormat} | Ls]) when is_map(MetaFormat) ->
    case maps:get(format, MetaFormat, not_found) of
        not_found -> extract_docformat(Ls);
        Format when is_list(Format) -> unicode:characters_to_binary(Format);
        Format when is_binary(Format) -> Format
    end;
extract_docformat([_ | Ls]) ->
    extract_docformat(Ls).

-spec create_module_doc(ModuleDoc :: binary() | none | hidden) -> map().
create_module_doc(ModuleDoc) when is_atom(ModuleDoc) ->
    ModuleDoc;
create_module_doc(ModuleDoc) when not is_atom(ModuleDoc) ->
    create_module_doc(<<"en">>, ModuleDoc).

-spec create_module_doc(Lang :: binary(), ModuleDoc :: binary()) -> map().
create_module_doc(Lang, ModuleDoc) ->
    #{Lang => ModuleDoc}.

-spec new_state(Dirname :: file:filename(), Filename :: file:filename(),
                Opts :: [opt()]) -> internal_docs().
new_state(Dirname, Filename, Opts) ->
    reset_state(#docs{cwd = Dirname, filename = Filename,
                      curr_filename = Filename, opts = Opts}).

-spec reset_state(State :: internal_docs()) -> internal_docs().
reset_state(State) ->
    State#docs{doc = none,
               doc_status = none,
               meta = #{exported => false},
               slogan = none}.

update_docstatus(State, V) ->
    State#docs{doc_status = V}.

update_ast(function, #docs{ast_fns=AST}=State, Fn) ->
    State#docs{ast_fns = [Fn | AST]};
update_ast(Type,#docs{ast_types=AST}=State, Fn) when Type =:= type; Type =:= opaque->
    State#docs{ast_types = [Fn | AST]};
update_ast(callback, #docs{ast_callbacks = AST}=State, Fn) ->
    State#docs{ast_callbacks = [Fn | AST]}.

-spec update_meta(State :: internal_docs(), Meta :: map()) -> internal_docs().
update_meta(#docs{meta = Meta0}=State, Meta1) ->
    State#docs{meta = maps:merge(Meta0, Meta1)}.

-spec update_user_defined_types(State :: internal_docs()) -> internal_docs().
update_user_defined_types(#docs{doc_status = DocStatus,
                                user_defined_types = UserDefinedTypes,
                                last_read_user_types = LastAddedTypes}=State) ->
   case DocStatus of
      {hidden, _Anno} ->
         State#docs{last_read_user_types = #{}};
      _ ->
         State#docs{user_defined_types = sets:union(UserDefinedTypes, LastAddedTypes),
                    last_read_user_types = #{}}
   end.

%% update_user_defined_types(#docs{}=State, Types) ->
%%    State#docs{last_read_user_types = Types}.

-spec update_doc(State :: internal_docs(), Doc) -> internal_docs() when
     Doc :: {unicode:chardata(), erl_anno:anno()} | atom().
update_doc(#docs{doc_status = DocStatus}=State, Doc0) ->
    %% The exported := true only applies to types and should be ignored for functions.
    %% This is because we need to export private types that are used on public
    %% functions, or the documentation will create dead links.
    Status = set_doc_status(DocStatus),
    State1 = update_docstatus(State, Status),
    State2 = update_meta(State1, #{exported => true}),
    case Doc0 of
        none ->
            State2;
        {Doc, Anno} ->
            State2#docs{doc = {string:trim(Doc), set_file_anno(Anno, State)}}
    end.

set_file_anno(Anno, State) ->
        case {State#docs.curr_filename, erl_anno:file(Anno)} of
            {ModuleName, undefined} when ModuleName =/= "",
                                         ModuleName =/= State#docs.filename ->
                erl_anno:set_file(ModuleName, Anno);
            _ ->
                Anno
        end.
    
%% Sets the doc status from `none` to `set`.
%% Leave unchanged if the status was already set to something.
set_doc_status(none) ->
    set;
set_doc_status(Other) ->
    Other.

-spec update_slogan(#docs{}, {FunName, Vars, Arity}) -> #docs{} when
     FunName :: atom(),
     Vars :: [atom()],
     Arity :: non_neg_integer().
update_slogan(#docs{}=State, {FunName, Vars, Arity}=Slogan)
  when is_atom(FunName) andalso is_list(Vars) andalso is_number(Arity) ->
   State#docs{slogan = Slogan}.

-spec update_filename(State :: internal_docs(), ModuleName :: unicode:chardata()) -> internal_docs().
update_filename(#docs{}=State, ModuleName) ->
    State#docs{curr_filename = ModuleName}.

-spec update_export_funs(State :: internal_docs(), proplists:proplist()) -> internal_docs().
update_export_funs(State, ExportedFuns) ->
    ExportedFuns1 = sets:union(State#docs.exported_functions, sets:from_list(ExportedFuns)),
    State#docs{exported_functions = ExportedFuns1}.

-spec update_export_types(State :: internal_docs(), proplists:proplist()) -> internal_docs().
update_export_types(State, ExportedTypes) ->
    ExportedTypes1 = sets:union(State#docs.exported_types, sets:from_list(ExportedTypes)),
    State#docs{exported_types = ExportedTypes1}.

update_export_all(State, ExportAll) ->
    State#docs{ export_all = ExportAll }.

remove_exported_type_info(State) ->
    Meta = State#docs.meta,
    State#docs{meta = maps:remove(exported, Meta)}.

extract_documentation(AST, State) ->
   State1 = extract_documentation0(AST, State),
   State2 = purge_private_types(State1),
   warnings(AST, State2).

warnings(AST, State) ->
   warn_hidden_types_used_in_public_fns(AST, State).

%% hidden types with `-doc hidden.` or `-doc false.`, which are public (inside
%% `export_type([])`), and used in public functions, they do not make sense. It
%% is a type that is not documented (due to hidden property), visible in the
%% docs (because it is in export_type), and reference / used by a public
%% function cannot be used.
%% A type that is hidden, private, and used in an exported function will be documented
%% by the doc generation showing the internal type structure.
warn_hidden_types_used_in_public_fns(AST, #docs{types_from_exported_funs = TypesFromExportedFuns,
                                                type_dependency = TypeDependency,
                                                type_defs = TypeDefs}=State) ->
   HiddenTypes = extract_hidden_types(AST),
   Types = maps:keys(TypesFromExportedFuns),
   ReachableTypes = digraph_utils:reachable(Types, TypeDependency),
   ReachableSet = sets:from_list(ReachableTypes),
   Warnings = sets:intersection(HiddenTypes, ReachableSet),
   WarningsWithAnno = sets:map(fun (Key) ->
                                     create_warning({Key, maps:get(Key, TypeDefs)}, State)
                               end, Warnings),
   State#docs{warnings = State#docs.warnings ++ sets:to_list(WarningsWithAnno) }.

create_warning({Type, Anno}, State) ->
   Location = erl_anno:location(Anno),
   Warning = {hidden_type_used_in_exported_fun, Type},
   {erl_anno_file(Anno, State), [{Location, beam_doc, Warning}]}.

purge_private_types(#docs{ast_types = AstTypes,
                          user_defined_types = UserDefinedTypes}=State) ->
   AstTypes1 = filter(fun ({{_, F, A}, _Anno, _Slogan, _Doc, #{exported := Exported}}) ->
                                  sets:is_element({F, A}, UserDefinedTypes) orelse Exported
                            end, AstTypes),
   State#docs{ast_types = AstTypes1}.

extract_documentation0([{attribute,_ANNO,export,ExportedFuns} | T]=_AST, State) ->
    extract_documentation0(T, update_export_funs(State, ExportedFuns));
extract_documentation0([{attribute, _Anno, file, {Filename, _A}} | T], State) ->
    extract_documentation0(T, update_filename(State, Filename));
extract_documentation0([{attribute, _Anno, doc, _Meta0}| _]=AST, State) ->
    extract_documentation_from_doc(AST, State);
extract_documentation0([{attribute, _Anno, spec, _}| _]=AST, State) ->
   extract_documentation_spec(AST, State);
extract_documentation0([AST0 | _T]=AST,
                      #docs{meta = #{ equiv := {call,_,_Equiv,_Args} = Equiv} = Meta}=State)
    when is_tuple(AST0) andalso (tuple_size(AST0) > 2 orelse tuple_size(AST0) < 6) ->
    Meta1 = Meta#{ equiv := unicode:characters_to_binary(erl_pp:expr(Equiv)) },
    extract_documentation0(AST, update_meta(State, Meta1));
extract_documentation0([AST0 | _T]=AST,
                      #docs{meta = #{ equiv := {Func,Arity}} = Meta}=State)
    when is_tuple(AST0) andalso (tuple_size(AST0) > 2 orelse tuple_size(AST0) < 6) ->
    Meta1 = Meta#{ equiv := unicode:characters_to_binary(io_lib:format("~p/~p",[Func,Arity])) },
    extract_documentation0(AST, update_meta(State, Meta1));
extract_documentation0([{function, _Anno, _F, _A, _Body} | _]=AST, State) ->
    State1 = remove_exported_type_info(State),
    extract_documentation_from_funs(AST, State1);
extract_documentation0([{attribute, _Anno, TypeOrOpaque, _} | _]=AST,State)
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque ->
    extract_documentation_from_type(AST, State);
extract_documentation0([{attribute, _Anno, callback, _} | _]=AST, State) ->
    State1 = remove_exported_type_info(State),
    extract_documentation_from_cb(AST, State1);
extract_documentation0([_H|T], State) ->
    extract_documentation0(T, State);
extract_documentation0([], #docs{doc_status = none}=State) ->
    State.


extract_documentation_spec([{attribute, Anno, spec, Form}=AST0| T], State) ->
   State1 = extract_slogan_from_spec(AST0, State),
   State2 = extract_spec_types(Anno, Form, State1),
   extract_documentation0(T, State2).

%% this is because public functions may use private types and these private
%% types need to be included in the beam and documentation.
extract_spec_types(Anno, {{Name,Arity}, SpecTypes}, #docs{exported_functions = ExpFuns}=State) ->
   case sets:is_element({Name, Arity}, ExpFuns) orelse State#docs.export_all of
      true ->
         %% io:format("(~p:~p) ~p~n", [?MODULE, ?LINE, {Name, Arity}]),
         add_user_types(Anno, SpecTypes, State);
      false ->
         State
   end;
extract_spec_types(Anno, {{_Mod, Name, Arity}, Types}, State) ->
   extract_spec_types(Anno, {{Name, Arity}, Types}, State).

add_user_types(_Anno, SpecTypes, State) ->
   Types = extract_user_types(SpecTypes),
   State1 = set_types_used_in_public_funs(State, Types),
   set_last_read_user_types(State1, Types).

%% pre: only call this function to add types from external functions.
set_types_used_in_public_funs(#docs{types_from_exported_funs = TypesFromExportedFuns}=State, Types) ->
   Combiner = fun (_Key, Value1, Value2) -> Value1 ++ Value2 end,
   Types0 = maps:merge_with(Combiner, TypesFromExportedFuns, Types),
   State#docs{types_from_exported_funs = Types0}.

set_last_read_user_types(#docs{}=State, Types) ->
   State#docs{last_read_user_types = Types}.

extract_user_types(Args) ->
   extract_user_types(Args, maps:new()).
extract_user_types(Types, Acc) when is_list(Types) ->
  foldl(fun extract_user_types/2, Acc, Types);
extract_user_types({ann_type, _, [_Name, Type]}, Acc) ->
   extract_user_types(Type, Acc);
extract_user_types({type, _, 'fun', Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _, map, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _,record,[_Name | Args]}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({remote_type,_,[_ModuleName,_TypeName,Args]}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _, tuple, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _,union, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({user_type, Anno, Name, Args}, Acc) ->
   %% append user type and continue iterating through lists in case of other
   %% user-defined types to be added
   Fun = fun (Value) -> [Anno | Value] end,
   Acc1 = maps:update_with({Name, length(Args)}, Fun, [Anno], Acc),
   extract_user_types(Args, Acc1);
extract_user_types({type, _, bounded_fun, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type,_,product,Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type,_,constraint,[{atom,_,is_subtype},Args]}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _, map_field_assoc, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _, map_field_exact, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type,_,field_type,[_Name, Type]}, Acc) ->
   extract_user_types(Type, Acc);
extract_user_types({type, _,_BuiltIn, Args}, Acc) when is_list(Args)->
   %% Handles built-in types such as 'list', 'nil' 'range'.
   extract_user_types(Args, Acc);
extract_user_types(_Else, Acc) ->
    Acc.


%% Accumulate list of variables. if some type is not simply a var,
%% i.e., distinct from foo(X, Y) where all arguments are variables,
%% return false to signal that we cannot extract a slogan from them spec
extract_slogan_from_spec({attribute, _Anno, Tag, Form}, State) when Tag =:= spec; Tag =:= callback ->
   maybe
      %% checks that slogans are not considered for multi-clause callbacks and specs
      {Name, Arity, [{type, _, 'fun', [{type, _, product, Args}, _Return]}]} ?=
         case Form of
            {{Name0, Arity0}, Types0} ->
               {Name0, Arity0, Types0};
            {{_Mod, Name0, Arity0}, Types0} ->
               {Name0, Arity0, Types0}
         end,

      Vars = foldl(fun (_, false) -> false;
                       ({var, _, Var}, Vars) -> [Var | Vars];
                       ({ann_type, _, [{var, _, Var} | _]}, Vars) -> [Var | Vars];
                       (_, _) -> false
                   end, [], Args),

      true ?= is_list(Vars),
      Arity ?= length(Vars),
      update_slogan(State, {Name, lists:reverse(Vars), Arity})
   else
      _ ->
         State
   end.



%% NOTE: Terminal elements for the documentation, such as `-type`, `-opaque`, `-callback`,
%%       and functions always need to reset the state when they finish, so that new
%%       new AST items start with a clean slate.
extract_documentation_from_type([{attribute, Anno, TypeOrOpaque, {TypeName, _TypeDef, TypeArgs}=Types}=_AST | T],
                      #docs{exported_types=ExpTypes, meta=Meta}=State)
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque ->
   Args = fun_to_varargs(TypeArgs),
   State0 = add_type_dependency(Anno, Types, State),
   Type = {TypeName, length(Args)},
   State1 = State0#docs{ meta = Meta#{exported := sets:is_element(Type, ExpTypes)}},
   State2 = gen_doc_with_slogan({type, Anno, TypeName, length(Args), Args}, State1),
   State3 = add_type_defs(Type, State2),
   extract_documentation0(T, State3).

add_type_defs(Type, #docs{type_defs = TypeDefs, ast_types = [{_KFA, Anno, _Slogan, _Doc, _Meta} | _]}=State) ->
   State#docs{type_defs = TypeDefs#{Type => Anno}}.

add_type_dependency(_Anno, {TypeName, TypeDef, TypeArgs}, #docs{type_dependency = TypeDependency}=State) ->
   Types = extract_user_types([TypeArgs, TypeDef]),
   State0 = set_last_read_user_types(State, Types),
   #docs{last_read_user_types = LastReadUserTypes} = State0,
   Type = {TypeName, length(TypeArgs)},
   digraph:add_vertex(TypeDependency, Type),
   UserTypes = maps:to_list(LastReadUserTypes),
   _ = [begin
            digraph:add_vertex(TypeDependency, TypeAndArity),
            digraph:add_edge(TypeDependency, Type, TypeAndArity)
        end || TypeAndArity <- UserTypes],
   State0.


extract_documentation_from_doc([{attribute, _Anno, doc, Meta0}=_AST | T], State) when is_map(Meta0) ->
    State1 = update_meta(State, Meta0),
    extract_documentation0(T, update_doc(State1, none));
extract_documentation_from_doc([{attribute, Anno, doc, DocStatus}=_AST | T], State)
  when DocStatus =:= hidden; DocStatus =:= false ->
    State1 = update_docstatus(State, {hidden, set_file_anno(Anno, State)}),
    extract_documentation0(T, State1);
extract_documentation_from_doc([{attribute, Anno, doc, Doc}=_AST | T], State) when is_list(Doc)  ->
   extract_documentation0(T, update_doc(State, {Doc, Anno}));
extract_documentation_from_doc([{attribute, Anno, doc, Doc}=_AST | T], State) when is_binary(Doc) ->
   extract_documentation0(T, update_doc(State, {unicode:characters_to_list(Doc), Anno})).

%%
%% Extracts types with documentation attribute set to `hidden` or `false`.
%%
%% E.g.:
%%
%%    -doc hidden.
%%    -type foo() :: integer().
%%
extract_hidden_types(AST) ->
   CurrentStatus = none,
   HiddenTypes = sets:new(),
   {_, Result} = foldl(fun extract_hidden_types/2, {CurrentStatus, HiddenTypes}, AST),
   Result.
extract_hidden_types({attribute, _Anno, doc, DocStatus}, {_TypeState, HiddenTypes}) when
   DocStatus =:= hidden; DocStatus =:= false ->
   {hidden, HiddenTypes};
extract_hidden_types({attribute, _Anno, doc, _}, Acc) ->
   Acc;
extract_hidden_types({attribute, _Anno, TypeOrOpaque, {Name, _Type, Args}}, {hidden, HiddenTypes})
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque ->
   {none, sets:add_element({Name, length(Args)}, HiddenTypes)};
extract_hidden_types(_, {_, HiddenTypes}) ->
   {none, HiddenTypes}.


%% NOTE: Terminal elements for the documentation, such as `-type`, `-opaque`, `-callback`,
%%       and functions always need to reset the state when they finish, so that new
%%       new AST items start with a clean slate.
extract_documentation_from_funs([{function, Anno, F, A, [{clause, _, ClauseArgs, _, _}]}=_AST | T],
                      #docs{exported_functions = ExpFuns}=State) ->
    State1 = case (sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all) of
                true ->
                   gen_doc_with_slogan({function, Anno, F, A, ClauseArgs}, State);
                false ->
                   reset_state(State)
             end,
   extract_documentation0(T, State1);
extract_documentation_from_funs([{function, Anno0, F, A, _Body}=_AST | T],
                      #docs{doc = Doc0, exported_functions=ExpFuns}=State) ->
    {Doc1, Anno1} = case Doc0 of
                        none -> {none, set_file_anno(Anno0, State)};
                        {Doc, Anno} -> {Doc, Anno}
                    end,

    State1 = case sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all of
               true ->
                  {Slogan, DocsWithoutSlogan} = extract_slogan(Doc1, State, F, A),
                  AttrBody = {function, F, A},
                  gen_doc(Anno1, AttrBody, Slogan, DocsWithoutSlogan, State);
               false ->
                  reset_state(State)
            end,
   extract_documentation0(T, State1).

extract_documentation_from_cb([{attribute, Anno, callback, {{CB, A}, [Fun]=Form}}=AST0 | T], State) ->
   State1 = extract_slogan_from_spec(AST0, State),

   %% adds user types as part of possible types that need to be exported
   State2 = add_user_types(Anno, Form, State1),

   Args = fun_to_varargs(Fun),
   State3 = gen_doc_with_slogan({callback, Anno, CB, A, Args}, State2),
   extract_documentation0(T, State3);
extract_documentation_from_cb([{attribute, Anno0, callback, {{CB, A}, Form}}=AST0 | T], #docs{doc = Doc0}=State) ->
   %% Multi-clause callback. Do not create a slogan from the callback args.

   State1 = extract_slogan_from_spec(AST0, State),

   %% adds user types as part of possible types that need to be exported
   State2 = add_user_types(Anno0, Form, State1),

   {Doc1, Anno1} = case Doc0 of
                        none -> {none, set_file_anno(Anno0, State)};
                        {Doc, Anno} -> {Doc, Anno}
                    end,

   {Slogan, DocsWithoutSlogan} = extract_slogan(Doc1, State2, CB, A),

   AttrBody = {callback, CB, A},
   State3 = gen_doc(Anno1, AttrBody, Slogan, DocsWithoutSlogan, State2),
   extract_documentation0(T, State3).


%% Generates documentation
-spec gen_doc(Anno, AttrBody, Slogan, Docs, State) -> Response when
      Anno      :: erl_anno:anno(),
      AttrBody  :: {function | type | callback, term(), integer()},
      Slogan    :: unicode:chardata(),
      Docs      :: none | hidden | unicode:chardata() | #{ <<_:16>> => unicode:chardata() },
      State     :: internal_docs(),
      Response  :: internal_docs().
gen_doc(Anno, AttrBody, Slogan, Docs, State) when not is_atom(Docs), not is_map(Docs) ->
    gen_doc(Anno, AttrBody, Slogan, #{ <<"en">> => unicode:characters_to_binary(string:trim(Docs)) }, State);
gen_doc(Anno, {Attr, _F, _A}=AttrBody, Slogan, Docs, #docs{meta = Meta}=State) ->
   Result = {AttrBody, Anno, [unicode:characters_to_binary(Slogan)], Docs,
             maybe_add_deprecation(AttrBody, Meta, State)},
   State1 = update_user_defined_types(State),
   reset_state(update_ast(Attr, warn_missing_docs(Result, State1), Result)).

warn_missing_docs({KFA, Anno, _, Doc, _}, State) ->
    case proplists:get_value(warn_missing_doc, State#docs.opts, false) of
        true when Doc =:= none ->
            Filename = erl_anno_file(Anno, State),
            Warning = {Filename, [{erl_anno:location(Anno), beam_doc, {missing_doc, KFA}}]},
            State#docs{ warnings = [Warning | State#docs.warnings] };
        _false ->
            State
    end.

erl_anno_file(Anno, State) ->
    case erl_anno:file(Anno) of
        undefined ->
            State#docs.filename;
        FN -> FN
    end.

maybe_add_deprecation(_KNA, #{ deprecated := Deprecated } = Meta, _State) ->
    Meta#{ deprecated := unicode:characters_to_binary(Deprecated) };
maybe_add_deprecation({Kind, Name, Arity}, Meta, #docs{ module = Module,
                                                        deprecated = Deprecations }) ->
    maybe
        error ?= maps:find({Kind, Name, Arity}, Deprecations),
        error ?= maps:find({Kind, Name, '_'}, Deprecations),
        error ?= maps:find({Kind, '_', Arity}, Deprecations),
        error ?= maps:find({Kind, '_', '_'}, Deprecations),
        Meta
    else
        {ok, Value} ->
            Text =
                if Kind =:= function ->
                        erl_lint:format_error({deprecated, {Module,Name,Arity},
                                               info_string(Value)});
                   Kind =:= type ->
                        erl_lint:format_error({deprecated_type, {Module,Name,Arity},
                                               info_string(Value)})
                end,
            Meta#{ deprecated => unicode:characters_to_binary(Text) }
    end.

%% Copies from lib/stdlib/scripts/update_deprecations
info_string(undefined) ->
    "see the documentation for details";
info_string(next_version) ->
    "will be removed in the next version. "
        "See the documentation for details";
info_string(next_major_release) ->
    "will be removed in the next major release. "
        "See the documentation for details";
info_string(eventually) ->
    "will be removed in a future release. "
        "See the documentation for details";
info_string(String) when is_list(String) ->
    String.

%% Generates the documentation inferring the slogan from the documentation.
gen_doc_with_slogan({Attr, _Anno0, F, A, Args}=AST, State) ->
    {Doc1, Anno1} = fetch_doc_and_anno(State, AST),
    {Slogan, DocsWithoutSlogan} = extract_slogan(Doc1, State, F, A, Args),
    AttrBody = {Attr, F, A},
    gen_doc(Anno1, AttrBody, Slogan, DocsWithoutSlogan, State).

fetch_doc_and_anno(#docs{doc = Doc, doc_status = DocStatus}=State, {_Attr, Anno0, _F, _A, _Args}) ->
    case {DocStatus, Doc} of
        {{hidden, Anno}, _} -> {hidden, Anno};
        {_, none} -> {none, set_file_anno(Anno0, State)};
        {_, {Doc1, Anno}} -> {Doc1, Anno}
    end.

-spec fun_to_varargs(tuple() | term()) -> list(term()).
fun_to_varargs({type, _, bounded_fun, [T|_]}) ->
    fun_to_varargs(T);
fun_to_varargs({type, _, 'fun', [{type,_,product,Args}|_] }) ->
    map(fun fun_to_varargs/1, Args);
fun_to_varargs({ann_type, _, [Name|_]}) ->
    Name;
fun_to_varargs({var,_,_} = Name) ->
    Name;
fun_to_varargs(Else) ->
    Else.

extract_slogan(Doc, State, F, A) ->
    extract_slogan(Doc, State, F, A, [invalid]).
extract_slogan(Doc, State, F, A, Args) ->
    maybe
        false ?= Doc =:= none orelse Doc =:= hidden,
        [MaybeSlogan | Rest] = string:split(Doc, "\n"),
        {ok, Toks, _} ?= erl_scan:string(unicode:characters_to_list([MaybeSlogan,"."])),
        {ok, [{call,_,{atom,_,F},SloganArgs}]} ?= erl_parse:parse_exprs(Toks),
        A ?= length(SloganArgs),
        {MaybeSlogan, Rest}
    else
        _ ->
            Slogan =
                case State#docs.slogan of
                    none ->
                        case all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args)  of
                            true ->
                                extract_slogan_from_args(F, Args);
                            false -> io_lib:format("~p/~p",[F,A])
                        end;
                    {F, Vars, A} ->
                        VarString = join(", ",[atom_to_list(Var) || Var <- Vars]),
                        unicode:characters_to_list(io_lib:format("~p(~s)", [F, VarString]))
                end,
            {Slogan, Doc}
    end.

extract_slogan_from_args(F, Args) ->
    io_lib:format("~p(~ts)",[F, join(", ",[string:trim(atom_to_list(Arg),leading,"_") || {var, _, Arg} <- Args])]).
