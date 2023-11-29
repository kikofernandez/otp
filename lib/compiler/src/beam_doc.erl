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

-export([main/3]).

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

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% EXPORT TRACKING
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

               %% tracks exported functions from multiple `-export([...])`
               exported_functions = sets:new() :: sets:set({FunName :: atom(), Arity :: non_neg_integer()}),

               %% tracks exported type from multiple `-export_type([...])`
               exported_types     = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),


               % user defined types that need to be shown in the documentation. these are types that are not
               %% exported but that the documentation needs to show because exported functions referred to them.
               user_defined_types = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),

               types_from_exported_funs = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),

               %% on analysing the AST, and upon finding a spec of a exported function,
               %% the types from the spec are added to the field below.
               %% if the function to which the spec belongs to is hidden, we purge types from this field.
               %% if the function to which the specs belong to are not hidden, they are added to user_defined_types.
               %%
               %% this field keeps track of these types until we reach the function definition, which
               %% means that we already know if the function sets `-doc false.`. upon having this information,
               %% we can discard the user defined types when the function uses `-doc false.` (hidden), since that means
               %% that the function should not be displayed in the docs. if the function is not hidden,
               %% we add the user defined types to the field `user_defined_types` as these can be (non-)exported
               %% types. if the types are exported, the docs will show the type definition. if the types
               %% are not exported, the type definition will be shown as not exported.
               last_read_user_types = sets:new() :: sets:set({TypeName :: atom(), Arity :: non_neg_integer()}),

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
               doc_status = none :: none  | hidden | set,
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
               ast_warnings = sets:new() :: sets:set()
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %%
               %% END
               %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              }).

-type internal_docs() :: #docs{}.

-define(DEFAULT_MODULE_DOC_LOC, 1).
-define(DEFAULT_FORMAT, <<"text/markdown">>).

-doc "
Transforms an Erlang abstract syntax form into EEP-48 documentation format.
".
-spec main(file:filename(), file:filename(), [erl_parse:abstract_form()]) ->
          #docs_v1{}.
main(Dirname, Filename, AST) ->
    try
       State = new_state(Dirname, Filename),
       {ModuleDocAnno, ModuleDoc} = extract_moduledoc(AST),
       DocFormat = extract_docformat(AST),
       {State1, AST1} = extract_exported_types(AST, State),
       Docs = extract_documentation(AST1, State1),
       DocV1 = #docs_v1{},
       Meta = extract_meta(AST, DocV1#docs_v1.metadata),
       DocV1#docs_v1{ format = DocFormat,
                      anno = ModuleDocAnno,
                      metadata = Meta,
                      module_doc = create_module_doc(ModuleDoc),
                      docs = process_docs(Docs) }
    catch E:R:ST ->
          erlang:raise(E, R, ST)
    end.

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
extract_exported_types([{attribute,_ANNO,export_type,ExportedTypes} | T], State, NewAST) ->
   extract_exported_types(T, update_export_types(State, ExportedTypes), NewAST);
extract_exported_types([{attribute,_ANNO,compile, export_all} | T], State, NewAST) ->
   extract_exported_types(T, update_export_all(State, true), NewAST);
extract_exported_types([Other | AST], State, NewAST) ->
   extract_exported_types(AST, State, [Other | NewAST]).

%% extract_hidden_types()


-spec extract_moduledoc(AST :: [tuple()]) -> ModuleDoc :: {erl_anno:anno(), binary() | none | hidden}.
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

-spec create_module_doc(ModuleDoc :: binary()) -> map().
create_module_doc(ModuleDoc) when is_atom(ModuleDoc) ->
    ModuleDoc;
create_module_doc(ModuleDoc) when not is_atom(ModuleDoc) ->
    create_module_doc(<<"en">>, ModuleDoc).

-spec create_module_doc(Lang :: binary(), ModuleDoc :: binary()) -> map().
create_module_doc(Lang, ModuleDoc) ->
    #{Lang => ModuleDoc}.

-spec new_state(Dirname :: file:filename(), Filename :: file:filename()) -> internal_docs().
new_state(Dirname, Filename) ->
    reset_state(#docs{cwd = Dirname, filename = Filename}).

-spec reset_state(State :: internal_docs()) -> internal_docs().
reset_state(State) ->
    State#docs{doc = none,
               doc_status = none,
               meta = #{exported => false},
               last_read_user_types = sets:new(),
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
      hidden ->
         State#docs{last_read_user_types = sets:new()};
      _ ->
         State#docs{user_defined_types = sets:union(UserDefinedTypes, LastAddedTypes),
                    last_read_user_types = sets:new()}
   end.


set_last_read_user_types(#docs{}=State, Types) ->
   State#docs{last_read_user_types = Types}.

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
            FileAnno =
                case State#docs.filename of
                    "" ->
                        Anno;
                    ModuleName ->
                        erl_anno:set_file(ModuleName, Anno)
                end,
            State2#docs{doc = {string:trim(Doc), FileAnno}}
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
    State#docs{filename = ModuleName}.

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

warn_hidden_types_used_in_public_fns(AST, #docs{types_from_exported_funs = TypesFromExportedFuns,
                                                ast_warnings = AstWarnings}=State) ->

   %% new pass to the AST to avoid more complexity in the State
   HiddenTypes = extract_hidden_types(AST),

   Result = {hidden_types_in_public_fns, sets:intersection(HiddenTypes, TypesFromExportedFuns)},
   State#docs{ast_warnings = sets:add_element(Result, AstWarnings)}.

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
   %% AST0 matches here terminal attributes to fix the metadata before processing them.
   %% terminal attribute, such as -type, function, -opaque, -callback.
   %% This is because terminal items are the ones that produce the documentation, and
   %% then move to another AST object.
    Meta1 = Meta#{ equiv := unicode:characters_to_binary(erl_pp:expr(Equiv)) },
    extract_documentation0(AST, update_meta(State, Meta1));
extract_documentation0([AST0 | _T]=AST,
                      #docs{meta = #{ equiv := {Func,Arity}} = Meta}=State)
    when is_tuple(AST0) andalso (tuple_size(AST0) > 2 orelse tuple_size(AST0) < 6) ->
   %% AST0 matches here terminal attributes to fix the metadata before processing them.
   %% terminal attribute, such as -type, function, -opaque, -callback.
   %% This is because terminal items are the ones that produce the documentation, and
   %% then move to another AST object.
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


extract_documentation_spec([{attribute, _Anno, spec, Form}=AST0| T], State) ->
   State1 = extract_slogan_from_spec(AST0, State),
   State2 = extract_spec_types(Form, State1),
   extract_documentation0(T, State2).

%% this is because public functions may use private types and these private
%% types need to be included in the beam and documentation.
extract_spec_types({{Name,Arity}, SpecTypes}, #docs{exported_functions = ExpFuns}=State) ->
   case sets:is_element({Name, Arity}, ExpFuns) orelse State#docs.export_all of
      true ->
         add_user_types(SpecTypes, State);
      false ->
         State
   end;
extract_spec_types({{_Mod, Name, Arity}, Types}, State) ->
   extract_spec_types({{Name, Arity}, Types}, State).

add_user_types(SpecTypes, State) ->
   Types = extract_user_types(SpecTypes, sets:new()),
   State1 = set_types_used_in_public_funs(State, Types),
   set_last_read_user_types(State1, Types).

%% pre: only call this function to add types from external functions.
set_types_used_in_public_funs(#docs{types_from_exported_funs = TypesFromExportedFuns}=State, Types) ->
   Types0 = sets:union(TypesFromExportedFuns, Types),
   State#docs{types_from_exported_funs = Types0}.


extract_user_types([], Acc) ->
   Acc;
extract_user_types([A | Args], Acc) ->
   Acc1 = extract_user_types(A, Acc),
   extract_user_types(Args, Acc1);
extract_user_types({ann_type, _, [_Name, Type]}, Acc) ->
   extract_user_types(Type, Acc);
extract_user_types({type,_,nil,Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _, 'fun', Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type,_,range,Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _, map, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({op,_,_Op,T1,T2}, Acc) ->
   extract_user_types([T1, T2], Acc);
extract_user_types({op,_,_Op,T}, Acc) ->
   extract_user_types(T, Acc);
extract_user_types({type, _,record,[_Name | Args]}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({remote_type,_,[_ModuleName,_TypeName,Args]}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _, tuple, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({type, _,union, Args}, Acc) ->
   extract_user_types(Args, Acc);
extract_user_types({user_type, _, Name, Args}, Acc) ->
   %% append user type and continue iterating through lists in case of other
   %% user-defined types to be added
   Acc1 = sets:add_element({Name, length(Args)}, Acc),
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
   %% Handles built-in types such as 'list'.
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
                       (_, _) -> false
                   end, [], Args),

      true ?= is_list(Vars),
      Arity ?= length(Vars),
      update_slogan(State, {Name, Vars, Arity})
   else
      _ ->
         State
   end.



%% NOTE: Terminal elements for the documentation, such as `-type`, `-opaque`, `-callback`,
%%       and functions always need to reset the state when they finish, so that new
%%       new AST items start with a clean slate.
extract_documentation_from_type([{attribute, Anno, TypeOrOpaque, {TypeName, TypeDef, TypeArgs}}=_AST | T],
                      #docs{exported_types=ExpTypes, meta=Meta}=State)
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque ->
   Args = fun_to_varargs(TypeArgs),
   State0 = add_user_types([TypeArgs, TypeDef], State),
   case sets:is_element({TypeName, length(Args)}, ExpTypes) of
      true ->
         State1 = State0#docs{ meta = Meta#{exported := true}},
         State2 = gen_doc_with_slogan({type, Anno, TypeName, length(Args), Args}, State1),
         extract_documentation0(T, State2);
      false ->
         %% the reason is that a public function that returns a private type,
         %% needs to show this private type in the documentation as a type that is
         %% internal, but we need to show its structure.
         %% another function in this module will deal with removing private types
         %% that are not used in public functions.
         State1 = State0#docs{ meta = Meta#{exported := false}},
         State2 = gen_doc_with_slogan({type, Anno, TypeName, length(Args), Args}, State1),
         extract_documentation0(T, State2)
   end.


extract_documentation_from_doc([{attribute, _Anno, doc, Meta0}=_AST | T], State) when is_map(Meta0) ->
    State1 = update_meta(State, Meta0),
    extract_documentation0(T, update_doc(State1, none));
extract_documentation_from_doc([{attribute, _Anno, doc, DocStatus}=_AST | T], State)
  when DocStatus =:= hidden; DocStatus =:= false ->
    State1 = update_docstatus(State, hidden),
    extract_documentation0(T, State1);
extract_documentation_from_doc([{attribute, Anno, doc, Doc}=_AST | T], State) when is_list(Doc)  ->
   extract_documentation0(T, update_doc(State, {Doc, Anno}));
extract_documentation_from_doc([{attribute, Anno, doc, Doc}=_AST | T], State) when is_binary(Doc) ->
   extract_documentation0(T, update_doc(State, {unicode:characters_to_list(Doc), Anno})).

extract_hidden_types(AST) ->
   extract_hidden_types(AST, {none, sets:new()}).
extract_hidden_types([], {_, HiddenTypes}) ->
   HiddenTypes;
extract_hidden_types([{attribute, _Anno, doc, DocStatus} | T], {_TypeState, HiddenTypes}) when
   DocStatus =:= hidden; DocStatus =:= false ->
   extract_hidden_types(T, {hidden, HiddenTypes});
extract_hidden_types([{attribute, _Anno, doc, _} | T], {TypeState, HiddenTypes}) ->
   extract_hidden_types(T, {TypeState, HiddenTypes});
extract_hidden_types([{attribute, _Anno, TypeOrOpaque, {Name, _Type, Args}} | T], {hidden, HiddenTypes})
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque ->
   extract_hidden_types(T, {none, sets:add_element({Name, length(Args)}, HiddenTypes)});
extract_hidden_types([{attribute, _Anno, TypeOrOpaque, _} | T], {none, HiddenTypes})
  when TypeOrOpaque =:= type; TypeOrOpaque =:= opaque ->
   extract_hidden_types(T, {none, HiddenTypes});
extract_hidden_types([_ | T], {_, HiddenTypes}) ->
   extract_hidden_types(T, {none, HiddenTypes}).


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
                        none -> {none, Anno0};
                        {Doc, Anno} -> {Doc, Anno}
                    end,

    State1 = case sets:is_element({F, A}, ExpFuns) orelse State#docs.export_all of
               true ->
                  {Slogan1, DocsWithoutSlogan} =
                     %% First we check if there is a doc prototype
                     case extract_slogan(Doc1, F, A) of
                        undefined -> {io_lib:format("~p/~p",[F,A]), Doc1};
                        SloganDocs -> SloganDocs
                     end,
                  Slogan = pick_slogan(Slogan1, State, F, A),
                  AttrBody = {function, F, A},
                  gen_doc(Anno1, AttrBody, Slogan, DocsWithoutSlogan, State);
               false ->
                  reset_state(State)
            end,
   extract_documentation0(T, State1).

pick_slogan(Slogan, #docs{slogan = none}, _F, _A) ->
   Slogan;
pick_slogan(_Slogan, #docs{slogan = {FunName, Vars, A}}, FunName, A) ->
   VarString = create_variables(Vars),
   unicode:characters_to_list(io_lib:format("~p(~s)", [FunName, VarString]));
pick_slogan(Slogan, _, _F, _A) ->
   Slogan.

create_variables([]) ->
   "";
create_variables([Var | Vars]) ->
   CommaSeparatedVars = create_variables(Vars, []),
   atom_to_list(Var) ++ CommaSeparatedVars.

create_variables([], Acc) ->
   reverse(Acc);
create_variables([Var | Vars], Acc) ->
   create_variables(Vars, atom_to_list(Var) ++ " ," ++ Acc).


extract_documentation_from_cb([{attribute, Anno, callback, {{CB, A}, [Fun]=Form}}=AST0 | T], State) ->
   State1 = extract_slogan_from_spec(AST0, State),

   %% adds user types as part of possible types that need to be exported
   State2 = add_user_types(Form, State1),


   Args = fun_to_varargs(Fun),
   State3 = gen_doc_with_slogan({callback, Anno, CB, A, Args}, State2),
   extract_documentation0(T, State3);
extract_documentation_from_cb([{attribute, Anno0, callback, {{CB, A}, Form}}=AST0 | T], #docs{doc = Doc0}=State) ->
   %% Multi-clause callback. Do not create a slogan from the callback args.

   State1 = extract_slogan_from_spec(AST0, State),

   %% adds user types as part of possible types that need to be exported
   State2 = add_user_types(Form, State1),

   {Doc1, Anno1} = case Doc0 of
                        none -> {none, Anno0};
                        {Doc, Anno} -> {Doc, Anno}
                    end,

   {Slogan1, DocsWithoutSlogan} =
      %% First we check if there is a doc prototype
      case extract_slogan(Doc1, CB, A) of
         undefined -> {io_lib:format("~p/~p",[CB,A]), Doc1};
         SloganDocs -> SloganDocs
      end,
   Slogan = pick_slogan(Slogan1, State2, CB, A),


   AttrBody = {callback, CB, A},
   State3 = gen_doc(Anno1, AttrBody, Slogan, DocsWithoutSlogan, State2),
   extract_documentation0(T, State3).


%% Generates documentation
-spec gen_doc(Anno, AttrBody, Slogan, Docs, State) -> Response when
      Anno      :: erl_anno:anno(),
      AttrBody  :: {function | type | callback, term(), integer()},
      Slogan    :: unicode:chardata(),
      Docs      :: none | hidden | unicode:chardata(),
      State     :: internal_docs(),
      Response  :: internal_docs().
gen_doc(Anno, {Attr, _F, _A}=AttrBody, Slogan, DocWithoutSlogan, #docs{meta = Meta}=State)
  when DocWithoutSlogan =:= none; DocWithoutSlogan =:= hidden ->
   Result = {AttrBody, Anno, [unicode:characters_to_binary(Slogan)], DocWithoutSlogan, Meta},
   State1 = update_user_defined_types(State),
   reset_state(update_ast(Attr, State1, Result));
gen_doc(Anno, {Attr, _F, _A}=AttrBody, Slogan, Docs, #docs{meta = Meta}=State) ->
   Result = {AttrBody, Anno, [unicode:characters_to_binary(Slogan)],
             #{ <<"en">> => unicode:characters_to_binary(string:trim(Docs)) }, Meta},
   State1 = update_user_defined_types(State),
   reset_state(update_ast(Attr, State1, Result)).

%% Generates the documentation inferring the slogan from the documentation.
gen_doc_with_slogan({Attr, _Anno0, F, A, Args}=AST, #docs{doc = Doc0, doc_status = DocStatus}=State) ->
    {Doc1, Anno1} = fetch_doc_and_anno(DocStatus, Doc0, AST),
    {Slogan1, DocsWithoutSlogan} =
        case extract_slogan(Doc1, F, A) of
            undefined ->
              case all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args) of
                 true ->
                    {extract_slogan_from_args(F, Args), Doc1};
                 false ->
                    {io_lib:format("~p/~p",[F,A]), Doc1}
              end;
            SloganDocs ->
                SloganDocs
        end,
   Slogan = pick_slogan(Slogan1, State, F, A),
   AttrBody = {Attr, F, A},
   gen_doc(Anno1, AttrBody, Slogan, DocsWithoutSlogan, State).

fetch_doc_and_anno(DocStatus, Doc, {_Attr, Anno0, _F, _A, _Args}) ->
    case {DocStatus, Doc} of
        {hidden, _} -> {hidden, Anno0};
        {_, none} -> {none, Anno0};
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

extract_slogan(Doc, _F, _A) when Doc =:= none; Doc =:= hidden ->
    undefined;
extract_slogan(Doc, F, A) ->
    maybe
        [MaybeSlogan | Rest] = string:split(Doc, "\n"),
        {ok, Toks, _} ?= erl_scan:string(unicode:characters_to_list([MaybeSlogan,"."])),
        {ok, [{call,_,{atom,_,F},Args}]} ?= erl_parse:parse_exprs(Toks),
        A ?= length(Args),
        {MaybeSlogan, Rest}
    else
        _ -> undefined
    end.

extract_slogan_from_args(F, Args) ->
    io_lib:format("~p(~ts)",[F, join(", ",[string:trim(atom_to_list(Arg),leading,"_") || {var, _, Arg} <- Args])]).
