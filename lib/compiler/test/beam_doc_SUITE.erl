
-module(beam_doc_SUITE).
-export([all/0, groups/0, init_per_group/2, end_per_group/2, singleton_moduledoc/1, singleton_doc/1,
         docmodule_with_doc_attributes/1, hide_moduledoc/1, docformat/1,
         singleton_docformat/1, singleton_meta/1, slogan/1,
         types_and_opaques/1, callback/1, hide_moduledoc2/1,
         private_types/1, export_all/1, equiv/1, spec/1, doc_with_file/1, doc_with_file_error/1,
         all_string_formats/1, docs_from_ast/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/eep48.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(get_name(), atom_to_list(?FUNCTION_NAME)).

all() ->
    [{group, documentation_generation_tests}, doc_with_file].

groups() ->
    [{documentation_generation_tests, [parallel], documentation_generation_tests()}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

documentation_generation_tests() ->
    [singleton_moduledoc,
     singleton_doc,
     docmodule_with_doc_attributes,
     hide_moduledoc,
     hide_moduledoc2,
     docformat,
     singleton_docformat,
     singleton_meta,
     slogan,
     types_and_opaques,
     callback,
     private_types,
     export_all,
     equiv,
     spec,
     doc_with_file_error,
     all_string_formats,
     docs_from_ast
    ].

singleton_moduledoc(Conf) ->
    ModuleName = "singletonmoduledoc",
    {ok, ModName} = compile_file(Conf, ModuleName),

    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    {ok, {docs_v1, _,_, Mime,ModuleDoc, _,_}} = code:get_doc(ModName),
    ok.

singleton_doc(Conf) ->
    ModuleName = "singletondoc",
    {ok, ModName} = compile_file(Conf, ModuleName),
    Mime = <<"text/markdown">>,
    Doc = #{<<"en">> => <<"Doc test module">>},
    FooDoc = #{<<"en">> => <<"Tests multi-clauses">>},
    {ok, {docs_v1, 1,_, Mime, none, _,
          [{{function, foo,1},_, [<<"foo(ok)">>], FooDoc, _},
           {{function, main,0},_, [<<"main()">>], Doc, _}]}} = code:get_doc(ModName),
    ok.

docmodule_with_doc_attributes(Conf) ->
    ModuleName = "docmodule_with_doc_attributes",
    {ok, ModName} = compile_file(Conf, ModuleName),
    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Doc = #{<<"en">> => <<"Doc test module">>},
    FileDocs =  #{<<"en">> => <<"# README\n\nThis is a test">>},
    {ok, #docs_v1{ anno = ModuleAnno,
                   beam_language = erlang,
                   format = Mime,
                   module_doc = ModuleDoc,
                   metadata = #{},
                   docs = Docs
                 }} = code:get_doc(ModName),

    
    [{{function,no_docs_multi,1},NoDocsMultiAnno,[<<"no_docs_multi/1">>],none,#{}},
     {{function,with_file_docs,0},FileDocsAnno, [<<"with_file_docs()">>],FileDocs,#{}},
     {{function,no_docs,0},NoDocsAnno, [<<"no_docs()">>],none,#{}},
     {{function,ok,0}, OkAnno, [<<"ok()">>],none,#{authors := "Someone"}},
     {{function, main,_},MainAnno, _, Doc, _}] = Docs,
    
    ?assertEqual(5, erl_anno:line(ModuleAnno)),
    ?assertEqual(10, erl_anno:line(MainAnno)),
    ?assertEqual(18, erl_anno:line(OkAnno)),
    ?assertEqual(21, erl_anno:line(NoDocsAnno)),
    ?assertEqual(1, erl_anno:line(FileDocsAnno)),
    ?assertEqual("README", filename:basename(erl_anno:file(FileDocsAnno))),
    ?assertEqual(28, erl_anno:line(NoDocsMultiAnno)),

    ok.

hide_moduledoc(Conf) ->
    {ok, ModName} = compile_file(Conf, "hide_moduledoc"),
    {ok, {docs_v1, _,_, _Mime, hidden, _,
          [{{function, main, 0}, _, [<<"main()">>],
            #{ <<"en">> := <<"Doc test module">> }, #{}}]}} = code:get_doc(ModName),
    ok.

%% TODO: crashes
hide_moduledoc2(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _Mime, hidden, _,
          [{{function, main, 0}, _, [<<"main()">>], hidden, #{}}]}} = code:get_doc(ModName),
    ok.

docformat(Conf) ->
    {ok, ModName} = compile_file(Conf, "docformat"),
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Meta = #{format => "text/asciidoc",
             deprecated => "Use something else",
             otp_doc_vsn => {1,0,0},
             since => "1.0"},
    Doc = #{<<"en">> => <<"Doc test module">>},
    {ok, {docs_v1, _,_, <<"text/asciidoc">>, ModuleDoc, Meta,
          [{{function, main,_},_, _, Doc, _}]}} = code:get_doc(ModName),
    ok.

singleton_docformat(Conf) ->
    {ok, ModName} = compile_file(Conf, "singleton_docformat"),
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Meta = #{format => <<"text/asciidoc">>,
             deprecated => "Use something else",
             otp_doc_vsn => {1,0,0},
             since => "1.0"},
    Doc = #{<<"en">> => <<"Doc test module\n\nMore info here">>},
    FunMeta = #{ authors => [<<"Beep Bop">>], equiv => <<"main/3">> },
    {ok, {docs_v1, _,erlang, <<"text/asciidoc">>, ModuleDoc, Meta,
          [{{function, main,0},_, [<<"main()">>], Doc, FunMeta}]}} = code:get_doc(ModName),
    ok.

singleton_meta(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Meta = #{ authors => [<<"Beep Bop">>], equiv => <<"main/3">>},
    DocMain1 = #{<<"en">> => <<"Returns always ok.">>},
    {ok, {docs_v1, _,erlang, <<"text/markdown">>, none, _,
          [{{function, main1,0},_, [<<"main1()">>], DocMain1, #{equiv := <<"main(_)">>}},
           {{function, main,0},_, [<<"main()">>], none, Meta}]}}
        = code:get_doc(ModName),
    ok.

slogan(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Doc = #{<<"en">> => <<"Returns ok.">>},
    BarDoc = #{ <<"en">> => <<"foo()\nNot a slogan since foo =/= bar">> },
    NoSloganDoc = #{ <<"en">> => <<"Not a slogan\n\nTests slogans in multi-clause">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function,spec_multiclause_slogan_ignored,1},_,[<<"spec_multiclause_slogan_ignored(X)">>],none,#{}},
           {{function, spec_no_doc_slogan, 1}, _, [<<"spec_no_doc_slogan(Y)">>], none, #{}},
           {{function, no_doc_slogan, 1}, _, [<<"no_doc_slogan(X)">>], none, #{}},
           {{function, spec_slogan, 1}, _, [<<"spec_slogan(X)">>], _, #{}},
           {{function, no_slogan,1},_,[<<"no_slogan/1">>], NoSloganDoc, #{}},
           {{function, bar,0},_,[<<"bar()">>], BarDoc, #{}},
           {{function, main,2},_,[<<"main(Foo, Bar)">>], Doc, #{}},
           {{function, main,1},_,[<<"main(Foo)">>], Doc, #{}}
          ]}
    } = code:get_doc(ModName),
    ok.

types_and_opaques(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    TypeDoc = #{<<"en">> => <<"Represents the name of a person.">>},
    GenericsDoc = #{<<"en">> => <<"Tests generics">>},
    OpaqueDoc = #{<<"en">> =>
                      <<"Represents the name of a person that cannot be named.">>},
    MaybeOpaqueDoc = #{<<"en">> => <<"mmaybe(X) ::= nothing | X.\n\nRepresents a maybe type.">>},
    MaybeMeta = #{ authors => "Someone else", exported => true },
    NaturalNumberMeta = #{since => "1.0", equiv => <<"non_neg_integer/0">>, exported => true},
    {ok, {docs_v1, _,_, _, none, _,
          [{{type,my_other_private_type,0},MyOtherPrivateType,
              [<<"my_other_private_type()">>],none,#{exported := false}},
           {{type,my_private_type,0},MyPrivateType,
              [<<"my_private_type()">>],none,#{exported := false}},
           {{type,mymap,0},MyMap,[<<"mymap()">>],none,#{exported := false}},
           {{type,state_enter,0},StateEnter,
               [<<"state_enter()">>],none,#{exported := false}},
           {{type,callback_mode,0},CallbackMode,
               [<<"callback_mode()">>],none,#{exported := false}},
           {{type,callback_mode_result,0},CallbackResult,
               [<<"callback_mode_result()">>],none,#{exported := true}},
           {{type,encoding_func,0},_,[<<"encoding_func()">>],none,#{exported := false}},
           {{type,three,0},_,[<<"three()">>],none,#{exported := false}},
           {{type,two,0},_,[<<"two()">>],none,#{exported := false}},
           {{type,one,0},_,[<<"one()">>],none,#{exported := false}},
           {{type,hidden,0},_,[<<"hidden()">>],hidden,#{exported := true}},
           {{type,hidden_false,0},_,[<<"hidden_false()">>],hidden,
            #{exported := true, authors := "Someone else"}},
           {{type, mmaybe,1},_,[<<"mmaybe(X)">>], MaybeOpaqueDoc, MaybeMeta},
           {{type, unnamed,0},_,[<<"unnamed()">>], OpaqueDoc,
            #{equiv := <<"non_neg_integer()">>, exported := true}},
           {{type, param,1},_,[<<"param(X)">>], GenericsDoc,
            #{equiv := <<"madeup()">>, exported := true}},
           {{type, natural_number,0},_,[<<"natural_number()">>], none, NaturalNumberMeta},
           {{type, name,1},_,[<<"name(_)">>], TypeDoc, #{exported := true}},
           {{function,ignore_type_from_hidden_fun,0},_,[<<"ignore_type_from_hidden_fun()">>],hidden,#{}},
           {{function,map_fun,0},_,[<<"map_fun()">>],none,#{}},
           {{function,private_encoding_func,2},_,[<<"private_encoding_func/2">>],none,#{}},
           {{function,foo,0},_,[<<"foo()">>],none,#{}}
          ]}} = code:get_doc(ModName),

    ?assertEqual(102, erl_anno:line(MyOtherPrivateType)),
    ?assertEqual(101, erl_anno:line(MyPrivateType)),
    ?assertEqual(98, erl_anno:line(MyMap)),
    ?assertEqual(95, erl_anno:line(StateEnter)),
    ?assertEqual(94, erl_anno:line(CallbackMode)),
    ?assertEqual(92, erl_anno:line(CallbackResult)),

    ok.

callback(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Doc = #{<<"en">> => <<"Callback fn that always returns ok.">>},
    ImpCallback = #{<<"en">> => <<"This is a test">>},
    FunctionDoc = #{<<"en">> => <<"all_ok()\n\nCalls all_ok/0">>},
    ChangeOrder = #{<<"en">> => <<"Test changing order">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{callback,multi,1},_,[<<"multi(Argument)">>],
            #{ <<"en">> := <<"A multiclause callback with slogan docs">> },#{}},
           {{callback,ann,1},_,[<<"ann/1">>],none,#{}},
           {{callback,param,1},_,[<<"param(X)">>],none,#{}},
           {{callback, change_order,0},_,[<<"change_order()">>], ChangeOrder,
            #{equiv := <<"ok()">>}},
           {{callback, all_ok,0},_,[<<"all_ok()">>], Doc, #{}},
           {{function, main2,0},_,[<<"main2()">>], #{<<"en">> := <<"Second main">>},
            #{equiv := <<"main()">>}},
           {{function, main,0},_,[<<"main()">>], FunctionDoc, #{}},
           {{function, all_ok,0},_, [<<"all_ok()">>],ImpCallback,
            #{equiv := <<"ok/0">>}}
          ]}} = code:get_doc(ModName),
    ok.

private_types(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    Code = code:get_doc(ModName),
    {ok, {docs_v1, _,_, _, none, _,
          [{{type,private,0}, {26,2}, [<<"private()">>], hidden, #{exported := false}},
           {{type,hidden_export_t,0},_,[<<"hidden_export_t()">>],hidden,#{exported := true}},
           {{type,public_t,0},_, [<<"public_t()">>], none,#{ exported := true}},

           {{type,private_t,0},_, [<<"private_t()">>], none,#{ exported := false}},
           {{function,hidden_type_exposed,0},{29,1},[<<"hidden_type_exposed()">>],none,#{}},
           {{function,hidden,0},_,[<<"hidden()">>],hidden,#{}},
           {{function,bar,0},_,[<<"bar()">>],none,#{}}
           ]}} = Code,
    ok.


export_all(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    ImpCallback = #{<<"en">> => <<"This is a test">>},
    FunctionDoc = #{<<"en">> => <<"all_ok()\n\nCalls all_ok/0">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main2,0},_,[<<"main2()">>], #{<<"en">> := <<"Second main">>},
            #{equiv := <<"main()">>}},
           {{function, main,0},_,[<<"main()">>], FunctionDoc, #{}},
           {{function, all_ok,0},_, [<<"all_ok()">>],ImpCallback,
            #{equiv := <<"ok/0">>}}
          ]}} = code:get_doc(ModName),
    ok.

equiv(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main, 2},_,[<<"main(A, B)">>], none,
            #{ }},
            {{function, main, 1},_,[<<"main(A)">>], none,
             #{ equiv := <<"main(A, 1)">> }}
          ]}} = code:get_doc(ModName),
    ok.

spec(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _, none, _,
          [{{type,no,0},_,[<<"no()">>],none,#{exported := false}},
           {{type,yes,0},_,[<<"yes()">>],none,#{exported := false}},
           {{callback,me,1},_,[<<"me/1">>],none,#{}},
           {{function,baz,1},_,[<<"baz(X)">>],none,#{}},
           {{function,foo,1},_,[<<"foo(X)">>],none,#{}}]}} = code:get_doc(ModName),
    ok.

doc_with_file(Conf) ->
    ModuleName = ?get_name(),
    {ok, Cwd} = file:get_cwd(),
    try
        ok = file:set_cwd(proplists:get_value(data_dir, Conf)),
        {ok, ModName} = compile_file(Conf, ModuleName, [{i, "./folder"}]),
        {ok, {docs_v1, ModuleAnno,_, _, #{<<"en">> := <<"# README\n\nThis is a test">>}, _,
              [{{type,bar,1},_,[<<"bar(X)">>],none,#{exported := false}},
               {{type,foo,1},_,[<<"foo(X)">>],none,#{exported := true}},
               {{type,private_type_exported,0},_,[<<"private_type_exported()">>],
                #{<<"en">> := <<"# TYPES\n\nTest">>}, #{exported := false}},
               {{function,main2,1},Main2Anno,[<<"main2/1">>],
                #{<<"en">> := <<"# File\n\ntesting fetching docs from other folders">>}, #{}},
               {{function,main,1},_,[<<"main(Var)">>],
                #{<<"en">> := <<"# Fun\n\nTest importing function">>},#{}}]}} = code:get_doc(ModName),

        ?assertEqual(1, erl_anno:line(ModuleAnno)),
        ?assertEqual(1, erl_anno:line(Main2Anno)),
        ?assertEqual("./folder/FILE", erl_anno:file(Main2Anno)),
        ok
    after
        ok = file:set_cwd(Cwd)
    end.

doc_with_file_error(Conf) ->
    ModuleName = ?get_name(),
    {error, Errors, []} = compile_file(Conf, ModuleName, [return]),
    [[Mod:format_error(Error) || {_Loc, Mod, Error} <- Errs] || {_File, Errs} <- Errors],
    error = compile_file(Conf, ModuleName, [report]),
    ok.

all_string_formats(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = compile_file(Conf, ModuleName),

    {ok, {docs_v1, _ModuleAnno,_, _, #{<<"en">> := <<"Moduledoc test module">>}, _,
              [
               {{function,six,0},_,_, #{<<"en">> := <<"all_string_formats-all_string_formats">>}, #{}},
               {{function,five,0},_,_, #{<<"en">> := <<"all_string_formats-Doc module">>}, #{}},
               {{function,four,0},_,_, #{<<"en">> := <<"Doc test mödule"/utf8>>}, #{}},
               {{function,three,0},_,_, #{<<"en">> := <<"Doctestmodule">>}, #{}},
               {{function,two,0},_,_, #{<<"en">> := <<"Doc test module">>}, #{}},
               {{function,one,0},_,_, #{<<"en">> := <<"Doc test module">>}, #{}}
              ]}} = code:get_doc(ModName),
    ok.

docs_from_ast(_Conf) ->
    Code = """
      -module(test).
      -moduledoc "moduledoc".
      -export([main/0]).
      -doc "main".
      main() -> ok.
      """,

    {ok, test, BeamCode} = compile:forms(scan_and_parse(Code),[beam_docs, debug_info]),
    {ok, {test, [{documentation, Docs }]}} = beam_lib:chunks(BeamCode, [documentation]),

    ?assertMatch(
       #docs_v1{ module_doc = #{ <<"en">> := <<"moduledoc">> },
                 anno = 2,
                 docs = [{{function,main,0}, 4, _, #{ <<"en">> := <<"main">> }, _}]},
       Docs),

    check_no_doc_attributes(BeamCode),

    {ok, test, BeamCodeWSource} = compile:forms(scan_and_parse(Code),[beam_docs, debug_info, {source, "test.erl"}]),
    {ok, {test, [{documentation, DocsWSource }]}} = beam_lib:chunks(BeamCodeWSource, [documentation]),

    ?assertMatch(
       #docs_v1{ module_doc = #{ <<"en">> := <<"moduledoc">> },
                 anno = 2,
                 docs = [{{function,main,0}, [{file,"test.erl"},{location,4}],
                          _, #{ <<"en">> := <<"main">> }, _}]},
       DocsWSource),
    check_no_doc_attributes(BeamCodeWSource),
    ok.

scan_and_parse(Code) ->
    {ok, Toks, _} = erl_scan:string(Code),
    parse(Toks).

parse([]) -> [];
parse(Toks) ->
    {Form, [Dot | Rest]} = lists:splitwith(fun(E) -> element(1,E) =/= dot end, Toks),
    {ok, F} = erl_parse:parse_form(Form ++ [Dot]),
    [F | parse(Rest)].

compile_file(Conf, ModuleName) ->
    compile_file(Conf, ModuleName, []).
compile_file(Conf, ModuleName, ExtraOpts) ->
    ErlModName = ModuleName ++ ".erl",
    Filename = filename:join(proplists:get_value(data_dir, Conf), ErlModName),
    io:format("Compiling: ~ts~n",[Filename]),
    case compile:file(Filename, [debug_info, beam_docs | ExtraOpts]) of
        {ok, ModName} ->

            check_no_doc_attributes(code:which(ModName)),

            {ok, ModName};
        Else ->
            Else
    end.


%% Verify that all doc and moduledoc attributes are stripped from debug_info
check_no_doc_attributes(Mod) ->
    {ok, {_ModName,
          [{debug_info,
            {debug_info_v1,erl_abstract_code,
             {AST, Opts}}}]}} = beam_lib:chunks(Mod, [debug_info]),
    false = lists:search(
              fun(E) ->
                      element(1,E) == attribute
                          andalso
                            (element(3,E) == doc orelse element(3,E) == moduledoc)
              end, AST),
    true = lists:member(beam_docs, Opts),
    ok.
