-module(md_to_html_SUITE).

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([convert_erlang_html/1, convert_unknown_format/1]).
-export([non_existing_moduledoc/1,hidden_moduledoc/1,existing_moduledoc/1]).
-export([h1_test/1, h2_test/1, h3_test/1, h4_test/1, h5_test/1, h6_test/1]).
-export([single_line_quote_test/1, ignore_three_spaces_before_quote/1,
         multiple_line_quote_test/1, paragraph_in_between_test/1]).
-export([paragraph_after_heading_test/1, quote_before_and_after_paragraph_test/1]).

-define(ERLANG_HTML, <<"application/erlang+html">>).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/eep48.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [{group, different_format_generator},
     {group, module_generator},
     {group, header_generator},
     {group, quote_generator},
     {group, paragraph_generator}
    ].

groups() ->
    [{different_format_generator, [parallel], different_format_conversion_tests()},
     {module_generator, [sequence], moduledoc_tests()},
     {header_generator, [parallel], header_tests()},
     {quote_generator, [parallel], quote_tests()},
     {paragraph_generator, [parallel], paragraph_tests()}
    ].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.


different_format_conversion_tests() ->
    [convert_erlang_html,
     convert_unknown_format].

moduledoc_tests() ->
    [non_existing_moduledoc,
     hidden_moduledoc,
     existing_moduledoc].

header_tests() ->
    [ h1_test,
      h2_test,
      h3_test,
      h4_test,
      h5_test,
      h6_test
    ].

quote_tests() ->
    [ single_line_quote_test,
      ignore_three_spaces_before_quote,
      multiple_line_quote_test,
      paragraph_in_between_test
    ].

paragraph_tests() ->
    [ paragraph_after_heading_test,
      quote_before_and_after_paragraph_test
    ].

convert_erlang_html(_Conf) ->
    Doc = #{<<"en">> => [{p, [], [<<"Test">>]}]},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}}],

    Docs = create_eep48(erlang, ?ERLANG_HTML, none, #{}, Functions),
    Docs = beam_doc:markdown_to_shelldoc(Docs),
    ok = shell_docs:validate(Docs),
    ok.

convert_unknown_format(_Conf) ->
    Doc = #{<<"en">> => <<"<text>Here</text>">>},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}}],

    Docs = create_eep48(erlang, <<"xml">>, Doc, #{},Functions),
    Docs = beam_doc:markdown_to_shelldoc(Docs),
    ok.

non_existing_moduledoc(_Conf) ->
    Docs = create_eep48(erlang, <<"text/markdown">>, none, #{}, []),
    _ = compile(Docs),
    ok.

hidden_moduledoc(_Conf) ->
    Docs = create_eep48(erlang, <<"text/markdown">>, hidden, #{}, []),
    _ = compile(Docs),
    ok.

existing_moduledoc(_Conf) ->
    Docs = create_eep48_doc(<<"# Here">>),
    HtmlDocs = compile(Docs),
    #{<<"en">> := HtmlModDoc} = extract_moduledoc(HtmlDocs),
    [{h1, [], [<<"Here">>]}] = HtmlModDoc,
    ok.

h1_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [header(1, <<"Here">>)]},
    Expected = extract_moduledoc(HtmlDocs),
    [{{function, foo, 0}, [], [], Expected, #{}}] = extract_doc(HtmlDocs),
    ok.

h2_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here\n## Header 2">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [{h1, [], [<<"Here">>]}, {h2, [], [<<"Header 2">>]}]},
    Expected = extract_moduledoc(HtmlDocs),
    [{{function, foo, 0}, [], [], Expected, #{}}] = extract_doc(HtmlDocs),
    ok.

h3_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here\n### Header 3">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [{h1, [], [<<"Here">>]}, {h3, [], [<<"Header 3">>]}]},
    Expected = extract_moduledoc(HtmlDocs),
    [{{function, foo, 0}, [], [], Expected, #{}}] = extract_doc(HtmlDocs),
    ok.

h4_test(_Conf) ->
    Docs = create_eep48_doc(<<"### Here\n#### Header 4">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [{h3, [], [<<"Here">>]}, {h4, [], [<<"Header 4">>]}]},
    Expected = extract_moduledoc(HtmlDocs),
    [{{function, foo, 0}, [], [], Expected, #{}}] = extract_doc(HtmlDocs),
    ok.

h5_test(_Conf) ->
    Doc = #{<<"en">> => <<"### Here\n#### Header 4\n##### H5">>},
    DocH5 = #{<<"en">> => <<"##### H5">>},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}},
                 {{function, bar, 0}, [], [], DocH5, #{}}],
    Docs = create_eep48(erlang, <<"text/markdown">>, Doc, #{}, Functions),

    HtmlDocs = compile(Docs),
    ExpectedH3 = #{<<"en">> => [ {h3, [], [<<"Here">>]},
                                 {h4, [], [<<"Header 4">>]},
                                 {h5, [], [<<"H5">>]}]},
    ExpectedH5 = #{<<"en">> => [ {h5, [], [<<"H5">>]} ]},
    ExpectedH3 = extract_moduledoc(HtmlDocs),
    [ E1, E2 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], ExpectedH3, #{}} = E1,
    {{function, bar, 0}, [], [], ExpectedH5, #{}} = E2,
    ok.

h6_test(_Conf) ->
    Doc = #{<<"en">> => <<"### Here\n#### Header 4\n##### H5">>},
    DocH6 = #{<<"en">> => <<"###### H6\n## H2">>},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}},
                 {{function, bar, 0}, [], [], DocH6, #{}}],
    Docs = create_eep48(erlang, <<"text/markdown">>, Doc, #{}, Functions),

    HtmlDocs = compile(Docs),
    ExpectedH3 = #{<<"en">> => [ {h3, [], [<<"Here">>]},
                                 {h4, [], [<<"Header 4">>]},
                                 {h5, [], [<<"H5">>]}]},
    ExpectedH6 = #{<<"en">> => [ {h6, [], [<<"H6">>]},
                                 {h2, [], [<<"H2">>]} ]},
    ExpectedH3 = extract_moduledoc(HtmlDocs),
    [ E1, E2 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], ExpectedH3, #{}} = E1,
    {{function, bar, 0}, [], [], ExpectedH6, #{}} = E2,
    ok.


single_line_quote_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here\n> This is a quote">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> =>
                     [ header(1, <<"Here">>),
                       quote(<<"This is a quote">>)]},
    ModuleDoc = extract_moduledoc(HtmlDocs),
    Expected = ModuleDoc,
    [ E1 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], Expected, #{}} = E1,
    ok.

ignore_three_spaces_before_quote(_Conf) ->
    Docs = create_eep48_doc(<<"   > # Here">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [quote(<<"# Here">>)]},
    Expected = extract_moduledoc(HtmlDocs),
    [ E1 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], Expected, #{}} = E1,
    ok.

multiple_line_quote_test(_Conf) ->
    Docs = create_eep48_doc(<<"> # Here\n> This is a quote">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> =>
                     [ quote(<<"# Here\nThis is a quote">>)]},
    Expected = extract_moduledoc(HtmlDocs),
    [ E1 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], Expected, #{}} = E1,
    ok.

paragraph_in_between_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Header 1\nThis is text\n> A quote\n## Header 2\nBody content">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> =>
                     [ header(1, <<"Header 1">>),
                       p(<<"This is text">>),
                       quote(<<"A quote">>),
                       header(2, <<"Header 2">>),
                       p(<<"Body content">>)]},
    Expected = extract_moduledoc(HtmlDocs),
    [ E1 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], Expected, #{}} = E1,
    ok.

paragraph_after_heading_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Header 1\nThis is text\n\nBody content">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> =>
                     [ header(1, <<"Header 1">>),
                       p(<<"This is text">>),
                       p(<<"Body content">>)]},
    Expected = extract_moduledoc(HtmlDocs),
    [ E1 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], Expected, #{}} = E1,
    ok.

quote_before_and_after_paragraph_test(_Conf) ->
    Docs = create_eep48_doc(<<"> Quote 1\nThis is text\n> Quote 2\nBody content">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> =>
                     [ quote(<<"Quote 1">>),
                       p(<<"This is text">>),
                       quote(<<"Quote 2">>),
                       p(<<"Body content">>)]},
    Expected = extract_moduledoc(HtmlDocs),
    [ E1 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], Expected, #{}} = E1,
    ok.

header(Level, Text) when is_integer(Level), is_binary(Text) ->
    HeadingLevel = integer_to_list(Level),
    HeadingLevelAtom = list_to_existing_atom("h" ++ HeadingLevel),
    {HeadingLevelAtom, [], [Text]}.

quote(X) ->
    {pre,[],[{code,[],[X]}]}.

p(X) ->
    {p, [], [X]}.

-spec create_eep48(Language, Mime, ModuleDoc, Metadata, Docs) -> #docs_v1{} when
      Language  :: atom(),
      Mime      :: binary(),
      ModuleDoc :: #{DocLanguage := DocValue} | none | hidden,
      Metadata  :: map(),
      Docs      :: [{{Kind, Name, Arity},
                     Anno :: erl_anno:anno(),
                     Signature :: [binary()],
                     Doc :: #{DocLanguage := DocValue} | none | hidden,
                     Metadata :: map()
                    }],
      Kind      :: function | type | callback,
      Name      :: atom(),
      Arity     :: non_neg_integer(),
      DocLanguage :: binary(),
      DocValue :: binary() | term().
create_eep48(Language, Mime, ModuleDoc, Metadata, Docs) ->
    #docs_v1{anno = [],
             beam_language = Language,
             format = Mime,
             module_doc = ModuleDoc,
             metadata = maps:merge(#{ otp_doc_vsn => ?CURR_DOC_VERSION }, Metadata),
             docs = Docs}.

extract_moduledoc(Docs) ->
    Docs#docs_v1.module_doc.

extract_doc(Docs) ->
    Docs#docs_v1.docs.

extract_format(Docs) ->
    Docs#docs_v1.format.

create_eep48_doc(Doc) when is_binary(Doc) ->
    create_eep48_doc(Doc, <<"text/markdown">>).

create_eep48_doc(Doc, Format) when is_binary(Doc) ->
    Docs = #{<<"en">> => Doc},
    Functions = [{{function, foo, 0}, [], [], Docs, #{}}],
    create_eep48(erlang, Format, Docs, #{}, Functions).

%% expected(X) when is_list(X)->
%%     #{<<"en">> => X};
%% expected(X) when is_tuple(X) ->
%%     expected([X]).


compile(Docs) ->
    HtmlDocs = beam_doc:markdown_to_shelldoc(Docs),
    ok = shell_docs:validate(HtmlDocs),
    ?NATIVE_FORMAT = extract_format(HtmlDocs),
    HtmlDocs.
