-module(mailbox_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../include/mailbox.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([mailbox_lint_tests/0]).
-export([missing_mailbox_association/1, missing_mailbox_function/1, missing_mailbox_function2/1,
         missing_mailbox_bound_function/1, unsupported_mailbox_recv_pattern/1,
         check_program_without_mailbox/1, check_program_without_mailbox_with_attr/1]).
-export([check_stdlib_lists/1, check_stdlib_gen_server/1, check_stdlib_gen_statem/1]).

-define(MAILBOX_LINT_GROUP, mailbox_lint).

all() ->
    [{group, ?MAILBOX_LINT_GROUP}].

%% Configuration of the max. time per test case
suite() ->
    [{timetrap, {seconds, 1}}].

groups() ->
    [{?MAILBOX_LINT_GROUP, [sequence], mailbox_lint_tests()}].

init_per_suite(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Check = fun (Program) ->
                      {ok, Tokens, _} = erl_scan:string(Program),
                      case Tokens of
                          [{'-',_},
                           {atom,_,module},
                           {'(',_},
                           {atom,_,ModuleName},
                           {')',_} | _] when is_atom(ModuleName) ->

                              Path = PrivDir ++ atom_to_list(ModuleName) ++ ".erl",
                              _ = file:write_file(Path, Program),
                              mailbox:check(Path, #option{})
                      end
              end,
    GetPath = fun (Program) ->
                      {ok, Tokens, _} = erl_scan:string(Program),
                      case Tokens of
                          [{'-',_},
                           {atom,_,module},
                           {'(',_},
                           {atom,_,ModuleName},
                           {')',_} | _] when is_atom(ModuleName) ->

                              Path = PrivDir ++ atom_to_list(ModuleName) ++ ".erl",
                              _ = file:write_file(Path, Program),
                              Path
                      end
              end,
    [{check, Check}, {get_path, GetPath} | Config].

end_per_suite(Config) ->
    Config.

mailbox_lint_tests() ->
    [
     missing_mailbox_association,
     missing_mailbox_function,
     missing_mailbox_function2,
     missing_mailbox_bound_function,
     unsupported_mailbox_recv_pattern,
     check_program_without_mailbox,
     check_program_without_mailbox_with_attr,
     check_stdlib_lists,
     check_stdlib_gen_server,
     check_stdlib_gen_statem
    ].

missing_mailbox_association(Config) ->
    Test = """
            -module(test).
            -new([]).
            """,
    Check = proplists:get_value(check, Config),
    Result = Check(Test),

    Expected = [{?ERR_MISSING_MAILBOX_NAME,{new,[]}}],
    Expected = Result,
    ok.

missing_mailbox_function(Config) ->
    Test = """
            -module(test).
            -new({foo_mailbox, []}).
            """,
    Check = proplists:get_value(check, Config),
    Result = Check(Test),

    Expected = [{?ERR_MISSING_MAILBOX_EMPTY_BOUND,{foo_mailbox, []}}],
    Expected = Result,
    ok.

missing_mailbox_function2(Config) ->
    Test = """
            -module(test).
            -new({foo_mailbox, [{foo, 0}]}).
            """,
    Check = proplists:get_value(check, Config),
    Result = Check(Test),

    Expected = [{?ERR_MISSING_BOUND_FUNCTION,{foo_mailbox, [{foo, 0}]}}],
    Expected = Result,
    ok.

missing_mailbox_bound_function(Config) ->
    Test = """
            -module(test).
            -new({foo_mailbox, [blah/0]}).
            """,
    Check = proplists:get_value(check, Config),
    Result = Check(Test),

    Expected = [{?ERR_MISSING_BOUND_FUNCTION,{foo_mailbox, [{blah, 0}]}}],
    Expected = Result,
    ok.

unsupported_mailbox_recv_pattern(Config) ->
    Test = """
            -module(test).
            -new({mailbox, [blah/0]}).
            blah() ->
              Client = self(),
              Pid = spawn(fun () -> Client ! {foo, bar} end),
              _ = receive {foo, bar, X} when is_atom(X) -> ok end,
              _ = receive {foo, bar} -> ok after _ -> ok end,
              ok.
            """,
    Check = proplists:get_value(check, Config),
    Result = Check(Test),
    Expected = [{?ERR_RECV_PATTERN_UNSUPPORTED,
                 {error,{'receive',6,{tuple,6,[{atom,6,foo},{atom,6,bar},{var,6,'X'}]}}}},
                {?ERR_RECV_PATTERN_UNSUPPORTED, {error, {'receive',7,{tuple,7,[{atom,7,foo},{atom,7,bar}]}}}}],
    Expected = Result,
    ok.

check_program_without_mailbox(Config) ->
    Test = """
            -module(test).
            -define(FOO, bar).
            blah() ->
              ok.
            """,
    Check = proplists:get_value(check, Config),
    Result = Check(Test),

    Expected = [],
    Expected = Result,
    ok.

check_program_without_mailbox_with_attr(Config) ->
    Test = """
            -module(test).
            -doc "blah doc".
            blah() ->
              ok.
            """,
    GetPath = proplists:get_value(get_path, Config),
    Path = GetPath(Test),
    Result = mailbox:check(Path, #option{mode = debug}),

    Expected = [],
    Expected = Result,
    ok.

check_stdlib_lists(Config) ->
    TestBin = setup_check_stdlib_file("lists.erl"),
    Check = proplists:get_value(check, Config),
    Result = Check(erlang:binary_to_list(TestBin)),

    true = lists:all(fun ({ErrorTag, _}) -> err_unsupported_qualifiers == ErrorTag end, Result),
    ok.


check_stdlib_gen_server(Config) ->
    TestBin = setup_check_stdlib_file("gen_server.erl"),
    Check = proplists:get_value(check, Config),
    Result = Check(erlang:binary_to_list(TestBin)),

    Expected = [err_unsupported_catch_clauses,
                err_unsupported_recv_pattern,
                err_unsupported_qualifiers],
    true = lists:all(fun ({ErrorTag, _}) -> lists:member(ErrorTag, Expected) end, Result),
    ok.

check_stdlib_gen_statem(Config) ->
    TestBin = setup_check_stdlib_file("gen_statem.erl"),
    Check = proplists:get_value(check, Config),
    Result = Check(erlang:binary_to_list(TestBin)),

    Expected = [err_unsupported_catch_clauses,
                err_unsupported_qualifiers],
    true = lists:all(fun ({ErrorTag, _}) -> lists:member(ErrorTag, Expected) end, Result),
    ok.

-spec setup_check_stdlib_file(File :: string()) -> binary().
setup_check_stdlib_file(File) when is_list(File) ->
    DataDir = filename:join([os:getenv("ERL_TOP"), "lib", "stdlib", "src"]),
    ok = filelib:ensure_path(DataDir),
    FilePath = filename:join(DataDir, File),
    io:format("FilePath: ~p~n", [FilePath]),
    true = filelib:is_file(FilePath),

    {ok, TestBin} = file:read_file(FilePath),
    TestBin.
