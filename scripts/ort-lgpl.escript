#!/home/erfeafr/Code/otp2/bin/escript
%% -*- erlang -*-

-mode(compile).

%% lgpl() ->
%%     [ "lib/edoc/include/edoc_doclet.hrl",
%%       "lib/edoc/src/edoc_data.erl",
%%       "lib/edoc/src/edoc_doclet_chunks.erl",
%%       "lib/edoc/src/edoc_doclet.erl",
%%       "lib/edoc/src/edoc.erl",
%%       "lib/edoc/src/edoc_extract.erl",
%%       "lib/edoc/src/edoc.hrl",
%%       "lib/edoc/src/edoc_layout_chunks.erl",
%%       "lib/edoc/src/edoc_layout.erl",
%%       "lib/edoc/src/edoc_lib.erl",
%%       "lib/edoc/src/edoc_macros.erl",
%%       "lib/edoc/src/edoc_parser.yrl",
%%       "lib/edoc/src/edoc_refs.erl",
%%       "lib/edoc/src/edoc_report.erl",
%%       "lib/edoc/src/edoc_run.erl",
%%       "lib/edoc/src/edoc_tags.erl",
%%       "lib/edoc/src/edoc_types.erl",
%%       "lib/edoc/src/edoc_types.hrl",
%%       "lib/edoc/src/edoc_wiki.erl",
%%       "lib/eunit/include/eunit.hrl",
%%       "lib/eunit/src/eunit_autoexport.erl",
%%       "lib/eunit/src/eunit_data.erl",
%%       "lib/eunit/src/eunit.erl",
%%       "lib/eunit/src/eunit_internal.hrl",
%%       "lib/eunit/src/eunit_lib.erl",
%%       "lib/eunit/src/eunit_listener.erl",
%%       "lib/eunit/src/eunit_proc.erl",
%%       "lib/eunit/src/eunit_serial.erl",
%%       "lib/eunit/src/eunit_server.erl",
%%       "lib/eunit/src/eunit_striptests.erl",
%%       "lib/eunit/src/eunit_surefire.erl",
%%       "lib/eunit/src/eunit_test.erl",
%%       "lib/eunit/src/eunit_tests.erl",
%%       "lib/eunit/src/eunit_tty.erl",
%%       "lib/eunit/test/eunit_test_listener.erl",
%%       "lib/syntax_tools/src/epp_dodger.erl",
%%       "lib/syntax_tools/src/erl_comment_scan.erl",
%%       "lib/syntax_tools/src/erl_prettypr.erl",
%%       "lib/syntax_tools/src/erl_recomment.erl",
%%       "lib/syntax_tools/src/erl_syntax.erl",
%%       "lib/syntax_tools/src/erl_syntax_lib.erl",
%%       "lib/syntax_tools/src/merl.erl",
%%       "lib/syntax_tools/src/merl_tests.erl",
%%       "lib/syntax_tools/src/merl_transform.erl",
%%       "lib/syntax_tools/src/prettypr.erl",
%%       ".ort/license-classifications.yml",
%%       ".ort.yml",
%%       "system/COPYRIGHT"].

main(_) ->
    {ok, Bin} = file:read_file("scan-resultB.json"),
    #{<<"scanner">> := #{<<"scan_results">> := [ScanResults]},
      <<"repository">> :=
          #{<<"config">> :=
                #{<<"excludes">> := #{<<"paths">> := Excludes},
                  <<"curations">> := #{<<"license_findings">> := Findings}}}} = json:decode(Bin),
    #{<<"summary">> := #{<<"licenses">> := Licenses}} = ScanResults,
    ExcludeRegexes = convert_excludes(Excludes),
    R = lists:foldl(fun (License, Acc) ->
                            group_by_license(ExcludeRegexes, Findings, License, Acc)
                    end, #{}, Licenses),
    io:format("Licenses: ~p~n", [R]).

group_by_license(ExcludeRegexes, Curations, License, Acc) ->
    #{<<"license">> := LicenseName, <<"location">> := Location, <<"score">> := _Score} = License,
    #{<<"path">> := Path, <<"start_line">> := _StartLine, <<"end_line">> := _EndLine} = Location,
    Excluded = exclude_path(Path, ExcludeRegexes),
    LicenseName1 = curated_path_license(LicenseName, Path, Curations),
    case Excluded of
        true ->
            Acc;
        false ->
            case maps:get(LicenseName1, Acc, []) of
                [] ->
                    Acc#{LicenseName1 => [Path]};
                Ls ->
                    Ls1 = case lists:search(fun(X) -> X == Path end, Ls) of
                              false -> [Path | Ls];
                              _ -> Ls
                          end,
                    Acc#{LicenseName1 => Ls1}
            end
    end.

convert_excludes(Excludes) ->
    lists:map(fun (#{<<"pattern">> := Pattern}) ->
                      Pattern1 = re:replace(Pattern, <<"\\.">>, <<"\\\\.">>, [global, {return, binary}]),
                      re:replace(Pattern1, <<"\\*\\*">>, <<".*">>, [global, {return, binary}])
              end, Excludes).

exclude_path(Path, ExcludeRegexes) ->
    lists:any(fun (Regex) ->
                      case re:run(Path, Regex) of
                          {match, _} -> true;
                          _ -> false
                      end
              end, ExcludeRegexes).

curated_path_license(Name, _Path, []) -> Name;
curated_path_license(_Name, Path, [#{<<"path">> := Path}=Cur | _Curations]) ->
    maps:get(<<"concluded_license">>, Cur);
curated_path_license(Name, Path, [_Cur | Curations]) ->
    curated_path_license(Name, Path, Curations).
