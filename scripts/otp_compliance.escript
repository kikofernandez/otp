#!/home/erfeafr/Code/otp2/bin/escript
%% -*- erlang -*-

-define(default_classified_result, "scan-result-classified.json").
-define(default_scan_result, "scan-result.json").
-define(diff_classified_result, "scan-result-diff.json").


-mode(compile).

%%
%% These commands generate a classification of files per license.
%% This output is not shown when using `ort`.
%%
%% classify:
%%   takes as input a scan of ort and returns a json file containing
%%   as keys the licenses and as values the files under those licenses.
%%
%% diff:
%%   performs a diff of existing classification file against
%%   other classification files. this is useful to guarantee that
%%   files that had license X had not unexpectedly been reported differently.
%%
%% detect:
%%   given a scan-result from ORT, it detects files without license
%%   and writes them into disk.
%%
%% check:
%%   given a recent scan-result from ORT (possibly from PR), and an
%%   existing file with known files without licenses (from prev. commit),
%%   calculate if new files without licenses have been added to the repo.
%%
%%
%% USE OF COMMANDS
%%
%% The commands `classify` and `diff` are useful for exploring the licenses.
%% ORT does not report in an easy way which files have been attached to which licenses,
%% unless one generates a report. At the time, we cannot generate an SBOM,
%% so we are in the dark.
%%
%% The commands `detect` and `check` can be used in CI/CD to
%% prevent entering new files with unknown license. In the normal case,
%% the `detect` command only needs to be issued once in the repo.
%% Once we keep track of this file, the command is not needed anymore,
%% as the list of files with no license should not grow, and only
%% the `check` command should be executed in the CI/CD.
%%
%%

main(Args) ->
    argparse:run(Args, cli(), #{progname => otp_compliance}).

cli() ->
    #{ commands =>
           #{"explore" =>
                 #{  help => """
                            Explore license data.
                            Useful to figure out the mapping files-to-licenses.

                            """,
                    commands =>
                        #{"classify" =>
                              #{ help =>
                                     """
                                     Classify files by their license group.
                                       - Input file expects a scan-result from ORT.
                                       - Output file shows mapping between licenses and files.
                                         The output file can be fed to the `explore diff` command.

                                     """,
                                 arguments => [ input_option(?default_scan_result),
                                                output_option(?default_classified_result),
                                                apply_excludes(),
                                                apply_curations() ],
                                 handler => fun classify/1},
                          "diff" =>
                              #{ help =>
                                     """
                                     Compare against previous license results.
                                       - Input file should be the output of the `classify` command for input and base files.
                                       - Output returns a summary of additions and deletions per license.

                                     """,
                                 arguments => [ input_option(?default_classified_result),
                                                base_file(),
                                                output_option(?diff_classified_result) ],
                                 handler => fun diff/1},

                          "git-author" =>
                              #{help => "Track author of the commit",
                               handler => fun git_author/1},

                          "detect-beam-license" =>
                              #{help => """
                                        Detects the license of a beam file, and updates the scan result.
                                          - Input file expects classified results from `classify` command.
                                          - Base file expects output file from `no_license` command.
                                          - Output file expects scan-result file from ORT.
                                        """,
                                arguments => [input_option(?default_classified_result),
                                              base_file(),
                                              output_option(?default_scan_result)],
                                handler => fun detect_beam_license/1}
                         }
                  },
             "compliance" =>
                 #{ help => """
                            Commands to enforce compliance policy towards unlicensed files.
                            """,
                    commands =>
                        #{"detect" =>
                              #{ help =>
                                     """
                                     Detects unlicensed files.
                                     - Input file expects a scan-result from ORT.
                                     - Output file is a list of files without license.
                                       The output file can be fed to the `compliance check` command.

                                     """,
                                 arguments => [ input_option(?default_scan_result),
                                                output_option(),
                                                apply_excludes() ],
                                 handler => fun detect_no_license/1},
                          "check" =>
                              #{ help =>
                                     """
                                     Checks that no new unlicensed files have been added.
                                     - Input file expects scan-result from ORT.
                                     - Base file expects output file from `no_license` command.

                                     """,
                                 arguments => [ input_option(?default_scan_result),
                                                base_file(),
                                                apply_excludes(),
                                                output_option() ],
                                 handler => fun check_no_license/1}}}}}.

%%
%% Options
%%
input_option(Default) ->
    #{name => input_file,
      type => binary,
      default => Default,
      long => "-input-file"}.

output_option(Default) ->
    #{name => output_file,
      type => binary,
      default => Default,
      long => "-output-file"}.

output_option() ->
    #{name => output_file,
      type => binary,
      required => true,
      long => "-output-file"}.

apply_excludes() ->
    #{name => exclude,
      type => boolean,
      short => $e,
      default => true,
      long => "-apply-excludes"}.

apply_curations() ->
    #{name => curations,
      type => boolean,
      short => $c,
      default => true,
      long => "-apply-curations"}.

base_file() ->
    #{name => base_file,
      type => binary,
      long => "-base-file"}.

%%
%% Commands
%%

git_author(_) ->
    File = "no_license.json",
    Unlicense = decode(File),
    Result = lists:map(fun (Path) ->
                      %% https://stackoverflow.com/questions/27028486/how-to-execute-system-command-in-erlang-and-get-results-using-oscmd-1
                      Command = "git log --reverse --format=format:\"%h -- %an-%as\" -- " ++ binary_to_list(Path) ++ " | head -n1",
                      Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
                      [Path, get_data(Port, [])]
              end, Unlicense),
    ok = file:write_file("no_license_with_author.json", Result).

get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
            {'EXIT',  Port,  _} ->
                ok
        after 1 ->              % force context switch
                ok
        end,
        lists:flatten(Sofar)
    end.

classify(#{input_file := Filename,
           output_file := Output,
           exclude := ApplyExclude,
           curations := ApplyCuration}) ->
    Json = decode(Filename),
    Excludes = excludes(Json),
    Curations = curations(Json),
    Licenses = licenses(scan_results(Json)),

    Excludes1 = onlyif([], ApplyExclude, fun () -> convert_excludes(Excludes) end),
    Curations1 = onlyif([], ApplyCuration, fun () -> Curations end),
    R = lists:foldl(fun (License, Acc) ->
                            group_by_license(Excludes1, Curations1, License, Acc)
                    end, #{}, Licenses),
    ok = file:write_file(Output, json:encode(R)).

diff(#{input_file := InputFile, base_file := BaseFile, output_file := Output}) ->
    Input = decode(InputFile),
    Base = decode(BaseFile),
    KeyList = maps:keys(Input) ++ maps:keys(Base),
    KeySet = sets:from_list(KeyList),
    Data = sets:fold(fun(Key, Acc) -> set_difference(Key, Input, Base, Acc) end, #{}, KeySet),
    file:write_file(Output, json:encode(Data)).

detect_beam_license(#{input_file := ClassifyFile,
                      base_file := NoLicenseFile,
                      output_file := _OutputFile}) ->
    Input = decode(ClassifyFile),
    NoLicense = decode(NoLicenseFile),   
    
    %% Create DB from filename => license
    DB = maps:fold(fun (License, Files, Acc) ->                      
                           FileNames = lists:map(fun extract_module_name/1, Files),
                           M = maps:from_keys(FileNames, License),
                           maps:merge(M, Acc)
                   end, #{}, Input),

    %% Returns {detected licenses, new unlicensed}
    {DB1, Unknown} = 
        lists:foldl(fun (BinName, {LicenseMap, Unlicensed})
                          when is_map(LicenseMap), is_list(Unlicensed) ->
                            Name = extract_module_name(BinName),
                            Name1 = lists:flatten(string:replace(Name, ".beam", ".erl")),
                            case maps:get(Name1, DB, badkey) of
                                badkey ->
                                    {LicenseMap, [BinName | Unlicensed]};
                                License ->
                                    {LicenseMap#{BinName => License}, Unlicensed}
                            end
                    end, {#{}, []}, NoLicense),
    %% updates unlicensed list of files
    %% file:write_file(NoLicenseFile, json:encode(Unknown)),
    %% TODO: updates the classify files
    update_classify_file(Input, DB1),
    %% TODO: updates the scan files
    %% io:format("Result: ~p~nCount: ~p~n", [{length(maps:keys(DB1)), maps:keys(DB1)}, length(Unknown)]),
    ok.

-spec update_classify_file(JSON :: map(), #{Path :: binary() => License :: binary()}) -> ok.
update_classify_file(Json, DB) ->
    NewMap = maps:fold(fun (Path, License, Acc) ->
                               case maps:get(License, Acc, badkey) of
                                   badkey ->
                                       Acc#{License => [Path]};
                                   Value ->
                                       Acc#{License := [Path | Value]}
                               end
                       end, #{}, DB),
    Result = maps:merge_with(fun (_Key, Val1, Val2) ->
                                     Val1 ++ Val2
                             end, Json, NewMap),
    ok.

extract_module_name(Bin) when is_binary(Bin) ->
    S = binary_to_list(Bin),    
    string:reverse(hd(string:split(string:reverse(S), "/"))).

detect_no_license(#{input_file := InputFile,
                    output_file := OutputFile,
                    exclude := ApplyExcludes}) ->
    Input = decode(InputFile),
    SortedResult = compute_unlicense_files(Input, ApplyExcludes),
    file:write_file(OutputFile, json:encode(SortedResult)).

compute_unlicense_files(Input, ApplyExcludes) ->
    Licenses = licenses(scan_results(Input)),

    PathsWithLicense =
        lists:foldl(fun (#{<<"location">> := #{<<"path">> := Path}}, Acc) ->
                            sets:add_element(Path, Acc)
                    end, sets:new(), Licenses),

    %% Get all files, incluiding those without license
    Files = files_from_scanner(Input),
    AllPaths =
        lists:foldl(fun (#{<<"path">> := Path}, Acc) ->
                            sets:add_element(Path, Acc)
                    end, sets:new(), Files),

    %% Paths without license
    PathsWithoutLicense = sets:to_list(sets:subtract(AllPaths, PathsWithLicense)),

    %% Excluded files that should be ignored
    Excludes = excludes(Input),
    ExcludeRegex = onlyif([], ApplyExcludes, fun () -> convert_excludes(Excludes) end),
    Result = lists:foldl(fun(Path, Acc) ->
                                 case exclude_path(Path, ExcludeRegex) of
                                     true ->
                                         Acc;
                                     false ->
                                         [Path | Acc]
                                 end
                         end, [], PathsWithoutLicense),
    lists:sort(Result).

check_no_license(#{input_file := InputFile,
                   base_file := BaseFile,
                   exclude := ApplyExcludes,
                   output_file := OutputFile}) ->
    UnlicenseNew = compute_unlicense_files(decode(InputFile), ApplyExcludes),
    Unlicense = decode(BaseFile),
    UnlicenseSet = sets:from_list(Unlicense),
    UnlicenseNewSet =  sets:from_list(UnlicenseNew),
    Result = sets:to_list(sets:subtract(UnlicenseNewSet, UnlicenseSet)),
    file:write_file(OutputFile, json:encode(Result)).


%%
%% Helper functions
%%

excludes(Input) ->
    #{<<"repository">> :=
          #{<<"config">> :=
                #{<<"excludes">> := #{<<"paths">> := Excludes}}}} = Input,
    Excludes.

curations(Input) ->
    #{<<"repository">> :=
          #{<<"config">> :=
                #{<<"curations">> := #{<<"license_findings">> := Curations}}}} = Input,
    Curations.

scan_results(Input) ->
    #{<<"scanner">> := #{<<"scan_results">> := [ScanResults]}} = Input,
    ScanResults.

licenses(Input) ->
    #{<<"summary">> := #{<<"licenses">> := Licenses}} = Input,
    Licenses.

files_from_scanner(Input) ->
    #{<<"scanner">> := #{<<"files">> := [#{<<"files">> := Files}]}} = Input,
    Files.

set_difference(Key, Input, Base, Acc) ->
    InputValues = sets:from_list(maps:get(Key, Input, [])),
    BaseValues = sets:from_list(maps:get(Key, Base, [])),
    Additions = sets:subtract(InputValues, BaseValues),
    Deletions = sets:subtract(BaseValues, InputValues),
    Acc#{Key => #{addition => sets:to_list(Additions), deletions => sets:to_list(Deletions)}}.

onlyif(_Default, true, Command) -> Command();
onlyif(Default, false, _Command) -> Default.

decode(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    json:decode(Bin).

group_by_license(ExcludeRegexes, Curations, License, Acc) ->
    #{<<"license">> := LicenseName, <<"location">> := Location, <<"score">> := _Score} = License,
    #{<<"path">> := Path, <<"start_line">> := _StartLine, <<"end_line">> := _EndLine} = Location,
    maybe
        false ?= exclude_path(Path, ExcludeRegexes),
        LicenseName1 = curated_path_license(LicenseName, Path, Curations),
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
    else
        _ ->
            Acc
    end.

convert_excludes(Excludes) ->
    lists:map(fun (#{<<"pattern">> := Pattern}) ->
                      Pattern1 = re:replace(Pattern, <<"\\.">>, <<"\\\\.">>, [global, {return, binary}]),
                      re:replace(Pattern1, <<"\\*\\*">>, <<".*">>, [global, {return, binary}])
              end, Excludes).

exclude_path(_Path, []) ->
    false;
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
