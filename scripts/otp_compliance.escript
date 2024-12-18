#!/usr/bin/env escript
%% -*- erlang -*-

%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2024 Erlang/OTP and its contributors

-define(default_classified_result, "scan-result-classified.json").
-define(default_scan_result, "scan-result.json").
-define(diff_classified_result, "scan-result-diff.json").


-mode(compile).

%%
%% Commands
%%
%% sbom
%%
%%    otp-info: given an oss-review-toolkit (ORT) scan result and a
%%              source SBOM, it populates the fields that ORT can't
%%              in Unmanaged projects.
%%
%% compliance   useful for CI/CD compliance checks.
%%
%%    detect:   given a scan-result from ORT, it detects files without license
%%              and writes them into disk.
%%
%%    check:    given a recent scan-result from ORT (possibly from PR), and an
%%              existing file with known files without licenses (from prev. commit),
%%              calculate if new files without licenses have been added to the repo.
%%
%% explore
%%
%%    classify: takes as input a scan of ort and returns a json file containing
%%              as keys the licenses and as values the files under those licenses.
%%
%%    diff:     performs a diff of existing classification file against
%%              other classification files. this is useful to guarantee that
%%              files that had license X had not unexpectedly been reported differently.
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
           #{"sbom" =>
                 #{ help => """
                            Contains useful commands to fix an ORT generated source SBOM.

                            """,
                   commands =>
                        #{"otp-info" =>
                              #{ help =>
                                     """
                                     Adds information missing in ORT's Erlang/OTP source SBOM
                                       - Add homepage
                                       - Fixes license of `*.beam` files
                                       - Fixes project name

                                     """,
                                 arguments => [ sbom_option(),
                                                input_option(?default_scan_result) ],
                                 handler => fun sbom_otp/1
                               }}},
             "explore" =>
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
                                 handler => fun diff/1}
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

sbom_option() ->
    #{name => sbom_file,
      type => binary,
      default => "bom.spdx.json",
      long => "-sbom-file"}.

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

%% TODO: missing step: yq -p yaml -o json ../otp/bom.spdx.yml > ../otp/bom.spdx.json
%%       i.e., convert yml to json
sbom_otp(#{sbom_file  := SbomFile}=Input) ->
    Sbom = decode(SbomFile),
    Licenses = path_to_license(Input),
    Copyrights = path_to_copyright(Input, Licenses),

    Fixes = [{fun fix_name/2, ~"Erlang/OTP"},
             {fun fix_supplier/2, ~"Organization: Ericsson AB"},
             {fun fix_download_location/2, ~"https://github.com/erlang/otp/releases"},
             {fun fix_beam_licenses/2, {Licenses, Copyrights}} ],
    Spdx = lists:foldl(fun ({Fun, Data}, Acc) -> Fun(Data, Acc) end, Sbom, Fixes),
    file:write_file(SbomFile, json:encode(Spdx)).

fix_name(Name, Sbom) ->
    Sbom#{ ~"name" := Name}.

fix_supplier(Name, #{~"packages" := [ Packages ] }=Sbom) ->
    Packages1 = Packages#{~"supplier" := Name },
    Sbom#{~"packages" := [Packages1]}.

fix_download_location(Url, #{~"packages" := [ Packages ] }=Sbom) ->
    Packages1 = Packages#{~"downloadLocation" := Url },
    Sbom#{~"packages" := [ Packages1 ]}.

%% re-populate licenses to .beam files from their .erl files
%% e.g., the lists.beam file should have the same license as lists.erl
fix_beam_licenses(LicensesAndCopyrights,
                  #{ ~"packages" := [Package],
                     ~"files"   := Files}=Sbom) ->
    Package1 = Package#{ ~"homepage" := ~"https://www.erlang.org",
                         ~"licenseConcluded" := ~"Apache-2.0"},
    Files1= lists:map(
              fun (SPDX) ->
                      try
                          case SPDX of
                              #{~"fileName" := <<"bootstrap/lib/compiler/ebin/", Filename/binary>>} ->
                                  [File, _] = binary:split(Filename, ~".beam"),
                                  fix_beam_spdx_license(<<"lib/compiler/src/", File/binary, ".erl">>, LicensesAndCopyrights, SPDX);
                              #{~"fileName" := <<"bootstrap/lib/kernel/ebin/",Filename/binary>>} ->
                                  [File, _] = binary:split(Filename, ~".beam"),
                                  fix_beam_spdx_license(<<"lib/kernel/src/",  File/binary, ".erl">>, LicensesAndCopyrights, SPDX);
                              #{~"fileName" := <<"bootstrap/lib/stdlib/ebin/",Filename/binary>>} ->
                                  [File, _] = binary:split(Filename, ~".beam"),
                                  fix_beam_spdx_license(<<"lib/stdlib/src/", File/binary, ".erl">>, LicensesAndCopyrights, SPDX);
                              #{~"fileName" := <<"erts/preloaded/ebin/",Filename/binary>>} ->
                                  [File, _] = binary:split(Filename, ~".beam"),
                                  fix_beam_spdx_license(<<"erts/preloaded/src/", File/binary, ".erl">>, LicensesAndCopyrights, SPDX);
                              #{~"fileName" := <<"erts/emulator/internal_doc/",Filename/binary>>} ->
                                  [File, _] = binary:split(Filename, ~".md"),
                                  fix_beam_spdx_license(<<"erts/preloaded/src/", File/binary, ".md">>, LicensesAndCopyrights, SPDX);
                              _ ->
                                  fix_spdx_license(SPDX)
                          end
                      catch
                          _:_ ->
                              fix_spdx_license(SPDX)
                      end
              end, Files),
    Sbom#{ ~"files" := Files1, ~"packages" := [Package1]}.

%% fixes spdx license of beam files
fix_beam_spdx_license(Path, {Licenses, Copyrights}, SPDX) ->
    License = maps:get(Path, Licenses, ~"NONE"),
    Copyright = maps:get(Path, Copyrights, ~"NONE"),
    SPDX#{ ~"copyrightText" := Copyright,
           ~"licenseConcluded" := License }.

%% fixes spdx license of non-beam files
fix_spdx_license(#{~"licenseInfoInFiles" := [License]}=SPDX) ->
    SPDX#{ ~"licenseConcluded" := License };
fix_spdx_license(SPDX) ->
    SPDX.

%% Given an input file, returns a mapping of
%% #{filepath => license} for each file path towards its license.
path_to_license(Input) ->
    ClassifyInput = Input#{ exclude => true, curations => true},
    ClassifyLicense = group_by_licenses(ClassifyInput),
    maps:fold(fun (K, Vs, Acc) ->
                      maps:merge(maps:from_keys(Vs, K), Acc)
              end, #{}, ClassifyLicense).

path_to_copyright(Input, _Licenses) ->
    ClassifyInput = Input#{ exclude => true},
    ClassifyCopyright = group_by_copyrights(ClassifyInput),
    maps:fold(fun (K, Vs, Acc) ->
                      maps:merge(maps:from_keys(Vs, K), Acc)
              end, #{}, ClassifyCopyright).

classify(#{output_file := Output}=Input) ->
    R = group_by_licenses(Input),
    ok = file:write_file(Output, json:encode(R)).

group_by_licenses(#{input_file := Filename,
                    exclude := ApplyExclude,
                    curations := ApplyCuration}) ->
    Json = decode(Filename),
    Excludes = apply_excludes(Json, ApplyExclude),
    Curations = apply_curations(Json, ApplyCuration),

    Licenses = licenses(scan_results(Json)),
    lists:foldl(fun (License, Acc) ->
                            group_by_license(Excludes, Curations, License, Acc)
                    end, #{}, Licenses).

group_by_copyrights(#{input_file := Filename,
                      exclude := ApplyExclude}) ->
    Json = decode(Filename),
    Excludes = apply_excludes(Json, ApplyExclude),
    Copyrights = copyrights(scan_results(Json)),
    lists:foldl(fun (Copyright, Acc) ->
                            group_by_copyright(Excludes, Copyright, Acc)
                    end, #{}, Copyrights).


apply_excludes(Json, ApplyExclude) ->
    Excludes = excludes(Json),
    onlyif([], ApplyExclude, fun () -> convert_excludes(Excludes) end).

apply_curations(Json, ApplyCuration) ->
    Curations = curations(Json),
    onlyif([], ApplyCuration, fun () -> Curations end).

diff(#{input_file := InputFile, base_file := BaseFile, output_file := Output}) ->
    Input = decode(InputFile),
    Base = decode(BaseFile),
    KeyList = maps:keys(Input) ++ maps:keys(Base),
    KeySet = sets:from_list(KeyList),
    Data = sets:fold(fun(Key, Acc) -> set_difference(Key, Input, Base, Acc) end, #{}, KeySet),
    file:write_file(Output, json:encode(Data)).

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
    try
        #{<<"repository">> :=
              #{<<"config">> :=
                    #{<<"excludes">> := #{<<"paths">> := Excludes}}}} = Input,
        Excludes
    catch
        _:_ ->
            []
    end.


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

copyrights(Input) ->
    #{<<"summary">> := #{<<"copyrights">> := Copyrights}} = Input,
    Copyrights.


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
    #{<<"license">> := LicenseName, <<"location">> := Location} = License,
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

group_by_copyright(ExcludeRegexes, Copyright, Acc) ->
    #{<<"statement">> := CopyrightSt, <<"location">> := Location} = Copyright,
    #{<<"path">> := Path, <<"start_line">> := _StartLine, <<"end_line">> := _EndLine} = Location,
    maybe
        false ?= exclude_path(Path, ExcludeRegexes),
        case maps:get(CopyrightSt, Acc, []) of
            [] ->
                Acc#{CopyrightSt => [Path]};
            Ls ->
                Ls1 = case lists:search(fun(X) -> X == Path end, Ls) of
                          false -> [Path | Ls];
                          _ -> Ls
                      end,
                Acc#{CopyrightSt => Ls1}
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
