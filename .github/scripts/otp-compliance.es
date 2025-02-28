#!/usr/bin/env escript
%% -*- erlang -*-

%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2024 Erlang/OTP and its contributors

-define(default_classified_result, "scan-result-classified.json").
-define(default_scan_result, "scan-result.json").
-define(diff_classified_result, "scan-result-diff.json").
-define(erlang_license, ~"Apache-2.0").
-define(spdxref_project_name, ~"SPDXRef-Project-OTP").
-define(spdx_project_name, ~"Erlang/OTP").
-define(spdx_creators_tooling, ~"Tool: otp_compliance").
-define(spdx_supplier, ~"Organization: Ericsson AB").
-define(spdx_download_location, ~"https://github.com/erlang/otp/releases").
-define(spdx_homepage, ~"https://www.erlang.org").
-define(spdx_version, ~"SPDX-2.2").


%% Add more relations if necessary.
-type spdx_relations() :: #{ 'DOCUMENTATION_OF' => [],
                             'CONTAINS' => [],
                             'TEST_OF' => [],
                             'PACKAGE_OF' => []}.

-record(spdx_package, {'SPDXID'           :: unicode:chardata(),
                       'versionInfo'      :: unicode:chardata(),
                       'name'             :: unicode:chardata(),
                       'copyrightText'    :: unicode:chardata(),
                       'filesAnalyzed'    = false :: boolean(),
                       'hasFiles'         = [] :: [unicode:chardata()],
                       'homepage'         :: unicode:chardata(),
                       'licenseConcluded' :: unicode:chardata(),
                       'licenseDeclared'  :: unicode:chardata(),
                       'licenseInfoFromFiles' = [] :: [unicode:chardata()],
                       'downloadLocation' = ~"https://github.com/erlang/otp/releases" :: unicode:chardata(),
                       'packageVerificationCode' :: #{ 'packageVerificationCodeValue' => unicode:chardata()},
                       'supplier' = ~"Organization: Ericsson AB" :: unicode:chardata(),
                       'relationships' = #{ 'DOCUMENTATION_OF' => [],
                                            'CONTAINS' => [],
                                            'TEST_OF' => [],
                                            'PACKAGE_OF' => []} :: spdx_relations()
                      }).
-type spdx_package() :: #spdx_package{}.

-record(app_info, { description  :: unicode:chardata(),
                    id           :: unicode:chardata(),
                    vsn          :: unicode:chardata(),

                    %% modules can only be included in one app.
                    %% not_loaded indicates a special handling of this module, e.g., erts.
                    modules      :: [atom()] | not_loaded,
                    applications :: [atom()],
                    included_applications :: [atom()],
                    optional_applications :: [atom()] }).

-type app_info() :: #app_info{}.

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
                                                write_to_file_option(),
                                                input_option() ],
                                 handler => fun sbom_otp/1},
                          "test" =>
                              #{ help => "Run unit tests",
                                 arguments => [],
                                 handler => fun test/1}
                         }},
             "explore" =>
                 #{  help => """
                            Explore license data.
                            Useful to figure out the mapping files-to-licenses.

                            """,
                    commands =>
                        #{"classify-license" =>
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
                                 handler => fun classify_license/1},
                          "classify-license-copyright" =>
                              #{ help =>
                                     """
                                     Pair files with their copyright and license.
                                     Depends on a `scan-result.json` and the output of the `classify-license`.

                                     """,
                                 arguments => [ input_option(?default_scan_result),
                                                base_file(?default_classified_result),
                                                output_option() ],
                                 handler => fun classify_path_license_copyright/1},

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
input_option() ->
    #{name => input_file,
      type => binary,
      long => "-input-file"}.


input_option(Default) ->
    (input_option())#{default => Default}.

sbom_option() ->
    #{name => sbom_file,
      type => binary,
      default => "bom.spdx.json",
      long => "-sbom-file"}.

write_to_file_option() ->
    #{name => write_to_file,
      type => binary,
      default => true,
      long => "-write_to_file"}.

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
base_file(DefaultFile) ->
    #{name => base_file,
      type => binary,
      default => DefaultFile,
      long => "-base-file"}.


%%
%% Commands
%%

sbom_otp(#{sbom_file  := SbomFile, write_to_file := Write, input_file := Input}) ->
    Sbom = decode(SbomFile),
    ScanResults = decode(Input),
    {Licenses, Copyrights} = fetch_license_copyrights(ScanResults),
    Spdx = generate_spdx_fixes(Sbom, Licenses, Copyrights),
    case Write of
        true ->
            Output = json:encode(Spdx),
            %% TODO: file:write_file(SbomFile, json:encode(Output)).
            file:write_file("otp.spdx.json", Output);
        false ->
            {ok, Spdx}
    end.

fetch_license_copyrights(Input) ->
    {path_to_license(Input), path_to_copyright(Input)}.

-spec generate_spdx_fixes(Json :: map(), dynamic(), dynamic()) -> Result :: map().
generate_spdx_fixes(Input, Licenses, Copyrights) ->
    Fixes = sbom_fixes(Licenses, Copyrights),
    Spdx = execute_sbom_fixes(Input, Fixes),
    package_by_app(Spdx).

execute_sbom_fixes(Sbom, Fixes) ->
    lists:foldl(fun ({Fun, Data}, Acc) -> Fun(Data, Acc) end, Sbom, Fixes).

sbom_fixes(Licenses, Copyrights) ->
    [{fun fix_project_name/2, ?spdxref_project_name},
     {fun fix_name/2, ?spdx_project_name},
     {fun fix_creators_tooling/2, ?spdx_creators_tooling},
     {fun fix_supplier/2, ?spdx_supplier},
     {fun fix_download_location/2, ?spdx_download_location},
     {fun fix_beam_licenses/2, {Licenses, Copyrights}} ].

fix_project_name(ProjectName, Sbom) ->
    Sbom#{ ~"documentDescribes" := [ ProjectName ]}.

fix_name(Name, Sbom) ->
    Sbom#{ ~"name" => Name}.

fix_creators_tooling(Tool, #{ ~"creationInfo" := #{~"creators" := [ORT | _]}=Creators}=Sbom) ->
    SHA = list_to_binary(string:trim(".sha." ++ os:cmd("git rev-parse HEAD"))),
    Sbom#{~"creationInfo" := Creators#{ ~"creators" := [ORT, <<Tool/binary, SHA/binary>>]}}.

fix_supplier(_Name, #{~"packages" := [ ] }=Sbom) ->
    io:format("[warn] no packages available!~n"),
    Sbom;
fix_supplier(Name, #{~"packages" := [ Packages ] }=Sbom) ->
    Sbom#{~"packages" := [maps:update_with(~"supplier", fun(_) -> Name end, Name, Packages)]}.

fix_download_location(_Url, #{~"packages" := [ ] }=Sbom) ->
    io:format("[warn] no packages available!~n"),
    Sbom;
fix_download_location(Url, #{~"packages" := [ Packages ] }=Sbom) ->
    Packages1 = Packages#{~"downloadLocation" := Url },
    Sbom#{~"packages" := [ Packages1 ]}.

%% re-populate licenses to .beam files from their .erl files
%% e.g., the lists.beam file should have the same license as lists.erl
fix_beam_licenses(_LicensesAndCopyrights, #{ ~"packages" := []}=Sbom) ->
    io:format("[warn] no packages available!~n"),
    Sbom;
fix_beam_licenses(LicensesAndCopyrights,
                  #{ ~"packages" := [Package],
                     ~"files"   := Files}=Sbom) ->
    Package1 = Package#{ ~"homepage" := ~"https://www.erlang.org",
                         ~"licenseConcluded" := ~"Apache-2.0"},
    Files1= lists:map(
              fun (SPDX) ->
                      %% Adds license and copyright from .erl or .hrl file to its .beam equivalent
                      case SPDX of
                          #{~"fileName" := <<"lib/stdlib/uc_spec/", _Filename/binary>>,
                            ~"licenseInfoInFiles" := [License]}  when License =/= ~"NONE", License =/= ~"NOASSERTION"->
                              files_have_no_license(SPDX#{~"licenseConcluded" := License});

                          #{~"fileName" := ~"bootstrap/lib/stdlib/ebin/erl_parse.beam"} ->
                              %% beam file auto-generated from grammar file
                              files_have_no_license(fix_beam_spdx_license(~"lib/stdlib/src/erl_parse.yrl", LicensesAndCopyrights, SPDX));

                          #{~"fileName" := ~"bootstrap/lib/stdlib/ebin/unicode_util.beam"} ->
                              %% follows from otp/lib/stdlib/uc_spec/README-UPDATE.txt
                              files_have_no_license(SPDX#{~"licenseConcluded" := ~"Unicode-3.0 AND Apache-2.0"});

                          #{~"fileName" := <<"erts/emulator/internal_doc/",Filename/binary>>} ->
                              case binary:split(Filename, ~".md") of
                                  [_File, _Ext] ->
                                      SPDX#{~"licenseConcluded" := ~"Apache-2.0"};
                                  _ ->
                                      SPDX
                              end;

                          #{~"fileName" := Filename} ->
                              case bootstrap_mappings(Filename) of
                                  {error, not_beam_file} ->
                                      fix_spdx_license(SPDX);
                                  {Path, Filename1} ->
                                      case binary:split(Filename1, ~".beam") of
                                          [File, _] ->
                                              files_have_no_license(fix_beam_spdx_license(Path, File, LicensesAndCopyrights, SPDX));
                                          _ ->
                                              SPDX
                                      end
                              end
                          end
              end, Files),
    Sbom#{ ~"files" := Files1, ~"packages" := [Package1]}.

bootstrap_mappings(<<"bootstrap/lib/compiler/ebin/", Filename/binary>>) -> {~"lib/compiler/src/", Filename};
bootstrap_mappings(<<"bootstrap/lib/kernel/ebin/",Filename/binary>>) -> {<<"lib/kernel/src/">>, Filename};
bootstrap_mappings(<<"bootstrap/lib/kernel/include/",Filename/binary>>) -> {<<"lib/kernel/include/">>, Filename};
bootstrap_mappings(<<"bootstrap/lib/stdlib/ebin/",Filename/binary>>) -> {<<"lib/stdlib/src/">>, Filename};
bootstrap_mappings(<<"erts/preloaded/ebin/",Filename/binary>>) -> {<<"erts/preloaded/src/">>, Filename};
bootstrap_mappings(_Other) ->
    {error, not_beam_file}.

%% fixes spdx license of beam files
fix_beam_spdx_license(Path, {Licenses, Copyrights}, SPDX) ->
    License = maps:get(Path, Licenses, ~"NOASSERTION"),
    Copyright = maps:get(Path, Copyrights, ~"NOASSERTION"),
    fix_spdx_license(SPDX#{ ~"copyrightText" := Copyright, ~"licenseConcluded" := License }).

fix_beam_spdx_license(Path, File, LicensesAndCopyrights, SPDX) when is_binary(Path),
                                                                    is_binary(File) ->
    Spdx0 = fix_beam_spdx_license(<<Path/binary, File/binary, ".erl">>, LicensesAndCopyrights, SPDX),
    case maps:get(~"licenseConcluded", Spdx0) of
        ~"NOASSERTION" ->
            fix_beam_spdx_license(<<Path/binary, File/binary, ".hrl">>, LicensesAndCopyrights, Spdx0);
        _ ->
            Spdx0
    end.

files_have_no_license(Spdx) ->
    Spdx#{~"licenseInfoInFiles" := [~"NONE"]}.

none_to_noassertion(~"NONE") ->
    ~"NOASSERTION";
none_to_noassertion(X) ->
    X.

%% TODO: check which license curations have actually licenses in files, and which ones
%%       are added to annotate a file with a license. this latter should not be a curation
%%       in ORT, but in this script.
%% fixes spdx license of non-beam files
fix_spdx_license(#{~"licenseInfoInFiles" := [LicenseInFile],
                   ~"licenseConcluded" := License,
                   ~"copyrightText" := C}=SPDX) ->
    License1 = case License of
                   ~"NONE" -> LicenseInFile;
                   ~"NOASSERTION" -> LicenseInFile;
                   Other -> Other
               end,
    SPDX#{ ~"licenseConcluded" := none_to_noassertion(License1),
           ~"copyrightText" := none_to_noassertion(C)
         };
fix_spdx_license(#{~"copyrightText" := C}=SPDX) ->
    SPDX#{ ~"copyrightText" := none_to_noassertion(C)}.

%% Given an input file, returns a mapping of
%% #{filepath => license} for each file path towards its license.
-spec path_to_license(Input :: map()) -> #{Path :: binary() => License :: binary()}.
path_to_license(Input) ->
    match_path_to(Input, fun group_by_licenses/3).

-spec path_to_copyright(Input :: map()) -> #{Path :: binary() => License :: binary()}.
path_to_copyright(Input) ->
    match_path_to(Input, fun group_by_copyrights/3).

-spec match_path_to(Input :: map(), GroupFun :: fun()) -> #{ Path :: binary() => Result :: binary() }.
match_path_to(Json, GroupFun) ->
    Exclude = true,
    Curations = false,
    GroupedResult = GroupFun(Json, Exclude, Curations),
    maps:fold(fun (K, Vs, Acc) ->
                      maps:merge(maps:from_keys(Vs, K), Acc)
              end, #{}, GroupedResult).

%%
%% Explore command
%%
classify_license(#{output_file := Output,
                   input_file := Filename,
                   exclude := ApplyExclude,
                   curations := ApplyCuration}) ->
    Json = decode(Filename),
    R = group_by_licenses(Json, ApplyExclude, ApplyCuration),
    ok = file:write_file(Output, json:encode(R)).

classify_path_license_copyright(#{output_file := Output,
                     input_file := Filename,
                     base_file  := LicenseFileGroup}) ->
    Copyrights = classify_copyright_result(Filename),
    Licenses = expand_license_result(LicenseFileGroup),
    Files = lists:sort(lists:uniq(maps:keys(Copyrights) ++ maps:keys(Licenses))),
    X = lists:foldl(fun (Path, Acc) ->
                          Copyright = maps:get(Path, Copyrights, ~"NONE"),
                          License = maps:get(Path, Licenses, ~"NONE"),
                          Acc#{Path => #{ ~"Copyright" => Copyright, ~"License" => License}}
                    end, #{}, Files),
    ok = file:write_file(Output, json:encode(X)).

expand_license_result(Filename) ->
    Json = decode(Filename),
    maps:fold(fun (License, Paths, Acc) ->
                      maps:merge(Acc, maps:from_list([{Path, License} || Path <- Paths]))
              end, #{}, Json).

classify_copyright_result(Filename) ->
    Json = decode(Filename),
    Copyrights = copyrights(scan_results(Json)),
    lists:foldl(fun (Copyright, Acc) ->
                        #{<<"statement">> := CopyrightSt, <<"location">> := Location} = Copyright,
                        #{<<"path">> := Path, <<"start_line">> := _StartLine, <<"end_line">> := _EndLine} = Location,
                        Acc#{Path => CopyrightSt}
                    end, #{}, Copyrights).

-spec group_by_licenses(map(), boolean(), boolean()) -> #{License :: binary() => [Path :: binary()]}.
group_by_licenses(Json, ApplyExclude, ApplyCuration) ->
    Excludes = apply_excludes(Json, ApplyExclude),
    Curations = apply_curations(Json, ApplyCuration),

    Licenses = licenses(scan_results(Json)),
    lists:foldl(fun (License, Acc) ->
                            group_by_license(Excludes, Curations, License, Acc)
                    end, #{}, Licenses).

group_by_copyrights(Json, ApplyExclude, _ApplyCuration) ->
    Excludes = apply_excludes(Json, ApplyExclude),
    Copyrights = copyrights(scan_results(Json)),
    lists:foldl(fun (Copyright, Acc) ->
                            group_by_copyright(Excludes, Copyright, Acc)
                    end, #{}, Copyrights).


apply_excludes(Json, ApplyExclude) ->
    onlyif([], ApplyExclude, fun () -> convert_excludes(excludes(Json)) end).

apply_curations(Json, ApplyCuration) ->
    onlyif([], ApplyCuration, fun () -> curations(Json) end).

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

%% fixes the Spdx to split Spdx by app
package_by_app(Spdx) ->
    AppSrcFiles = find_app_src_files("."),

    %% {Packages, ModulesUsed}
    PackageTemplates = generate_spdx_mappings(AppSrcFiles),
    Packages = generate_spdx_packages(PackageTemplates, Spdx),
    Spdx1 = add_packages(Packages, Spdx),
    create_relationships(Packages, Spdx1).


-spec add_packages(Packages :: [spdx_package()], Spdx :: map()) -> SpdxResult :: map().
add_packages(Packages, #{~"packages" := SpdxPackages}=Spdx) ->
    Spdx#{~"packages" := SpdxPackages ++ lists:map(fun create_spdx_package/1, Packages)}.

-spec create_spdx_package(Package :: spdx_package()) -> map().
create_spdx_package(Pkg) ->
    SPDXID = Pkg#spdx_package.'SPDXID',
    VersionInfo= Pkg#spdx_package.'versionInfo',
    Name = Pkg#spdx_package.'name',
    CopyrightText = Pkg#spdx_package.'copyrightText',
    FilesAnalyzed = Pkg#spdx_package.'filesAnalyzed',
    HasFiles = Pkg#spdx_package.'hasFiles',
    Homepage = Pkg#spdx_package.'homepage',
    LicenseConcluded = Pkg#spdx_package.'licenseConcluded',
    LicenseDeclared = Pkg#spdx_package.'licenseDeclared',
    LicenseInfo = Pkg#spdx_package.'licenseInfoFromFiles',
    DownloadLocation = Pkg#spdx_package.'downloadLocation',
    PackageVerification = Pkg#spdx_package.'packageVerificationCode',
    Supplier = Pkg#spdx_package.'supplier',
    #{ ~"SPDXID" => SPDXID,
       ~"versionInfo" => VersionInfo,
       ~"name" => Name,
       ~"copyrightText" => CopyrightText,
       ~"filesAnalyzed" => FilesAnalyzed,
       ~"hasFiles" => HasFiles,
       ~"homepage" => Homepage,
       ~"licenseConcluded" => LicenseConcluded,
       ~"licenseDeclared" => LicenseDeclared,
       ~"licenseInfoFromFiles" => LicenseInfo,
       ~"downloadLocation" => DownloadLocation,
       ~"packageVerificationCode" => PackageVerification,
       ~"supplier" => Supplier
     }.

%% Example:
%% https://github.com/spdx/tools-java/blob/master/testResources/SPDXJSONExample-v2.2.spdx.json#L240-L275
create_relationships(Packages, Spdx) ->
    Relationships =
        lists:foldl(fun (Pkg, Acc) ->
                            #{'PACKAGE_OF' := L} = Pkg#spdx_package.'relationships',
                            lists:foldl(fun ({ElementId, RelatedElement}, Acc1) ->
                                              [create_spdx_relation('PACKAGE_OF', ElementId, RelatedElement) | Acc1]
                                      end, Acc, L)
                    end, [], Packages),
    Spdx#{~"relationships" => Relationships}.

create_spdx_relation('PACKAGE_OF'=Relation, ElementId, RelatedElement) ->
    #{~"spdxElementId" => ElementId,
      ~"relatedSpdxElement" => RelatedElement,
      ~"relationshipType" => Relation}.

-spec find_app_src_files(Folder :: string()) -> [string()].
find_app_src_files(Folder) ->
    S = os:cmd("find "++ Folder ++ " -regex .*.app.src | grep -v \"/test\" | grep -v smoke-build | cut -d/ -f2-"),
    lists:map(fun erlang:list_to_binary/1, string:split(S, "\n", all)).

-spec generate_spdx_mappings(Path :: binary()) -> Result when
      Result :: #{AppName :: binary() => {AppPath :: binary(), AppInfo :: app_info()}}.
generate_spdx_mappings(AppSrcPath) ->
    lists:foldl(fun (AppSrcPath0, Acc) ->
                        DetectedPackages = build_package_location(AppSrcPath0),
                        maps:merge(Acc, DetectedPackages)
                end, #{}, AppSrcPath).

build_package_location(<<>>) -> #{};
build_package_location(AppSrcPath) ->
    case string:split(AppSrcPath, "/", all) of
        [~"lib", App | _] ->
            AppName = erlang:binary_to_atom(App),
            _ = case application:load(AppName) of
                    R when R==ok orelse R=={error, {already_loaded, AppName}} ->
                        %% somewhat unsafe binary_to_atom/1 but we have guarantees to receive
                        %% only apps in Erlang/OTP
                        {ok, AppKey} = application:get_all_key(AppName),
                        AppKey1 = app_key_to_record(AppKey),
                        #{App => {<<"lib/", App/binary>>, AppKey1}};
                    _E ->
                        % TODO: Remove this case. useful only for debudding, but any software running
                        % this script should have all dependencies and never end up here.
                        io:format("[Error] ~p~n", [{AppSrcPath, _E, AppName, App}]),
                        #{}
                end;
        [~"erts"=Erts | _] ->
            #{Erts => {Erts, #app_info{ description = ~"Erlang Runtime System",
                                        id           = [],
                                        vsn          = erlang:list_to_binary(erlang:system_info(version)),
                                        modules      = not_loaded,
                                        applications = [],
                                        included_applications = [],
                                        optional_applications = [] }}}
    end.

app_key_to_record(AppKey) ->
    [{description, Description}, {id, Id},
     {vsn, Vsn}, {modules, Modules},
     {maxP, _}, {maxT, _},
     {registered, _Registered},
     {included_applications, Included},
     {optional_applications, Optional},
     {applications, Apps},
     {env, _Env}, {mod, _Mod},
     {start_phases,_Phases}] = AppKey,
    #app_info{ description  = erlang:list_to_binary(Description),
               id           = erlang:list_to_binary(Id),
               vsn          = erlang:list_to_binary(Vsn),
               modules      = Modules,
               applications = Apps,
               included_applications = Included,
               optional_applications = Optional }.


-spec generate_spdx_packages(PackageMappings, Spdx) -> [spdx_package()] when
      PackageMappings :: #{AppName => {AppPath, app_info()}},
      AppName         :: unicode:chardata(),
      AppPath         :: unicode:chardata(),
      Spdx            :: map().
generate_spdx_packages(PackageMappings, #{~"files" := Files,
                                          ~"documentDescribes" := [ProjectName]}=_Spdx) ->
    maps:fold(fun (PackageName, {PrefixPath, AppInfo}, Acc) ->
                      SpdxPackageFiles = group_files_by_app(Files, PrefixPath),
                      SpdxPackageName = generate_spdxid_name(PackageName),
                      Package =
                          #spdx_package {
                             'SPDXID' = SpdxPackageName,
                             'versionInfo' = AppInfo#app_info.vsn,
                             'name' = PackageName,
                             'copyrightText' = generate_copyright_text(SpdxPackageFiles),
                             'filesAnalyzed' = true,

                             %% O(n2) complexity... fix if necessary
                             'hasFiles' = generate_has_files(SpdxPackageFiles),

                             'homepage' = ?spdx_homepage,
                             'licenseConcluded' = ?erlang_license,
                             'licenseDeclared'  = ?erlang_license,
                             'licenseInfoFromFiles' = generate_license_info_from_files(SpdxPackageFiles),
                             'packageVerificationCode' = #{ 'packageVerificationCodeValue' => ~"TODO"},
                             %% TODO: write the documentation folder, and the BUILD of a Make
                             %%       and the dependencies between apps.
                             'relationships' = #{ 'PACKAGE_OF' => [{SpdxPackageName, ProjectName}]}
                            },
                      [Package | Acc]
               end, [], PackageMappings).

generate_spdxid_name(PackageName) ->
    <<"SPDXRef-otp-", PackageName/binary>>.

generate_license_info_from_files(SpdxPackageFiles) ->
    lists:foldl(fun (#{~"licenseInfoInFiles" := LicenseInfoInFiles}, AccLicenses) ->
                        Licenses0 = lists:foldl(fun (L, Acc0) ->
                                                        string:split(L, ~" OR ") ++ Acc0
                                                end, [], LicenseInfoInFiles),
                        Licenses1 = lists:foldl(fun (L, Acc0) ->
                                                        string:split(L, ~" AND ") ++ Acc0
                                                end, [], Licenses0),
                        lists:uniq(lists:map(fun (L) -> string:trim(L) end, Licenses1 ++ AccLicenses))
                end, [], SpdxPackageFiles).

generate_has_files(SpdxPackageFiles) ->
    lists:map(fun (#{~"SPDXID" := SpdxId}) -> SpdxId end, SpdxPackageFiles).

generate_copyright_text(SpdxPackageFiles) ->
    CopyrightText = lists:foldl(fun (#{~"copyrightText" := CopyrightText}, Acc0) ->
                                        lists:uniq([CopyrightText | Acc0])
                                end, [], SpdxPackageFiles),
    lists:foldl(fun (Copyright, Acc0) ->
                    <<Copyright/binary, "\n", Acc0/binary>>
                end, <<>>, CopyrightText).

group_files_by_app(Files, PrefixPath) ->
    lists:filter(fun (#{~"fileName" := Filename}) ->
                         case string:split(Filename, PrefixPath) of
                             [_ , _ | _] ->
                                 true;
                             _ ->
                                 false
                         end
                 end, Files).

%% spdx_fetch_prefix(PrefixPath, #{"files" := Files}=SPDX) ->
%%     lists:filter(fun (#{~"fileName" := PrefixPath ++ _=Filename}) ->
%%                          case string:prefix(Filename, PrefixPath) of
%%                              nomatch ->
%%                                  false;
%%                              _ ->
%%                                  true
%%                          end
%%                  end, List).

%% is_prefix(SPDXFiles, Prefix) ->


%% remove_files_from_sbom(_SBOM, _ModulesUsed) ->
%%     ok.

%% add_packages(_SBOM, _Packages) ->
%%     ok.

%% Modules: https://erlangforums.com/t/check-if-application-available-in-erlang-vm-how/2983
%% code:all_available().
%% pattern match result and
%% call module_info(attributes)
%% and filter for all that have {behaviour, [application]}.

%% application:get_all_key(kernel).
%% we can get the modules, and applications that we rely/need.

%% system_information:applications().
%%  * kernel-10.2.2
%%  * stdlib-6.2
%%  * xmerl-2.1
%%  * wx-2.4.3
%%  * tools-4.1.1
%%  * tftp-1.2.2
%%  * syntax_tools-3.2.1
%%  * ssl-11.2.7
%%  * ssh-5.2.6
%%  * snmp-5.18
%%  * sasl-4.2.2
%%  * runtime_tools-2.1.1
%%  * reltool-1.0.1
%%  * public_key-1.17.1
%%  * parsetools-2.6
%%  * os_mon-2.10.1
%%  * observer-2.17
%%  * mnesia-4.23.3
%%  * megaco-4.7
%%  * jinterface-1.14.1
%%  * inets-9.3.1
%%  * ftp-1.2.3
%%  * eunit-2.9.1
%%  * et-1.7.1
%%  * erl_interface-5.5.2
%%  * eldap-1.2.14
%%  * edoc-1.3.2
%%  * diameter-2.4.1
%%  * dialyzer-5.3.1
%%  * debugger-5.5
%%  * crypto-5.5.2
%%  * compiler-8.5.5
%%  * common_test-1

empty_sbom() ->
    #{ ~"SPDXID" => ~"SPDXRef-DOCUMENT",
       ~"creationInfo" => #{
                            ~"created" => ~"2025-02-10T15:42:12Z",
                            ~"creators" => [ ~"Tool"],
                            ~"licenseListVersion" => ~"3.26"
                           },
       ~"dataLicense" => ~"CC0-1.0",
       ~"name" => ~"foo",
       ~"documentDescribes" => [ ~"SPDXRef-Project-Unmanaged-otp" ],
       ~"documentNamespace" => ~"spdx://c3947827-a093-422e-be44-ad5420e1f87c",
       ~"files" => [],
       ~"packages" => [],
       ~"spdxVersion" => ~"SPDX-2.2"
     }.

test(_) ->
    ok = test_project(),
    ok = test_packages(),
    ok.

test_project() ->
    Sbom = empty_sbom(),
    ok = test_project_name(Sbom),
    ok = test_name(Sbom),
    ok = test_creators_tooling(Sbom),
    ok = test_spdx_version(Sbom),
    ok.

%% TODO: We do not have any files in the Sbom, so the packages have almost all fields empty.
test_packages() ->
    Sbom = generate_spdx_fixes(empty_sbom(), #{}, #{}),
    ok = test_minimum_apps(Sbom),
    ok = test_copyright_not_empty(Sbom),
    ok = test_filesAnalised(Sbom),
    ok = test_hasFiles_not_empty(Sbom),
    ok = test_homepage(Sbom),
    ok = test_licenseConcluded_exists(Sbom),
    ok = test_licenseDeclared_exists(Sbom),
    ok = test_licenseInfoFromFiles_not_empty(Sbom),
    ok = test_package_names(Sbom),
    ok = test_verificationCode(Sbom),
    ok = test_supplier_Ericsson(Sbom),
    ok = test_versionInfo_not_empty(Sbom),
    ok.

test_project_name(Sbom) ->
    #{~"documentDescribes" := [ProjectName]} = generate_spdx_fixes(Sbom, #{}, #{}),
    ?spdxref_project_name = ProjectName,
    ok.

test_name(Sbom) ->
    #{~"name" := Name} = generate_spdx_fixes(Sbom, #{}, #{}),
    ?spdx_project_name = Name,
    ok.

test_creators_tooling(Sbom) ->
    #{~"creationInfo" := #{~"creators" := Creators}} = generate_spdx_fixes(Sbom, #{}, #{}),
    true = lists:any(fun (Name) ->
                             case string:prefix(Name, ?spdx_creators_tooling) of
                                 nomatch -> false;
                                 _ -> true
                             end
                     end, Creators),
    ok.

test_spdx_version(Sbom) ->
    #{~"spdxVersion" := Version} = generate_spdx_fixes(Sbom, #{}, #{}),
    ?spdx_version = Version,
    ok.

test_minimum_apps(#{~"documentDescribes" := [ProjectName], ~"packages" := Packages}=_Sbom) ->
    _ = lists:foreach(fun (X) -> application:load(erlang:binary_to_atom(X)) end, minimum_otp_apps()),
    PackageNames = [ProjectName | to_spdx_name(minimum_otp_apps())],
    SPDXIds = [ProjectName | lists:map(fun (#{~"SPDXID" := SPDXId}) -> SPDXId end, Packages)],
    true = PackageNames -- SPDXIds == SPDXIds -- PackageNames,
    ok.

to_spdx_name(L) when is_list(L) ->
    lists:map(fun generate_spdxid_name/1, minimum_otp_apps()).

minimum_otp_apps() ->
    [~"kernel", ~"stdlib", ~"xmerl", ~"wx", ~"tools", ~"tftp", ~"syntax_tools", ~"ssl",
     ~"ssh", ~"snmp", ~"sasl", ~"runtime_tools", ~"reltool", ~"public_key", ~"parsetools",
     ~"os_mon", ~"observer", ~"mnesia", ~"megaco", ~"jinterface", ~"inets", ~"ftp", ~"eunit",
     ~"et", ~"erl_interface", ~"eldap", ~"edoc", ~"diameter", ~"dialyzer", ~"debugger", ~"crypto",
     ~"compiler", ~"common_test", ~"erts", ~"asn1"].

test_copyright_not_empty(#{~"packages" := Packages}) ->
    io:format("Copyright: ~p~n~n~n", [Packages]),
    true = lists:all(fun (#{~"copyrightText" := Copyright, ~"name" := Name}=Sbom) ->
                             Copyright =/= ~""
                     end, Packages),
    ok.

test_filesAnalised(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"filesAnalyzed" := Bool}) -> Bool = true end, Packages),
    ok.

test_hasFiles_not_empty(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"hasFiles" := Files}) -> length(Files) > 0 end, Packages),
    ok.

test_homepage(#{~"packages" := Packages})->
    true = lists:all(fun (#{~"homepage" := Homepage}) -> Homepage == ?spdx_homepage end, Packages),
    ok.

test_licenseConcluded_exists(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"licenseConcluded" := License}) -> License =/= ~"" andalso License =/= ~"NONE" end, Packages),
    ok.

test_licenseDeclared_exists(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"licenseDeclared" := License}) -> License =/= ~"" andalso License =/= ~"NONE" end, Packages),
    ok.

test_licenseInfoFromFiles_not_empty(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"licenseInfoInFiles" := [_ | _]}) -> true end, Packages),
    ok.

test_package_names(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"name" := Name}) -> lists:member(Name, minimum_otp_apps()) end, Packages),
    ok.

test_verificationCode(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"packageVerificationCode" := #{~"packageVerificationCodeValue" := Value}}) ->
                             Value =/= ~"TODO" andalso Value =/= ~""
                     end, Packages),
    ok.

test_supplier_Ericsson(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"supplier" := Name}) -> Name = ?spdx_supplier end, Packages),
    ok.

test_versionInfo_not_empty(#{~"packages" := Packages}) ->
    true = lists:all(fun (#{~"version" := Version}) -> Version =/= ~"" end, Packages),
    ok.
