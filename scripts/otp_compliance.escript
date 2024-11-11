#!/home/erfeafr/Code/otp2/bin/escript
%% -*- erlang -*-

-define(default_classified_result, "scan-result-classified.json").
-define(default_scan_result, "scan-result.json").

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
%%

main(Args) ->
    argparse:run(Args, cli(), #{progname => otp_compliance}).

cli() ->
    #{ commands =>
           #{"classify" =>
                 #{ help => "Classify files by their license group.",
                    arguments => [ input_option(),
                                   output_option(),
                                   apply_excludes(),
                                   apply_curations() ],
                    handler => fun classify/1},
             "diff" =>
                  #{ help => "Compare against previous license results.",
                     arguments => [ input_diff_option(),
                                    output_diff_option(),
                                    base_file() ],
                     handler => fun diff/1}}}.

%%
%% Classification options
%%
input_option() ->
    #{name => input_file,
      type => binary,
      short => $i,
      default => ?default_scan_result,
      long => "-input-file"}.

output_option() ->
    #{name => output_file,
      type => binary,
      short => $o,
      default => ?default_classified_result,
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

%%
%% Diff options
%%
input_diff_option() ->
    #{name => input_file,
      type => binary,
      short => $i,
      required => true,
      long => "-input-file"}.

output_diff_option() ->
    #{name => output_file,
      type => binary,
      short => $o,
      required => true,
      long => "-output-file"}.

base_file() ->
    #{name => base_file,
      type => binary,
      short => $b,
      default => ?default_classified_result,
      long => "-base-file"}.

%%
%% Commands
%%

classify(#{input_file := Filename,
           output_file := Output,
           exclude := ApplyExclude,
           curations := ApplyCuration}) ->
    #{<<"scanner">> := #{<<"scan_results">> := [ScanResults]},
      <<"repository">> :=
          #{<<"config">> :=
                #{<<"excludes">> := #{<<"paths">> := Excludes},
                  <<"curations">> := #{<<"license_findings">> := Curations}}}} = decode(Filename),
    #{<<"summary">> := #{<<"licenses">> := Licenses}} = ScanResults,
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

%%
%% Helper functions
%%

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
