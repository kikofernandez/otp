-module(test).

-export([main/1, main2/1, main4/1, main3/1]).

%%
%% this function introduces a singleton list, which could be remove.
%% as a side effect, there is one less letrec function.
%%
%% This function produces:
%%
%% 'main'/1 =
%%     %% Line 12
%%     ( fun (_0) ->
%% 	  ( case ( _0
%% 		   -| [{'function',{'main',1}}] ) of
%% 	      <X> when 'true' ->
%% 		  %% Line 14
%% 		  ( letrec
%% 			'lc$^0'/1 =
%% 			    fun (_6) ->
%% 				case _6 of
%% 				  <[E|_2]> when 'true' ->
%% 				      ( letrec
%% 					    'lc$^1'/1 =
%% 						fun (_8) ->
%% 						    case _8 of
%% 						      <[Res|_4]>
%% 							  when call 'erlang':'/='
%% 								(Res,
%% 								 'ok') ->
%% 							  let <_10> =
%% 							      apply 'lc$^1'/1
%% 								  (_4)
%% 							  in  ( [Res|_10]
%% 								-| ['compiler_generated'] )
%% 						      ( <[_3|_4]> when 'true' ->
%% 							    apply 'lc$^1'/1
%% 								(_4)
%% 							-| ['compiler_generated'] )
%% 						      <[]> when 'true' ->
%% 							  apply 'lc$^0'/1
%% 							      (_2)
%% 						      ( <_9> when 'true' ->
%% 							    call 'erlang':'error'
%% 								({'bad_generator',_9})
%% 							-| ['compiler_generated'] )
%% 						    end
%% 					in  let <_5> =
%% 						apply 'some_test'/1
%% 						    (E)
%% 					    in  apply 'lc$^1'/1
%% 						    ([_5|[]])
%% 					-| ['list_comprehension'] )
%% 				  ( <[_1|_2]> when 'true' ->
%% 					apply 'lc$^0'/1
%% 					    (_2)
%% 				    -| ['compiler_generated'] )
%% 				  <[]> when 'true' ->
%% 				      []
%% 				  ( <_7> when 'true' ->
%% 					call 'erlang':'error'
%% 					    ({'bad_generator',_7})
%% 				    -| ['compiler_generated'] )
%% 				end
%% 		    in  apply 'lc$^0'/1
%% 			    (X)
%% 		    -| ['list_comprehension'] )
%% 	      ( <_11> when 'true' ->
%% 		    ( primop 'match_fail'
%% 			  (( {'function_clause',_11}
%% 			     -| [{'function',{'main',1}}] ))
%% 		      -| [{'function',{'main',1}}] )
%% 		-| ['compiler_generated'] )
%% 	    end
%% 	    -| [{'function',{'main',1}}] )
%%       -| [{'function',{'main',1}}] )
%%
%%
%%
%%
%% Desired Output
%%
%% 'main'/1 =
%%      %% Line 15
%%      ( fun (_0) ->
%%        ( case ( _0
%%             -| [{'function',{'main',1}}] ) of
%%            <X> when 'true' ->
%%            %% Line 17
%%            ( letrec
%%              'lc$^0'/1 =
%%                  fun (_6) ->
%%                  case _6 of
%%                    <[E|_2]> when 'true' ->
%%                        ( let <_5> =
%%                          apply 'some_test'/1
%%                              (E)
%%                          in  case _5 of
%%                                <Res>
%%                                when call 'erlang':'/='
%%                                  (Res,
%%                                   'ok') ->
%%                                  let <_10> =
%%                                     apply 'lc$^0'/1
%%                                     (_2)
%%                                 in  ( [Res|_10]
%%                                   -| ['compiler_generated'] )
%%                                <_9> when 'true' ->
%%                                apply 'lc$^0'/1
%%                                    (_2)
%%                              end
%%                      -| ['list_comprehension'] )
%%                    ( <[_1|_2]> when 'true' ->
%%                      apply 'lc$^0'/1
%%                          (_2)
%%                      -| ['compiler_generated'] )
%%                    <[]> when 'true' ->
%%                        []
%%                    ( <_7> when 'true' ->
%%                      call 'erlang':'error'
%%                          ({'bad_generator',_7})
%%                      -| ['compiler_generated'] )
%%                  end
%%              in  apply 'lc$^0'/1
%%                  (X)
%%              -| ['list_comprehension'] )
%%            ( <_11> when 'true' ->
%%              ( primop 'match_fail'
%%                (( {'function_clause',_11}
%%                   -| [{'function',{'main',1}}] ))
%%               -| [{'function',{'main',1}}] )
%%          -| ['compiler_generated'] )
%%         end
%%          -| [{'function',{'main',1}}] )
%%        -| [{'function',{'main',1}}] )
%%
main(X) ->
    %% Kiko will optimize comprehensions like
    [Res || E <- X, Res <- [some_test(E)], Res /= ok].

some_test(X) ->
    X.


main2(X) ->
    Ys = X,
    %% Kiko will optimize comprehensions like
    [{Res, Y, Z} || E <- X, Res <- [some_test(E)], Res /= ok, Y <- Ys, Z <- [Y], Z/= ok].



%% Cost of inlining nested function calls, instead of jumping around.
-doc """
Cost of inlining nested function calls, instead of jumping around.

This version is 3 times faster than main2/1.

> ./../erlperf/erlperf 'test:main2([1,2,3,4,5,6,7,8]).' 'test:main3([1,2,3,4,5,6,7,8]).' --samples 30 -w 10 -r full

OS : Linux
CPU: 11th Gen Intel(R) Core(TM) i5-1145G7 @ 2.60GHz
VM : Erlang/OTP 28 [DEVELOPMENT] [erts-15.0.1] [source-27a6328515] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Code                               ||   Samples       Avg   StdDev    Median      P99  Iteration    Rel
test:main3([1,2,3,4,5,6,7,8]).      1        30    827 Ki    7.37%    849 Ki   916 Ki    1209 ns   100%
test:main2([1,2,3,4,5,6,7,8]).      1        30    311 Ki    7.90%    322 Ki   345 Ki    3217 ns    38%
""".
-spec main3(list()) -> list().
main3(X) ->
    Ys = X,
    %% Kiko will optimize comprehensions like
    [inline(E, Ys, false) || E <- X].

inline(E, Ys, false) ->
    Res = some_test(E),
    case Res /= ok of
        true ->
            inline(Res, Ys, true)
    end;
inline(_, [], _) ->
    [];
inline(Res, [Z|Zs], true) ->
    case Z /= ok of
        true -> [{Res, Z, Z} | inline(Res, Zs, true)]
    end.









%% Translation to Core Erlang

%% main3(X) ->
%%     case X of
%%         X when true ->
%%             Ys = X,
%%             Lc01 = fun Lc01(Elva) ->
%%                            io:format("~p Lc01: ~p~n", [?LINE, Elva]),
%%                            case Elva of
%%                                [E|Tre] when true ->
%%                                    io:format("~p Lc01: ~p~n", [?LINE, Elva]),
%%                                    Lc11 = fun Lc11(Treton) ->
%%                                                   io:format("~p Lc11~n", [?LINE]),
%%                                                   case Treton of
%%                                                       [Res|Fem] when Res /= ok ->
%%                                                           Lc21 = fun Lc21(Femton) ->
%%                                                                          case Femton of
%%                                                                              [Y|Atta] when true ->
%%                                                                                           Lc31 = fun Lc31(Sjuton) ->
%%                                                                                                          case Sjuton of
%%                                                                                                              [Z|Tio] when Z /= ok ->
%%                                                                                                                  Nitton = apply(Lc31, [Tio]),
%%                                                                                                                  [{Res, Y, Z}|Nitton];
%%                                                                                                              [_Nio|Tio] when true ->
%%                                                                                                                  apply(Lc31, [Tio]);
%%                                                                                                              [] ->
%%                                                                                                                  apply(Lc21, [Atta])
%%                                                                                                          end
%%                                                                                                  end,
%%                                                                                           apply(Lc31, [[Y|[]]]);
%%                                                                              %% [_Sju|Atta] when true ->
%%                                                                              %%              apply(Lc21, [Atta]);
%%                                                                              [] when true ->
%%                                                                                           apply(Lc11, [Fem])
%%                                                                          end
%%                                                                  end,
%%                                                           apply(Lc21, [Ys]);
%%                                                       [_Fyra|Fem] ->
%%                                                           apply(Lc11, [Fem]);
%%                                                       [] ->
%%                                                           apply(Lc01, [Tre])
%%                                                   end
%%                                         end,
%%                                    io:format("~p some_test: ~p(~p)~n", [?LINE, fun some_test/1, E]),
%%                                    Sex = apply(fun some_test/1, [E]),
%%                                    io:format("~p Lc11: ~p(~p)~n", [?LINE, Lc01, X]),
%%                                    apply(Lc11, [[Sex|[]]]);
%%                               %% [Tva|Tre] ->
%%                               %%      apply(Lc01, Tre);
%%                                [] ->
%%                                    []
%%                            end
%%                    end,
%%             io:format("~p Lc01: ~p(~p)~n", [?LINE, Lc01, X]),
%%             apply(Lc01, [X])
%%     end.



%%
%%  I believe this approach suffers from too many de-structuring checks, list emptyness checks, etc
%%
main4(X) ->
    Ys = X,
    Loop = fun Loop(Elva, Treton, Femton, Sjuton) ->
                 %% case {Elva, Treton, Femton, Sjuton} of
                 %%     {_, _, _, Sjuton} ->
                         case Sjuton of
                             [Z|Tio] ->
                                 case Z /= ok of
                                     true ->
                                         [Res|_Fem] = Treton,
                                         [Y|_Atta] = Femton,
                                         Nitton = Loop(Elva, Treton, [Y|_Atta], Tio),
                                         [{Res, Y, Z}|Nitton];
                                     false ->
                                         Loop(Elva, Treton, Femton, Tio)
                                 end;
                             [] ->
                                 case Femton of
                                     [_Y|Atta] ->
                                         Loop(Elva, Treton, Atta, nil)
                                 end;
                             nil ->
                                 case Femton of
                                     [] ->
                                         [_Res|Fem] = Treton,
                                         Loop(Elva, Fem, nil, nil);
                                     [Y|_Atta] ->
                                         Loop(Elva, Treton, Femton, [Y]);
                                     nil ->
                                         case Treton of
                                             [] ->
                                                 case Elva of
                                                     [_E|Tre] ->
                                                         Loop(Tre, nil, nil, nil)
                                                 end;
                                             [Res|Fem] ->
                                                 case Res /= ok of
                                                     true ->
                                                         Loop(Elva, [Res|Fem], Ys, nil);
                                                     false ->
                                                         Loop(Elva, Fem, nil, nil)
                                                 end;
                                             nil ->
                                                 case Elva of
                                                     [] ->
                                                         [];
                                                     [E|_Tre] ->
                                                         Sex = some_test(E),
                                                         Loop(Elva, [Sex], Femton, Sjuton)
                                                 end
                                         end
                                 end
                         end
                 %% end
           end,
    Loop(X, nil, nil, nil).



%%
%% Work with single argument functions and an extra arg that checks the depth of the recursion
%%
%% main5(X) ->
%%     Ys = X,
%%     Loop = fun Loop(V, 1) ->
%%                    case V of
%%                        [] ->
%%                            [];
%%                        [E|_Tre] ->
%%                            Sex = some_test(E),
%%                            Loop([Sex], 2)
%%                    end;
%%                (V, 2) ->
%%                    case V of
%%                        [] ->
%%            end,
%%         loop(X, 1).



%% main4(X) ->
%%     Ys = X,
%%     Loop = fun Loop(Elva, Treton, Femton, Sjuton) ->
%%                    case {Elva, Treton, Femton, Sjuton} of
%%                        {[E|_Tre], nil, nil, nil} when true ->                          % OK
%%                            Sex = apply(fun some_test/1, [E]),
%%                            apply(Loop, [Elva, [Sex], Femton, Sjuton]);
%%                        {[], nil, nil, nil} ->                                          % OK
%%                            [];
%%                        {[_E|Tre], [Res|Fem], nil, nil} when Res /= ok ->               % OK
%%                            apply(Loop, [[_E|Tre], [Res|Fem], Ys, nil]);
%%                        {[_E|Tre], [_Fyra|Fem], nil, nil} ->                            % OK
%%                            apply(Loop, {[_E|Tre], Fem, nil, nil});
%%                        {[_E|Tre], [], nil, nil} ->                                     % OK
%%                            apply(Loop, [Tre, nil, nil, nil]);
%%                        {[_E|_Tre], [_Res|_Fem], [Y|_Atta], nil} ->                     % OK
%%                            apply(Loop, [Elva, Treton, Femton, [Y]]);
%%                        {[_E|_Tre], [_Res|Fem], [], nil} ->                             % OK
%%                            apply(Loop, [Elva, Fem, nil, nil]);
%%                        {[_E|_Tre], [Res|_Fem], [Y|_Atta], [Z|Tio]} when Z /= ok ->     % OK
%%                            Nitton = apply(Loop, [[_E|_Tre], [Res|_Fem], [Y|_Atta], Tio]),
%%                            [{Res, Y, Z}|Nitton];
%%                        {[_E|_Tre], [_Res|_Fem], [_Y|_Atta], [_|Tio]} ->                % OK
%%                            apply(Loop, [Elva, Treton, Femton, Tio]);
%%                        {[_E|_Tre], [_Res|_Fem], [_Y|Atta], []} ->                      % OK
%%                            apply(Loop, [Elva, Treton, Atta, nil])
%%                    end
%%            end,
%%     Loop(X, nil, nil, nil).




%% some_test(_) ->
%%     case rand:uniform(2) of
%%         1 -> ok;
%%         2 -> error
%%     end.

%% test(Name) ->
%%     maybe
%%         {ok,Markdown} ?= file:read_file(Name),
%%         Man = convert(Markdown),
%%         maybe
%%             ok ?= file:write_file(Name, Man),
%%         else
%%             {error,Reason0} ->
%%                 Reason = file:format_error(Reason0),
%%                 erlang:fail(io_lib:format("~ts: write failed: ~ts",
%%                                           [Name,Reason]))
%%         end
%%     else
%%         {error,Reason1} ->
%%             Reason2 = file:format_error(Reason1),
%%             erlang:fail(io_lib:format("~ts: ~ts", [Name,Reason2]))
%%     end.


%% convert(_) ->
%%      ok.



%%
%% Current output
%%
%%
%% 'main'/1 =
%%     %% Line 15
%%     ( fun (_0) ->
%%       ( case ( _0
%%            -| [{'function',{'main',1}}] ) of
%%           <X> when 'true' ->
%%           %% Line 17
%%           ( letrec
%%             'lc$^0'/1 =
%%                 fun (_6) ->
%%                 case _6 of
%%                   <[E|_2]> when 'true' ->
%%                       ( letrec
%%                         'lc$^1'/1 =
%%                         fun (_8) ->
%%                             case _8 of
%%                               <[Res|_4]>
%%                               when call 'erlang':'/='
%%                                 (Res,
%%                                  'ok') ->
%%                               let <_10> =
%%                                   apply 'lc$^1'/1
%%                                   (_4)
%%                               in  ( [Res|_10]
%%                                 -| ['compiler_generated'] )
%%                               ( <[_3|_4]> when 'true' ->
%%                                 apply 'lc$^1'/1
%%                                 (_4)
%%                             -| ['compiler_generated'] )
%%                               <[]> when 'true' ->
%%                               apply 'lc$^0'/1
%%                                   (_2)
%%                               ( <_9> when 'true' ->
%%                                 call 'erlang':'error'
%%                                 ({'bad_generator',_9})
%%                             -| ['compiler_generated'] )
%%                             end
%%                     in  let <_5> =
%%                         apply 'some_test'/1
%%                             (E)
%%                         in  apply 'lc$^1'/1
%%                             ([_5|[]])
%%                     -| ['list_comprehension'] )
%%                   ( <[_1|_2]> when 'true' ->
%%                     apply 'lc$^0'/1
%%                         (_2)
%%                     -| ['compiler_generated'] )
%%                   <[]> when 'true' ->
%%                       []
%%                   ( <_7> when 'true' ->
%%                     call 'erlang':'error'
%%                         ({'bad_generator',_7})
%%                     -| ['compiler_generated'] )
%%                 end
%%             in  apply 'lc$^0'/1
%%                 (X)
%%             -| ['list_comprehension'] )
%%           ( <_11> when 'true' ->
%%             ( primop 'match_fail'
%%               (( {'function_clause',_11}
%%                  -| [{'function',{'main',1}}] ))
%%               -| [{'function',{'main',1}}] )
%%         -| ['compiler_generated'] )
%%         end
%%         -| [{'function',{'main',1}}] )
%%       -| [{'function',{'main',1}}] )



%%
%% Desired Output
%%
%% 'main'/1 =
%%      %% Line 15
%%      ( fun (_0) ->
%%        ( case ( _0
%%             -| [{'function',{'main',1}}] ) of
%%            <X> when 'true' ->
%%            %% Line 17
%%            ( letrec
%%              'lc$^0'/1 =
%%                  fun (_6) ->
%%                  case _6 of
%%                    <[E|_2]> when 'true' ->
%%                        ( let <_5> =
%%                          apply 'some_test'/1
%%                              (E)
%%                          in  case _5 of
%%                                <Res>
%%                                when call 'erlang':'/='
%%                                  (Res,
%%                                   'ok') ->
%%                                  let <_10> =
%%                                     apply 'lc$^0'/1
%%                                     (_2)
%%                                 in  ( [Res|_10]
%%                                   -| ['compiler_generated'] )
%%                                <_9> when 'true' ->
%%                                apply 'lc$^0'/1
%%                                    (_2)
%%                              end
%%                      -| ['list_comprehension'] )
%%                    ( <[_1|_2]> when 'true' ->
%%                      apply 'lc$^0'/1
%%                          (_2)
%%                      -| ['compiler_generated'] )
%%                    <[]> when 'true' ->
%%                        []
%%                    ( <_7> when 'true' ->
%%                      call 'erlang':'error'
%%                          ({'bad_generator',_7})
%%                      -| ['compiler_generated'] )
%%                  end
%%              in  apply 'lc$^0'/1
%%                  (X)
%%              -| ['list_comprehension'] )
%%            ( <_11> when 'true' ->
%%              ( primop 'match_fail'
%%                (( {'function_clause',_11}
%%                   -| [{'function',{'main',1}}] ))
%%               -| [{'function',{'main',1}}] )
%%          -| ['compiler_generated'] )
%%         end
%%          -| [{'function',{'main',1}}] )
%%        -| [{'function',{'main',1}}] )
