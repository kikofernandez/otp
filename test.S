{module, test}.  %% version = 0

{exports, [{main,1},
           {main2,1},
           {main3,1},
           {main4,1},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 66}.


{function, main, 1, 2}.
  {label,1}.
    {line,[]}.
    {func_info,{atom,test},{atom,main},1}.
  {label,2}.
    {call_only,1,{f,60}}. % '-main/1-lc$^0/1-0-'/1


{function, some_test, 1, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,test},{atom,some_test},1}.
  {label,4}.
    return.


{function, main2, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,test},{atom,main2},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {call_only,2,{f,44}}. % '-main2/1-lc$^0/1-0-'/2


{function, main3, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,test},{atom,main3},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {call_only,2,{f,40}}. % '-main3/1-lc$^0/1-0-'/2


{function, inline, 3, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,test},{atom,inline},3}.
  {label,10}.
    {'%',{var_info,{x,2},[{type,{t_atom,[false,true]}}]}}.
    {test,is_eq_exact,{f,13},[{x,2},{atom,true}]}.
    {test,is_nonempty_list,{f,11},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,1}}.
    {test,is_ne_exact,{f,14},[{x,2},{atom,ok}]}.
    {allocate,2,3}.
    {move,{x,0},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{atom,true},{x,2}}.
    {call,3,{f,10}}. % inline/3
    {'%',{var_info,{x,0},[{type,{t_list,{t_tuple,3,true,#{}},nil}}]}}.
    {test_heap,6,1}.
    {put_tuple2,{x,1},{list,[{y,0},{y,1},{y,1}]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,11}.
    {test,is_nil,{f,12},[{x,1}]}.
    {move,nil,{x,0}}.
    return.
  {label,12}.
    {move,{atom,true},{x,2}}.
    {call_only,3,{f,65}}. % '-inlined-inline/3-'/3
  {label,13}.
    {allocate,1,2}.
    {move,{x,1},{y,0}}.
    {call,1,{f,4}}. % some_test/1
    {test,is_eq_exact,{f,15},[{x,0},{atom,ok}]}.
  {label,14}.
    {case_end,{atom,false}}.
  {label,15}.
    {move,{atom,true},{x,2}}.
    {move,{y,0},{x,1}}.
    {call_last,3,{f,10},1}. % inline/3


{function, main4, 1, 17}.
  {label,16}.
    {line,[]}.
    {func_info,{atom,test},{atom,main4},1}.
  {label,17}.
    {move,{atom,nil},{x,3}}.
    {move,{atom,nil},{x,2}}.
    {move,{x,0},{x,4}}.
    {move,{atom,nil},{x,1}}.
    {call_only,5,{f,23}}. % '-main4/1-Loop/4-0-'/5


{function, module_info, 0, 19}.
  {label,18}.
    {line,[]}.
    {func_info,{atom,test},{atom,module_info},0}.
  {label,19}.
    {move,{atom,test},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 21}.
  {label,20}.
    {line,[]}.
    {func_info,{atom,test},{atom,module_info},1}.
  {label,21}.
    {move,{x,0},{x,1}}.
    {move,{atom,test},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main4/1-Loop/4-0-', 5, 23}.
  {label,22}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main4/1-Loop/4-0-'},5}.
  {label,23}.
    {test,is_nonempty_list,{f,25},[{x,3}]}.
    {get_list,{x,3},{x,5},{x,3}}.
    {test,is_eq_exact,{f,24},[{x,5},{atom,ok}]}.
    {call_only,5,{f,23}}. % '-main4/1-Loop/4-0-'/5
  {label,24}.
    {test,is_nonempty_list,{f,36},[{x,1}]}.
    {get_hd,{x,1},{x,6}}.
    {test,is_nonempty_list,{f,33},[{x,2}]}.
    {allocate,3,7}.
    {move,{x,6},{y,1}}.
    {move,{x,5},{y,2}}.
    {get_hd,{x,2},{y,0}}.
    {call,5,{f,23}}. % '-main4/1-Loop/4-0-'/5
    {'%',{var_info,{x,0},[{type,{t_list,{t_tuple,3,true,#{}},nil}}]}}.
    {test_heap,6,1}.
    {put_tuple2,{x,1},{list,[{y,1},{y,0},{y,2}]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,3}.
    return.
  {label,25}.
    {test,is_atom,{f,32},[{x,3}]}.
    {test,is_eq_exact,{f,38},[{tr,{x,3},{t_atom,any}},{atom,nil}]}.
    {test,is_nonempty_list,{f,26},[{x,2}]}.
    {test_heap,2,5}.
    {get_hd,{x,2},{x,3}}.
    {put_list,{x,3},nil,{x,3}}.
    {call_only,5,{f,23}}. % '-main4/1-Loop/4-0-'/5
  {label,26}.
    {test,is_atom,{f,31},[{x,2}]}.
    {test,is_eq_exact,{f,37},[{tr,{x,2},{t_atom,any}},{atom,nil}]}.
    {test,is_nonempty_list,{f,28},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,3}}.
    {test,is_eq_exact,{f,27},[{x,2},{atom,ok}]}.
    {move,{atom,nil},{x,2}}.
    {move,{x,3},{x,1}}.
    {move,{atom,nil},{x,3}}.
    {call_only,5,{f,23}}. % '-main4/1-Loop/4-0-'/5
  {label,27}.
    {move,{atom,nil},{x,3}}.
    {move,{x,4},{x,2}}.
    {call_only,5,{f,23}}. % '-main4/1-Loop/4-0-'/5
  {label,28}.
    {test,is_atom,{f,30},[{x,1}]}.
    {test,is_eq_exact,{f,35},[{tr,{x,1},{t_atom,any}},{atom,nil}]}.
    {test,is_nonempty_list,{f,29},[{x,0}]}.
    {allocate,2,5}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{y,1}}.
    {get_hd,{x,0},{x,0}}.
    {call,1,{f,4}}. % some_test/1
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,nil},{x,3}}.
    {move,{atom,nil},{x,2}}.
    {move,{y,1},{x,4}}.
    {move,{y,0},{x,0}}.
    {call_last,5,{f,23},2}. % '-main4/1-Loop/4-0-'/5
  {label,29}.
    {test,is_nil,{f,34},[{x,0}]}.
    return.
  {label,30}.
    {test,is_nil,{f,35},[{x,1}]}.
    {test,is_nonempty_list,{f,34},[{x,0}]}.
    {get_tl,{x,0},{x,0}}.
    {move,{atom,nil},{x,2}}.
    {move,{atom,nil},{x,3}}.
    {move,{atom,nil},{x,1}}.
    {call_only,5,{f,23}}. % '-main4/1-Loop/4-0-'/5
  {label,31}.
    {test,is_nil,{f,37},[{x,2}]}.
    {test,is_nonempty_list,{f,36},[{x,1}]}.
    {get_tl,{x,1},{x,1}}.
    {move,{atom,nil},{x,3}}.
    {move,{atom,nil},{x,2}}.
    {call_only,5,{f,23}}. % '-main4/1-Loop/4-0-'/5
  {label,32}.
    {test,is_nil,{f,38},[{x,3}]}.
    {test,is_nonempty_list,{f,37},[{x,2}]}.
    {get_tl,{x,2},{x,2}}.
    {move,{atom,nil},{x,3}}.
    {call_only,5,{f,23}}. % '-main4/1-Loop/4-0-'/5
  {label,33}.
    {badmatch,{x,2}}.
  {label,34}.
    {case_end,{x,0}}.
  {label,35}.
    {case_end,{x,1}}.
  {label,36}.
    {badmatch,{x,1}}.
  {label,37}.
    {case_end,{x,2}}.
  {label,38}.
    {case_end,{x,3}}.


{function, '-main3/1-lc$^0/1-0-', 2, 40}.
  {label,39}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main3/1-lc$^0/1-0-'},2}.
  {label,40}.
    {test,is_nonempty_list,{f,41},[{x,0}]}.
    {allocate,2,2}.
    {move,{x,1},{y,1}}.
    {get_list,{x,0},{x,0},{y,0}}.
    {move,{atom,false},{x,2}}.
    {call,3,{f,10}}. % inline/3
    {'%',{var_info,{x,0},[{type,{t_list,{t_tuple,3,true,#{}},nil}}]}}.
    {move,{y,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{x,0},{y,1}}.
    {trim,1,1}.
    {move,{x,2},{x,0}}.
    {call,2,{f,40}}. % '-main3/1-lc$^0/1-0-'/2
    {'%',{var_info,{x,0},
                   [{type,{t_list,{t_list,{t_tuple,3,true,#{}},nil},nil}}]}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,41}.
    {test,is_nil,{f,42},[{x,0}]}.
    return.
  {label,42}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,bad_generator},{x,0}]}}.
    {call_ext_only,1,{extfunc,erlang,error,1}}.


{function, '-main2/1-lc$^0/1-0-', 2, 44}.
  {label,43}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main2/1-lc$^0/1-0-'},2}.
  {label,44}.
    {test,is_nonempty_list,{f,45},[{x,0}]}.
    {allocate,2,2}.
    {move,{x,1},{y,1}}.
    {get_list,{x,0},{x,0},{y,0}}.
    {call,1,{f,4}}. % some_test/1
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,0}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,2}}.
    {call_last,3,{f,48},2}. % '-main2/1-lc$^1/1-1-'/3
  {label,45}.
    {test,is_nil,{f,46},[{x,0}]}.
    return.
  {label,46}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,bad_generator},{x,0}]}}.
    {call_ext_only,1,{extfunc,erlang,error,1}}.


{function, '-main2/1-lc$^1/1-1-', 3, 48}.
  {label,47}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main2/1-lc$^1/1-1-'},3}.
  {label,48}.
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {test,is_nonempty_list,{f,50},[{x,0}]}.
    {get_list,{x,0},{x,3},{x,0}}.
    {test,is_eq_exact,{f,49},[{x,3},{atom,ok}]}.
    {call_only,3,{f,48}}. % '-main2/1-lc$^1/1-1-'/3
  {label,49}.
    {move,{x,2},{x,4}}.
    {swap,{x,0},{x,2}}.
    {call_only,5,{f,52}}. % '-main2/1-lc$^2/1-2-'/5
  {label,50}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {call_only,2,{f,44}}. % '-main2/1-lc$^0/1-0-'/2


{function, '-main2/1-lc$^2/1-2-', 5, 52}.
  {label,51}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main2/1-lc$^2/1-2-'},5}.
  {label,52}.
    {'%',{var_info,{x,2},[{type,{t_list,any,nil}}]}}.
    {test,is_nonempty_list,{f,53},[{x,0}]}.
    {test_heap,2,5}.
    {get_list,{x,0},{x,5},{x,0}}.
    {move,{x,4},{x,6}}.
    {move,{x,0},{x,4}}.
    {put_list,{x,5},nil,{x,0}}.
    {call_only,7,{f,56}}. % '-main2/1-lc$^3/1-3-'/7
  {label,53}.
    {test,is_nil,{f,54},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {move,{x,4},{x,2}}.
    {call_only,3,{f,48}}. % '-main2/1-lc$^1/1-1-'/3
  {label,54}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,bad_generator},{x,0}]}}.
    {call_ext_only,1,{extfunc,erlang,error,1}}.


{function, '-main2/1-lc$^3/1-3-', 7, 56}.
  {label,55}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main2/1-lc$^3/1-3-'},7}.
  {label,56}.
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {'%',{var_info,{x,2},[{type,{t_list,any,nil}}]}}.
    {test,is_nonempty_list,{f,58},[{x,0}]}.
    {get_list,{x,0},{x,7},{x,0}}.
    {test,is_eq_exact,{f,57},[{x,7},{atom,ok}]}.
    {call_only,7,{f,56}}. % '-main2/1-lc$^3/1-3-'/7
  {label,57}.
    {allocate,3,8}.
    {move,{x,7},{y,0}}.
    {move,{x,5},{y,1}}.
    {move,{x,3},{y,2}}.
    {call,7,{f,56}}. % '-main2/1-lc$^3/1-3-'/7
    {'%',{var_info,{x,0},[{type,{t_list,{t_tuple,3,true,#{}},nil}}]}}.
    {test_heap,6,1}.
    {put_tuple2,{x,1},{list,[{y,2},{y,1},{y,0}]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,3}.
    return.
  {label,58}.
    {move,{x,4},{x,0}}.
    {move,{x,6},{x,4}}.
    {call_only,5,{f,52}}. % '-main2/1-lc$^2/1-2-'/5


{function, '-main/1-lc$^0/1-0-', 1, 60}.
  {label,59}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main/1-lc$^0/1-0-'},1}.
  {label,60}.
    {test,is_nonempty_list,{f,62},[{x,0}]}.
    {allocate,2,1}.
    {init_yregs,{list,[{y,0}]}}.
    {get_list,{x,0},{x,0},{y,1}}.
    {call,1,{f,4}}. % some_test/1
    {move,{x,0},{y,0}}.
    {test,is_eq_exact,{f,61},[{x,0},{atom,ok}]}.
    {move,{y,1},{x,0}}.
    {call_last,1,{f,60},2}. % '-main/1-lc$^0/1-0-'/1
  {label,61}.
    {move,{y,1},{x,0}}.
    {move,{y,0},{y,1}}.
    {trim,1,1}.
    {call,1,{f,60}}. % '-main/1-lc$^0/1-0-'/1
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,62}.
    {test,is_nil,{f,63},[{x,0}]}.
    return.
  {label,63}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,bad_generator},{x,0}]}}.
    {call_ext_only,1,{extfunc,erlang,error,1}}.


{function, '-inlined-inline/3-', 3, 65}.
  {label,64}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-inlined-inline/3-'},3}.
  {label,65}.
    {jump,{f,64}}.
