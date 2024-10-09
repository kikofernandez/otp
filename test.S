{module, test}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 17}.


{function, main, 1, 2}.
  {label,1}.
    {line,[]}.
    {func_info,{atom,test},{atom,main},1}.
  {label,2}.
    {call_only,1,{f,10}}. % '-main/1-lc$^0/1-0-'/1


{function, some_test, 1, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,test},{atom,some_test},1}.
  {label,4}.
    return.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,test},{atom,module_info},0}.
  {label,6}.
    {move,{atom,test},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,test},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,test},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-main/1-lc$^0/1-0-', 1, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main/1-lc$^0/1-0-'},1}.
  {label,10}.
    {test,is_nonempty_list,{f,11},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,0},{y,0}}.
    {call,1,{f,4}}. % some_test/1
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,0}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,14},1}. % '-main/1-lc$^1/1-1-'/2
  {label,11}.
    {test,is_nil,{f,12},[{x,0}]}.
    return.
  {label,12}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{atom,bad_generator},{x,0}]}}.
    {call_ext_only,1,{extfunc,erlang,error,1}}.


{function, '-main/1-lc$^1/1-1-', 2, 14}.
  {label,13}.
    {line,[]}.
    {func_info,{atom,test},{atom,'-main/1-lc$^1/1-1-'},2}.
  {label,14}.
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {test,is_nonempty_list,{f,16},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,0}}.
    {test,is_eq_exact,{f,15},[{x,2},{atom,ok}]}.
    {call_only,2,{f,14}}. % '-main/1-lc$^1/1-1-'/2
  {label,15}.
    {allocate,1,3}.
    {move,{x,2},{y,0}}.
    {call,2,{f,14}}. % '-main/1-lc$^1/1-1-'/2
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,16}.
    {move,{x,1},{x,0}}.
    {call_only,1,{f,10}}. % '-main/1-lc$^0/1-0-'/1
