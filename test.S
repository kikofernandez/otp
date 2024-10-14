{module, test}.  %% version = 0

{exports, [{main,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 9}.


{function, main, 1, 2}.
  {label,1}.
    {line,[{location,"test.erl",137}]}.
    {func_info,{atom,test},{atom,main},1}.
  {label,2}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,0}}.
    {call_only,1,{f,4}}. % some_test/1


{function, some_test, 1, 4}.
  {label,3}.
    {line,[{location,"test.erl",145}]}.
    {func_info,{atom,test},{atom,some_test},1}.
  {label,4}.
    {'%',{var_info,{x,0},[{type,{t_cons,any,nil}}]}}.
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
