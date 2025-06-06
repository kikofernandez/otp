%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%

-module(odbc).
-moduledoc """
Erlang ODBC application

This application provides an Erlang interface to communicate with relational
SQL-databases. It is built on top of Microsofts ODBC interface and therefore
requires that you have an ODBC driver to the database that you want to connect
to.

> #### Note {: .info }
>
> The functions `first/[1,2]`, `last/[1,2]`, `next/[1,2]`, `prev[1,2]` and
> `select/[3,4]` assumes there is a result set associated with the connection to
> work on. Calling the function `select_count/[2,3]` associates such a result
> set with the connection. Calling select_count again will remove the current
> result set association and create a new one. Calling a function which dose not
> operate on an associated result sets, such as `sql_query/[2,3]`, will remove
> the current result set association.
>
> Alas some drivers only support sequential traversal of the result set, e.i.
> they do not support what in the ODBC world is known as scrollable cursors.
> This will have the effect that functions such as `first/[1,2]`, `last/[1,2]`,
> `prev[1,2]`, etc will return `{error, driver_does_not_support_function}`

## Error Handling

The error handling strategy and possible errors sources are described in the
Erlang ODBC [User's Guide.](error_handling.md)

## References

\[1]: Microsoft ODBC 3.0, Programmer's Reference and SDK Guide  
See also http://msdn.microsoft.com/
""".

-behaviour(gen_server).

-include("odbc_internal.hrl").

-define(ODBC_PORT_TIMEOUT, 5000).

%% API --------------------------------------------------------------------

-export([start/0, start/1, stop/0,
	 connect/2, disconnect/1, commit/2, commit/3, sql_query/2,
	 sql_query/3, select_count/2, select_count/3, first/1, first/2,
	 last/1, last/2, next/1, next/2, prev/1, prev/2, select/3,
	 select/4, param_query/3, param_query/4, describe_table/2,
	 describe_table/3]).

%%-------------------------------------------------------------------------
%% supervisor callbacks
-export([start_link_sup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-doc "Opaque reference to an ODBC connection as returnded by connect/2.".
-doc(#{group => <<"Types used in ODBC application">>}).
-opaque connection_reference() :: pid().
-doc "Name of column in the result set.".
-doc(#{group => <<"Types used in ODBC application">>}).
-type col_name()             :: string().
-doc """
A tuple, with the number of elements selected form columns in a database row,
containg the values of the columns such as `{value(), value() ... value()} `.
""".
-doc(#{group => <<"Types used in ODBC application">>}).
-type row()                  :: tuple().
-doc "Erlang data type that corresponds to the ODBC data type being handled.".
-doc(#{group => <<"Types used in ODBC application">>}).
-type value()                :: null | term().
-doc "Return value for queries that select data from database tabels.".
-doc(#{group => <<"Types used in ODBC application">>}).
-type selected()             :: {selected, [col_name()], [row()]}.
-doc "Return value for queries that update database tables.".
-doc(#{group => <<"Types used in ODBC application">>}).
-type updated()              :: {updated, n_rows()}.
-doc """
The number of affected rows for UPDATE, INSERT, or DELETE queries. For other
query types the value is driver defined, and hence should be ignored.
""".
-doc(#{group => <<"Types used in ODBC application">>}).
-type n_rows()               :: integer().
-doc """
Data type used by ODBC, to learn which Erlang data type corresponds to an ODBC
data type see the Erlang to ODBC data type [mapping](databases.md#type) in the
User's Guide.
""".
-doc(#{group => <<"Types used in ODBC application">>}).
-type odbc_data_type()       ::  sql_integer | sql_smallint | sql_tinyint |
                                 {sql_decimal, Precision::integer(), Scale::integer()} |
                                 {sql_numeric, Precision::integer(), Scale::integer()} |
                                 {sql_char, Size::integer()} |
                                 {sql_wchar, Size::integer()} |
                                 {sql_varchar, Size::integer()} |
                                 {sql_wvarchar, Size::integer()}|
                                 {sql_float, Precision::integer()} |
                                 {sql_wlongvarchar, Size::integer()} |
                                 {sql_float, Precision::integer()} |
                                 sql_real | sql_double | sql_bit | atom().
-doc """
An explanation of what went wrong. For common errors there will be atom
decriptions.
""".
-doc(#{group => <<"Types used in ODBC application">>}).
-type common_reason()        :: connection_closed | extended_error() | term().
-doc """
extended error type with ODBC and native database error codes, as well as the
base reason that would have been returned had extended_errors not been enabled.
""".
-doc(#{group => <<"Types used in ODBC application">>}).
-type extended_error()       :: {string(), integer(), term()}.

-export_type([connection_reference/0,
              col_name/0, row/0,
              selected/0,
              updated/0,
              n_rows/0,
              odbc_data_type/0,
              common_reason/0,
              extended_error/0]).

%%--------------------------------------------------------------------------
%% Internal state
-record(state, {erlang_port,                 % The port to the c-program
		reply_to,		     % gen_server From parameter 
		owner,                       % Pid of the connection owner
		result_set = undefined,      % exists | undefined
		auto_commit_mode = on,       % on | off
		%% Indicates if first, last and "select absolute"
		%% is supported by the odbc driver.
		absolute_pos,                % true | false  
		%% Indicates if prev and "select relative"
		%% is supported by the odbc driver.
		relative_pos,                % true | false
		scrollable_cursors,      % on | off
		%% connecting | connected | disconnecting
		state = connecting,	    
		%% For timeout handling
		pending_request,      
		num_timeouts = 0,
		listen_sockets,
		sup_socket,
		odbc_socket
	       }).

%%--------------------------------------------------------------------------

%%%=========================================================================
%%%  API
%%%=========================================================================

%%--------------------------------------------------------------------
-doc(#{equiv => start/1}).
-spec start() -> ok | {error, Reason} when
      Reason ::term().
%%--------------------------------------------------------------------
start() ->
    application:start(odbc).

%%--------------------------------------------------------------------
-doc """
Starts the odbc application. Default type is temporary.
[See application(3)](`m:application`)
""".
-spec start(Type) -> ok | {error, Reason} when
      Type :: permanent | transient | temporary,
      Reason ::term().
%%
%% Description: Starts the inets application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start(Type) -> 
    application:start(odbc, Type).

%%--------------------------------------------------------------------
-doc "Stops the odbc application. [See application(3)](`m:application`)".
-spec stop() -> ok.
%%
%% Description: Stops the odbc application.
%%--------------------------------------------------------------------
stop() -> 
    application:stop(odbc).

%%-------------------------------------------------------------------------
-doc """
Opens a connection to the database. The connection is associated with the
process that created it and can only be accessed through it. This function may
spawn new processes to handle the connection. These processes will terminate if
the process that created the connection dies or if you call disconnect/1.

If automatic commit mode is turned on, each query will be considered as an
individual transaction and will be automatically committed after it has been
executed. If you want more than one query to be part of the same transaction the
automatic commit mode should be turned off. Then you will have to call commit/3
explicitly to end a transaction.

The default timeout is infinity

If the option binary_strings is turned on all strings will be returned as
binaries and strings inputted to param_query will be expected to be binaries.
The user needs to ensure that the binary is in an encoding that the database
expects. By default this option is turned off.

As default result sets are returned as a lists of tuples. The `TupleMode` option
still exists to keep some degree of backwards compatibility. If the option is
set to off, result sets will be returned as a lists of lists instead of a lists
of tuples.

Scrollable cursors are nice but causes some overhead. For some connections speed
might be more important than flexible data access and then you can disable
scrollable cursor for a connection, limiting the API but gaining speed.

> #### Note {: .info }
>
> Turning the scrollable_cursors option off is noted to make old odbc-drivers
> able to connect that will otherwise fail.

If trace mode is turned on this tells the ODBC driver to write a trace log to
the file SQL.LOG that is placed in the current directory of the erlang emulator.
This information may be useful if you suspect there might be a bug in the erlang
ODBC application, and it might be relevant for you to send this file to our
support. Otherwise you will probably not have much use of this.

> #### Note {: .info }
>
> For more information about the `ConnectStr` see description of the function
> SQLDriverConnect in \[1].

The `extended_errors` option enables extended ODBC error information when an
operation fails. Rather than returning `{error, Reason}`, the failing function
will return `{error, {ODBCErrorCode, NativeErrorCode, Reason}}`. Note that this
information is probably of little use when writing database-independent code,
but can be of assistance in providing more sophisticated error handling when
dealing with a known underlying database.

- `ODBCErrorCode` is the ODBC error string returned by the ODBC driver.
- `NativeErrorCode` is the numeric error code returned by the underlying
  database. The possible values and their meanings are dependent on the database
  being used.
- `Reason` is as per the `Reason` field when extended errors are not enabled.

> #### Note {: .info }
>
> The current implementation spawns a port program written in C that utilizes
> the actual ODBC driver. There is a default timeout of 5000 msec for this port
> program to connect to the Erlang ODBC application. This timeout can be changed
> by setting an application specific environment variable 'port_timeout' with
> the number of milliseconds for the ODBC application. E.g.: \[\{odbc,
> [\{port_timeout, 60000\}]\}] to set it to 60 seconds.
""".
-spec connect(ConnectionStr, Options) -> {ok, ConnectionReferense} |
          {error, Reason} when
      ConnectionStr :: string(),
      Options :: [{auto_commit, on | off} |
                  {timeout, timeout()} |
                  {binary_strings, on | off} |
                  {tuple_row, on | off} |
                  {scrollable_cursors, on | off} |
                  {trace_driver, on | off} |
                  {extended_errors, on | off}],
      ConnectionReferense :: connection_reference(),
      Reason :: port_program_executable_not_found | common_reason().

%% Description: Spawns an erlang control process that will open a port
%%              to a c-process that uses the ODBC API to open a connection
%%              to the database. 
%%-------------------------------------------------------------------------
connect(ConnectionStr, Options) when is_list(ConnectionStr), is_list(Options) ->
    
    %% Spawn the erlang control process.
    try  supervisor:start_child(odbc_sup, [[{client, self()}]]) of
	 {ok, Pid} ->
	    connect(Pid, ConnectionStr, Options);
	 {error, Reason} ->
	    {error, Reason}
    catch
	exit:{noproc, _} ->
            {error, odbc_not_started}
    end.

%%--------------------------------------------------------------------------
-doc """
Closes a connection to a database. This will also terminate all processes that
may have been spawned when the connection was opened. This call will always
succeed. If the connection cannot be disconnected gracefully it will be brutally
killed. However you may receive an error message as result if you try to
disconnect a connection started by another process.
""".
-spec disconnect(ConnectionReferense) -> ok | {error, Reason} when
      ConnectionReferense :: connection_reference(),
      Reason :: process_not_owner_of_odbc_connection | extended_error().
%%                                    
%% Description: Disconnects from the database and terminates both the erlang
%%              control process and the database handling c-process. 
%%--------------------------------------------------------------------------
disconnect(ConnectionReference) when is_pid(ConnectionReference)->
    ODBCCmd = [?CLOSE_CONNECTION],
    case call(ConnectionReference, {disconnect, ODBCCmd}, 5000) of 
	{error, connection_closed} ->
	    %% If the connection has already been closed the effect of
	    %% disconnect has already been accomplished
	    ok; 
	%% Note a time out of this call will return ok, as disconnect
	%% will always succeed, the time out is to make sure
	%% the connection is killed brutally if it will not be shut down
	%% gracefully.
	ok ->
	    ok;
	%% However you may receive an error message as result if you try to
	%% disconnect a connection started by another process.
	Other ->
	    Other
    end. 
	    
%%--------------------------------------------------------------------------
-doc(#{equiv => commit/3}).
-spec commit(ConnectionReference, CommitMode) -> ok | {error, Reason} when
      ConnectionReference :: connection_reference(),
      CommitMode ::  commit | rollback,
      Reason :: not_an_explicit_commit_connection |
                process_not_owner_of_odbc_connection | common_reason().
%%--------------------------------------------------------------------------
commit(ConnectionReference, CommitMode) ->
    commit(ConnectionReference, CommitMode, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------------
-doc """
Commits or rollbacks a transaction. Needed on connections where automatic commit
is turned off.
""".
-spec commit(ConnectionReference, CommitMode, TimeOut) -> ok | {error, Reason} when
      ConnectionReference :: connection_reference(),
      CommitMode ::  commit | rollback,
      TimeOut :: erlang:timeout(),
      Reason :: not_an_explicit_commit_connection |
                process_not_owner_of_odbc_connection | common_reason().
%%--------------------------------------------------------------------------
commit(ConnectionReference, commit, infinity) 
  when is_pid(ConnectionReference) ->
    ODBCCmd = [?COMMIT_TRANSACTION, ?COMMIT],
    call(ConnectionReference, {commit, ODBCCmd}, infinity);

commit(ConnectionReference, commit, TimeOut) 
  when is_pid(ConnectionReference), is_integer(TimeOut), TimeOut > 0  ->
    ODBCCmd = [?COMMIT_TRANSACTION, ?COMMIT],
    call(ConnectionReference, {commit, ODBCCmd}, TimeOut);

commit(ConnectionReference, rollback, infinity) 
  when is_pid(ConnectionReference) ->
    ODBCCmd = [?COMMIT_TRANSACTION, ?ROLLBACK],
    call(ConnectionReference, {commit, ODBCCmd}, infinity);

commit(ConnectionReference, rollback, TimeOut) 
  when is_pid(ConnectionReference), is_integer(TimeOut), TimeOut > 0  ->
    ODBCCmd = [?COMMIT_TRANSACTION, ?ROLLBACK],
    call(ConnectionReference, {commit, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
-doc(#{equiv => sql_query/3}).
-spec sql_query(ConnectionReference, SQLQuery) -> Result | {error, Reason}  when
      ConnectionReference :: connection_reference(),
      SQLQuery :: string(),
      Result  :: updated() | selected(),
      Reason  :: process_not_owner_of_odbc_connection | common_reason().
%%                                    
%%--------------------------------------------------------------------------
sql_query(ConnectionReference, SQLQuery) ->
    sql_query(ConnectionReference, SQLQuery, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------------
-doc """
Executes a SQL query or a batch of SQL queries. If it is a SELECT query the
result set is returned, on the format `{selected, ColNames, Rows}`. For other
query types the tuple `{updated, NRows}` is returned, and for batched queries,
if the driver supports them, this function can also return a list of result
tuples.

> #### Note {: .info }
>
> Some drivers may not have the information of the number of affected rows
> available and then the return value may be `{updated, undefined} `.
>
> The list of column names is ordered in the same way as the list of values of a
> row, e.g. the first `ColName` is associated with the first `Value` in a `Row`.
""".
-spec sql_query(ConnectionReference, SQLQuery, TimeOut) -> Result | {error, Reason}  when
      ConnectionReference :: connection_reference(),
      SQLQuery :: string(),
      TimeOut :: erlang:timeout(),
      Result  :: updated() | selected(),
      Reason  :: process_not_owner_of_odbc_connection | common_reason().
%%
%%--------------------------------------------------------------------------
sql_query(ConnectionReference, SQLQuery, infinity) when 
  is_pid(ConnectionReference), is_list(SQLQuery) -> 
    ODBCCmd = [?QUERY, SQLQuery],
    call(ConnectionReference, {sql_query, ODBCCmd}, infinity);

sql_query(ConnectionReference, SQLQuery, TimeOut) 
  when is_pid(ConnectionReference),is_list(SQLQuery),is_integer(TimeOut),TimeOut>0 -> 
    ODBCCmd = [?QUERY, SQLQuery],
    call(ConnectionReference, {sql_query, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
-doc(#{equiv => select_count/3}).
-spec select_count(ConnectionReference, SQLQuery) -> {ok, NrRows} |
          {error, Reason} when
      ConnectionReference :: connection_reference(),
      SQLQuery :: string(),
      NrRows :: n_rows(),
      Reason :: process_not_owner_of_odbc_connection | common_reason().
%%                                    
%%--------------------------------------------------------------------------
select_count(ConnectionReference, SQLQuery) ->	
    select_count(ConnectionReference, SQLQuery, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------------
-doc """
Executes a SQL SELECT query and associates the result set with the connection. A
cursor is positioned before the first row in the result set and the tuple
`{ok, NrRows}` is returned.

> #### Note {: .info }
>
> Some drivers may not have the information of the number of rows in the result
> set, then `NrRows` will have the value `undefined`.
""".
-spec select_count(ConnectionReference, SQLQuery, TimeOut) -> {ok, NrRows} |
          {error, Reason} when
      ConnectionReference :: connection_reference(),
      SQLQuery :: string(),
      TimeOut :: erlang:timeout(),
      NrRows :: n_rows(),
      Reason :: process_not_owner_of_odbc_connection | common_reason().
%%
%%--------------------------------------------------------------------------
select_count(ConnectionReference, SQLQuery, infinity) when 
  is_pid(ConnectionReference), is_list(SQLQuery) ->
    ODBCCmd = [?SELECT_COUNT, SQLQuery],
    call(ConnectionReference, {select_count, ODBCCmd}, infinity);

select_count(ConnectionReference, SQLQuery, TimeOut) when 
  is_pid(ConnectionReference), is_list(SQLQuery), is_integer(TimeOut), TimeOut > 0 ->
    ODBCCmd = [?SELECT_COUNT, SQLQuery],
    call(ConnectionReference, {select_count, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
-doc(#{equiv => first/2}).
-spec first(ConnectionReference) ->  Result | {error, Reason}  when
      ConnectionReference :: connection_reference(),
      Result :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function |
                scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
first(ConnectionReference) ->	
    first(ConnectionReference, ?DEFAULT_TIMEOUT).	

%%--------------------------------------------------------------------------
-doc "Returns the first row of the result set and positions a cursor at this row.".
-spec first(ConnectionReference, TimeOut) -> Result | {error, Reason} when
      ConnectionReference :: connection_reference(),
      TimeOut :: erlang:timeout(),
      Result :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function |
                scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
first(ConnectionReference, infinity) when is_pid(ConnectionReference) ->	
    ODBCCmd = [?SELECT, ?SELECT_FIRST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, infinity);

first(ConnectionReference, TimeOut)
  when is_pid(ConnectionReference), is_integer(TimeOut), TimeOut > 0 ->	
    ODBCCmd = [?SELECT, ?SELECT_FIRST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
-doc(#{equiv => last/2}).
-spec last(ConnectionReference) -> Result | {error, Reason} when
      ConnectionReference :: connection_reference(),
      Result :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function |
                scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
last(ConnectionReference) ->	
    last(ConnectionReference, ?DEFAULT_TIMEOUT).	

%%--------------------------------------------------------------------------
-doc "Returns the last row of the result set and positions a cursor at this row.".
-spec last(ConnectionReference, TimeOut) -> Result | {error, Reason} when
      ConnectionReference :: connection_reference(),
      TimeOut :: erlang:timeout(),
      Result :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function |
                scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
last(ConnectionReference, infinity) when is_pid(ConnectionReference) ->	
    ODBCCmd = [?SELECT, ?SELECT_LAST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, infinity);

last(ConnectionReference, TimeOut) 
  when is_pid(ConnectionReference), is_integer(TimeOut), TimeOut > 0 ->	
    ODBCCmd = [?SELECT, ?SELECT_LAST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
-doc(#{equiv => next/2}).
-spec next(ConnectionReference) -> Result | {error, Reason} when
      ConnectionReference :: connection_reference(),
      Result :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function |
                scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
next(ConnectionReference) ->	
    next(ConnectionReference, ?DEFAULT_TIMEOUT).	

%%--------------------------------------------------------------------------
-doc """
Returns the next row of the result set relative the current cursor position and
positions the cursor at this row. If the cursor is positioned at the last row of
the result set when this function is called the returned value will be
`{selected, ColNames,[]}` e.i. the list of row values is empty indicating that
there is no more data to fetch.
""".
-spec next(ConnectionReference, TimeOut) -> Result | {error, Reason} when
     ConnectionReference :: connection_reference(),
      TimeOut :: erlang:timeout(),
      Result :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function |
                scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
next(ConnectionReference, infinity) when is_pid(ConnectionReference) ->	
    ODBCCmd = [?SELECT, ?SELECT_NEXT],
    call(ConnectionReference, {select_cmd, next, ODBCCmd}, infinity);

next(ConnectionReference, TimeOut) 
  when is_pid(ConnectionReference), is_integer(TimeOut), TimeOut > 0 ->	
    ODBCCmd = [?SELECT, ?SELECT_NEXT],
    call(ConnectionReference, {select_cmd, next, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
-doc(#{equiv => prev/2}).
-spec prev(ConnectionReference) -> Result | {error, Reason} when
      ConnectionReference :: connection_reference(),
      Result :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function |
                scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
prev(ConnectionReference) ->	
    prev(ConnectionReference, ?DEFAULT_TIMEOUT).	

%%--------------------------------------------------------------------------
-doc """
Returns the previous row of the result set relative the current cursor position
and positions the cursor at this row.
""".
-spec prev(ConnectionReference, TimeOut) -> Result | {error, Reason} when
      ConnectionReference :: connection_reference(),
      TimeOut :: erlang:timeout(),
      Result :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function |
                scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
prev(ConnectionReference, infinity) when is_pid(ConnectionReference) ->	
    ODBCCmd = [?SELECT, ?SELECT_PREV],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd}, infinity);

prev(ConnectionReference, TimeOut) 
  when is_pid(ConnectionReference), is_integer(TimeOut), TimeOut > 0 ->	
    ODBCCmd = [?SELECT, ?SELECT_PREV],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
-doc(#{equiv => select/4}).
-spec select(ConnectionReference, Position, N) ->  Result | {error, Reason} when
      ConnectionReference ::connection_reference(),
      Position :: next | {relative, integer()} | {absolute, integer()},
      N :: integer(),
      Result  :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function
              | scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
select(ConnectionReference, Position, N) ->
    select(ConnectionReference, Position, N, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------------
-doc """
Selects `N` consecutive rows of the result set. If `Position` is `next` it is
semantically equivalent of calling `next/[1,2]` `N` times. If `Position` is
`{relative, Pos}`, `Pos` will be used as an offset from the current cursor
position to determine the first selected row. If `Position` is
`{absolute, Pos}`, `Pos` will be the number of the first row selected. After
this function has returned the cursor is positioned at the last selected row. If
there is less then `N` rows left of the result set the length of `Rows` will be
less than `N`. If the first row to select happens to be beyond the last row of
the result set, the returned value will be `{selected, ColNames,[]}` e.i. the
list of row values is empty indicating that there is no more data to fetch.
""".
-spec select(ConnectionReference, Position, N, TimeOut) ->  Result | {error, Reason} when
      ConnectionReference ::connection_reference(),
      Position :: next | {relative, integer()} | {absolute, integer()},
      N :: integer(),
      TimeOut :: erlang:timeout(),
      Result  :: selected(),
      Reason :: result_set_does_not_exist | driver_does_not_support_function
              | scrollable_cursors_disabled | process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
select(ConnectionReference, next, N, infinity) 
  when is_pid(ConnectionReference), is_integer(N), N > 0 ->
    ODBCCmd = [?SELECT, ?SELECT_N_NEXT,
	       integer_to_list(?DUMMY_OFFSET), ";", 
	       integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, next, ODBCCmd},
	 infinity);

select(ConnectionReference, next, N, TimeOut) 
  when is_pid(ConnectionReference), is_integer(N), N > 0,
  is_integer(TimeOut), TimeOut > 0 ->
    ODBCCmd = [?SELECT, ?SELECT_N_NEXT,
	       integer_to_list(?DUMMY_OFFSET), ";", 
	       integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, next, ODBCCmd},
	 TimeOut);

select(ConnectionReference, {relative, Pos} , N, infinity) 
  when is_pid(ConnectionReference), is_integer(Pos), Pos > 0, is_integer(N), N > 0 ->
    ODBCCmd = [?SELECT, ?SELECT_RELATIVE,
	       integer_to_list(Pos), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd},
	 infinity);

select(ConnectionReference, {relative, Pos} , N, TimeOut) 
  when is_pid(ConnectionReference), is_integer(Pos), Pos >0, is_integer(N),  N > 0,
  is_integer(TimeOut), TimeOut > 0 ->
    ODBCCmd = [?SELECT,?SELECT_RELATIVE,
	       integer_to_list(Pos), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd},
	 TimeOut);

select(ConnectionReference, {absolute, Pos} , N, infinity) 
  when is_pid(ConnectionReference), is_integer(Pos), Pos > 0, is_integer(N), N > 0 ->
    ODBCCmd = [?SELECT, ?SELECT_ABSOLUTE,
	       integer_to_list(Pos), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd},
	 infinity);

select(ConnectionReference, {absolute, Pos} , N, TimeOut) 
  when is_pid(ConnectionReference), is_integer(Pos), Pos > 0, is_integer(N),  N > 0, 
  is_integer(TimeOut), TimeOut > 0  ->
    ODBCCmd = [?SELECT, ?SELECT_ABSOLUTE,
	       integer_to_list(Pos), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd},
	 TimeOut).
%%--------------------------------------------------------------------------
-doc(#{equiv => param_query/4}).
-spec param_query(ConnectionReference, SQLQuery, Params) ->
          Result | {error, Reason} when
      ConnectionReference :: connection_reference(),
      SQLQuery :: string(),
      Params   :: [{odbc_data_type(), [value()]}] |[{odbc_data_type(), in | out| inout, [value()]}],
      Result ::  selected() | updated(),
      Reason :: driver_does_not_support_function |
                process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
param_query(ConnectionReference, SQLQuery, Params) ->
    param_query(ConnectionReference, SQLQuery, Params, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------------
-doc """
Executes a parameterized SQL query. For an example see the
["Using the Erlang API"](getting_started.md#param_query) in the Erlang ODBC
User's Guide.

> #### Note {: .info }
>
> Use the function describe_table/\[2,3] to find out which ODBC data type that
> is expected for each column of that table. If a column has a data type that is
> described with capital letters, alas it is not currently supported by the
> param_query function. To learn which Erlang data type corresponds to an ODBC
> data type see the Erlang to ODBC data type [mapping](databases.md#type) in the
> User's Guide.
""".
-spec param_query(ConnectionReference, SQLQuery, Params, TimeOut) ->
          Result | {error, Reason} when
      ConnectionReference :: connection_reference(),
      SQLQuery :: string(),
      Params   :: [{odbc_data_type(), [value()]}] |[{odbc_data_type(), in | out| inout, [value()]}],
      TimeOut :: erlang:timeout(),
      Result ::  selected() | updated(),
      Reason :: driver_does_not_support_function |
                process_not_owner_of_odbc_connection |
                common_reason().
%%--------------------------------------------------------------------------
param_query(ConnectionReference, SQLQuery, Params, infinity) 
  when is_pid(ConnectionReference), is_list(SQLQuery), is_list(Params) ->
    Values = param_values(Params),
    NoRows = length(Values),
    NewParams = lists:map(fun fix_params/1, Params),
    ODBCCmd = [?PARAM_QUERY, term_to_binary({SQLQuery ++ [?STR_TERMINATOR],
					     NoRows, NewParams})],
    call(ConnectionReference, {param_query, ODBCCmd}, infinity);

param_query(ConnectionReference, SQLQuery, Params, TimeOut)
  when is_pid(ConnectionReference), is_list(SQLQuery), is_list(Params),
       is_integer(TimeOut), TimeOut > 0 ->
    Values = param_values(Params),
    NoRows = length(Values),
    NewParams = lists:map(fun fix_params/1, Params),
    ODBCCmd = [?PARAM_QUERY, term_to_binary({SQLQuery ++ [?STR_TERMINATOR],
					     NoRows, NewParams})],
    call(ConnectionReference, {param_query, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
-doc(#{equiv => describe_table/3}).
-spec describe_table(ConnectionReference, Table) ->
          {ok, Description} | {error, Reason} when
      ConnectionReference :: connection_reference(),
      Table :: string(),
      Description :: [{col_name(), odbc_data_type()}],
      Reason ::  process_not_owner_of_odbc_connection | common_reason().
%%--------------------------------------------------------------------------

describe_table(ConnectionReference, Table) ->
    describe_table(ConnectionReference, Table, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------------
-doc """
Queries the database to find out the ODBC data types of the columns of the table
`Table`.
""".
-spec describe_table(ConnectionReference, Table, TimeOut) ->
          {ok, Description} | {error, Reason} when
      ConnectionReference :: connection_reference(),
      Table :: string(),
      TimeOut :: erlang:timeout(),
      Description :: [{col_name(), odbc_data_type()}],
      Reason ::  process_not_owner_of_odbc_connection | common_reason().
%%--------------------------------------------------------------------------

describe_table(ConnectionReference, Table, infinity) when 
  is_pid(ConnectionReference), is_list(Table) -> 
    ODBCCmd = [?DESCRIBE, "SELECT * FROM " ++ Table],
    call(ConnectionReference, {describe_table, ODBCCmd}, infinity);

describe_table(ConnectionReference, Table, TimeOut) 
  when is_pid(ConnectionReference),is_list(Table),is_integer(TimeOut),TimeOut>0 -> 
    ODBCCmd = [?DESCRIBE, "SELECT * FROM " ++ Table],
    call(ConnectionReference, {describe_table, ODBCCmd}, TimeOut).
%%%=========================================================================
%%% Start/stop
%%%=========================================================================
%%--------------------------------------------------------------------------
%% start_link_sup(Args) -> {ok, Pid} | {error, Reason} 
%%                                    
%% Description: Callback function for the odbc supervisor. It is called 
%%            : when connect/2 calls supervisor:start_child/2 to start an 
%%            : instance of the erlang odbc control process.
%%--------------------------------------------------------------------------
-doc false.
start_link_sup(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%% Stop functionality is handled by disconnect/1

%%%========================================================================
%%% Callback functions from gen_server
%%%========================================================================

%%-------------------------------------------------------------------------
%% init(Args) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}
%% Description: Initiates the erlang process that manages the connection
%%              and starts the port-program that use the odbc driver
%%		to communicate with the database.
%%-------------------------------------------------------------------------
-doc false.
init(Args) ->
    process_flag(trap_exit, true),
    {value, {client, ClientPid}} = lists:keysearch(client, 1, Args),
    
    erlang:monitor(process, ClientPid),
    
    Inet = case gen_tcp:listen(0, [inet6, {ip, loopback}]) of
	       {ok, Dummyport} ->
		   gen_tcp:close(Dummyport),
		   inet6;
	       _ ->
		   inet
	   end,

    {ok, ListenSocketSup} =
	gen_tcp:listen(0, [Inet, binary, {packet, ?LENGTH_INDICATOR_SIZE},
			   {active, false}, {nodelay, true},
			   {ip, loopback}]),
    {ok, ListenSocketOdbc} =
	gen_tcp:listen(0, [Inet, binary, {packet, ?LENGTH_INDICATOR_SIZE},
			   {active, false}, {nodelay, true},
			   {ip, loopback}]),

    %% Start the port program (a c program) that utilizes the odbc driver 
    case os:find_executable(?SERVERPROG, ?SERVERDIR) of
	FileName when is_list(FileName)->
	    Port  = open_port({spawn, "\""++FileName++"\""},
			      [{packet, ?LENGTH_INDICATOR_SIZE}, binary,
			       exit_status]),
	    State = #state{listen_sockets = 
			   [ListenSocketSup, ListenSocketOdbc],
			   erlang_port = Port, owner = ClientPid},
	    {ok, State};
	false ->
	    {stop, port_program_executable_not_found}
    end.
		    
%%--------------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State}               |
%%                                      {noreply, State, Timeout}      |
%%                                      {stop, Reason, Reply, State}   |
%%                                      {stop, Reason, Reply, State}     
%% Description: Handle incoming requests. Only requests from the process
%%              that created the connection are allowed in order to preserve
%%              the semantics of result sets.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------
-doc false.
handle_call({Client, Msg, Timeout}, From, State = 
	    #state{owner = Client, reply_to = undefined})  ->
    handle_msg(Msg, Timeout, State#state{reply_to = From});

%% The client has caught the timeout and is sending a new request, but
%% we must preserve a synchronous communication with the port. This
%% request will be handled when we have received the answer to the
%% timed out request and thrown it away, if it has not already been
%% timed out itself in which case the request is thrown away.
handle_call(Request = {Client, _, Timeout}, From, 
	    State = #state{owner = Client, reply_to = skip,
			   num_timeouts = N}) when N < ?MAX_SEQ_TIMEOUTS ->
    {noreply, State#state{pending_request = {Request, From}}, Timeout};

%% The client has sent so many sequential requests that has timed out that 
%% there might be something radically wrong causing the ODBC-driver to
%% hang. So we give up and close the connection. 
handle_call({Client, _, _}, From, 
	    State = #state{owner = Client,  
			   num_timeouts = N}) when N >= ?MAX_SEQ_TIMEOUTS ->
    gen_server:reply(From, {error, connection_closed}), 
    {stop, too_many_sequential_timeouts, State#state{reply_to = undefined}};

handle_call(_, _, State) ->
    {reply, {error, process_not_owner_of_odbc_connection}, 
     State#state{reply_to = undefined}}.

%%--------------------------------------------------------------------------
%% Func: handle_msg(Msg, Timeout, State) -> same as handle_call/3.
%% Description: Sends requests to the port-program.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------
handle_msg({connect, ODBCCmd, AutoCommitMode, SrollableCursors},
	   Timeout, State) ->

    [ListenSocketSup, ListenSocketOdbc] = State#state.listen_sockets,
    
    %% Inform c-client so it knows where to send answers
    {ok, InetPortSup} = inet:port(ListenSocketSup),
    {ok, InetPortOdbc} = inet:port(ListenSocketOdbc),
    
    port_command(State#state.erlang_port, 
		 [integer_to_list(InetPortSup), ";", 
		  integer_to_list(InetPortOdbc) , ?STR_TERMINATOR]),
    
    NewState = State#state{auto_commit_mode = AutoCommitMode,
			   scrollable_cursors = SrollableCursors},
    
    case gen_tcp:accept(ListenSocketSup, port_timeout()) of
	{ok, SupSocket} ->
	    gen_tcp:close(ListenSocketSup),
	    case gen_tcp:accept(ListenSocketOdbc, port_timeout()) of
		{ok, OdbcSocket} ->
		    gen_tcp:close(ListenSocketOdbc),
		    odbc_send(OdbcSocket, ODBCCmd), 
		    {noreply, NewState#state{odbc_socket = OdbcSocket,
					     sup_socket = SupSocket}, 
		     Timeout};
		{error, Reason} ->
		    {stop, Reason, {error, connection_closed}, NewState}
	    end;
	{error, Reason} ->
	    {stop, Reason, {error, connection_closed}, NewState}
    end;    
    
handle_msg({disconnect, ODBCCmd}, Timeout, State) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State#state{state = disconnecting}, Timeout};

handle_msg({commit, _ODBCCmd}, Timeout, 
	   State = #state{auto_commit_mode = on}) ->
    {reply, {error, not_an_explicit_commit_connection}, 
     State#state{reply_to = undefined}, Timeout};

handle_msg({commit, ODBCCmd}, Timeout, 
	   State = #state{auto_commit_mode = off}) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State, Timeout};

handle_msg({sql_query, ODBCCmd}, Timeout, State) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State#state{result_set = undefined}, Timeout};

handle_msg({param_query, ODBCCmd}, Timeout, State) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State#state{result_set = undefined}, Timeout};

handle_msg({describe_table, ODBCCmd}, Timeout, State) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State#state{result_set = undefined}, Timeout};

handle_msg({select_count, ODBCCmd}, Timeout, State) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State#state{result_set = exists}, Timeout};

handle_msg({select_cmd, absolute, ODBCCmd}, Timeout,
	   State = #state{result_set = exists, absolute_pos = true}) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State, Timeout};

handle_msg({select_cmd, relative, ODBCCmd}, Timeout, 
	   State = #state{result_set = exists, relative_pos = true}) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State, Timeout};

handle_msg({select_cmd, next, ODBCCmd}, Timeout,
	   State = #state{result_set = exists}) ->
    odbc_send(State#state.odbc_socket, ODBCCmd),
    {noreply, State, Timeout};

handle_msg({select_cmd, _Type, _ODBCCmd}, _Timeout,
	   State = #state{result_set = undefined}) ->
    {reply, {error, result_set_does_not_exist}, 
     State#state{reply_to = undefined}};

handle_msg({select_cmd, _Type, _ODBCCmd}, _Timeout, State) ->
    Reply = case State#state.scrollable_cursors of
		on ->
		    {error, driver_does_not_support_function};
		off ->
		    {error, scrollable_cursors_disabled}
	    end,
	    
    {reply, Reply, State#state{reply_to = undefined}};

%---------------------------------------------------------------------------
%% Catch all -  This can only happen if the application programmer writes 
%% really bad code that violates the API.
handle_msg(Request, _Timeout, State) ->
    {stop, {'API_violation_connection_colsed', Request},
     {error, connection_closed}, State#state{reply_to = undefined}}.

%%--------------------------------------------------------------------------
%% handle_cast(Request, State) -> {noreply, State} | 
%%                                {noreply, State, Timeout} |
%%                                {stop, Reason, State} 
%% Description: Handles cast messages.         
%% Note: The order of the function clauses is significant.
%%-------------------------------------------------------------------------
%% Catch all - This can only happen if the application programmer writes 
%% really bad code that violates the API.
-doc false.
handle_cast(Msg, State) ->
    {stop, {'API_violation_connection_colsed', Msg}, State}.

%%--------------------------------------------------------------------------
%% handle_info(Msg, State) -> {noreply, State} | {noreply, State, Timeout} |
%%			      {stop, Reason, State}
%% Description: Handles timouts, replies from the port-program and EXIT and
%%		down messages.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------
-doc false.
handle_info({tcp, Socket, BinData}, State = #state{state = connecting, 
						reply_to = From,
						odbc_socket = Socket}) ->
    case binary_to_term(BinData) of
	{ok, AbsolutSupport, RelativeSupport} ->
	    NewState = State#state{absolute_pos = AbsolutSupport,
				   relative_pos = RelativeSupport},
	    gen_server:reply(From, ok), 
	    {noreply, NewState#state{state = connected,
				     reply_to = undefined}};
	Error ->
	    gen_server:reply(From, Error), 
	    {stop, normal, State#state{reply_to = undefined}}
    end;
        

handle_info({tcp, Socket, _},
	    State = #state{state = connected,
 			   odbc_socket = Socket,
 			   reply_to = skip,
 			   pending_request = undefined}) ->
    %% Disregard this message as it is a answer to a query that has timed
    %% out.
    {noreply, State#state{reply_to = undefined}};

handle_info({tcp, Socket, _},
 	    State = #state{state = connected, odbc_socket = Socket,
 			   reply_to = skip}) ->
    
    %% Disregard this message as it is a answer to a query that has timed
    %% out and process the pending request. 
    {{_, Msg, Timeout}, From} = State#state.pending_request,
    handle_msg(Msg, Timeout, State#state{pending_request=undefined,
					 reply_to = From});

handle_info({tcp, Socket, BinData}, State = #state{state = connected,
						   reply_to = From,
						   odbc_socket = Socket}) ->
    %% Send the reply from the database (received by the erlang control 
    %% process from the port program) to the waiting client.
    gen_server:reply(From, BinData),
    {noreply, State#state{reply_to = undefined,
			  num_timeouts = 0}};

handle_info({tcp, Socket, BinData}, State = #state{state = disconnecting,
						   reply_to = From,
						   odbc_socket = Socket}) ->

    %% The connection will always be closed 
    gen_server:reply(From, ok),  
    
    case binary_to_term(BinData) of
 	ok -> 
 	    ok;
 	{error, Reason} ->
	    Report = 
		io_lib:format("ODBC could not end connection "  
			      "gracefully due to ~p~n", [Reason]),
 	    error_logger:error_report(Report)
    end,
    
    {stop, normal, State#state{reply_to = undefined}};

handle_info(timeout, 
	    State = #state{state = disconnecting, 
			   reply_to = From}) when From /= undefined ->
    gen_server:reply(From, ok), 
    {stop, {timeout, "Port program is not responding to disconnect, " 
 	    "will be killed"}, State};

handle_info(timeout, 
	    State = #state{state = connecting, 
			   reply_to = From}) when From /= undefined ->
    gen_server:reply(From, timeout),
    {stop, normal, State#state{reply_to = undefined}};

handle_info(timeout, 
	    State = #state{state = connected, 
			   pending_request = undefined,
			   reply_to = From}) when From /= undefined ->
    gen_server:reply(From, timeout),
    {noreply, State#state{reply_to = skip,
			  num_timeouts = State#state.num_timeouts + 1}};

handle_info(timeout, State =
	    #state{state = connected,      
		   pending_request = {{_, {disconnect, _}, _}, 
				      PendingFrom}}) ->
    gen_server:reply(PendingFrom, ok),
    {stop, {timeout, "Port-program busy when trying to disconnect,  "
	    "will be killed"},
     State#state{pending_request = undefined, reply_to = undefined,
		 num_timeouts = State#state.num_timeouts + 1}};

handle_info(timeout, State =
	    #state{state = connected, 
		   pending_request = {_, PendingFrom}}) ->
    gen_server:reply(PendingFrom, timeout),
    %% The state variable reply_to should continue to have the value skip 
    {noreply, State#state{pending_request = undefined,  
 			  num_timeouts = State#state.num_timeouts + 1}};

handle_info({Port, {exit_status, ?EXIT_SUCCESS}},   
	    State = #state{erlang_port = Port, state = disconnecting}) ->
    {noreply, State}; % Ignore as this is perfectly normal in this case 

handle_info({Port, {exit_status, Status}}, 
	    State = #state{erlang_port = Port}) ->
    {stop, {port_exit, ?PORT_EXIT_REASON(Status)}, State};

handle_info({'EXIT', Port, _}, State = #state{erlang_port = Port,
					      state = disconnecting}) ->
    {noreply, State}; % Ignore as this is perfectly normal in this case 

handle_info({'EXIT', Port, Reason}, State = #state{erlang_port = Port}) ->
    {stop, Reason, State};

%%% If the owning process dies there is no reason to go on
handle_info({'DOWN', _Ref, _Type, _Process, normal}, State) ->
    {stop, normal, State#state{reply_to = undefined}};
    
handle_info({'DOWN', _Ref, _Type, _Process, timeout}, State) ->
    {stop, normal, State#state{reply_to = undefined}};

handle_info({'DOWN', _Ref, _Type, _Process, shutdown}, State) ->
    {stop, normal, State#state{reply_to = undefined}};
 
handle_info({'DOWN', _Ref, _Type, Process, Reason}, State) ->
    {stop, {stopped, {'EXIT', Process, Reason}}, 
     State#state{reply_to = undefined}};

handle_info({tcp_closed, Socket}, State = #state{odbc_socket=Socket,
						 state = disconnecting}) ->
    {stop, normal, State};
%---------------------------------------------------------------------------
%% Catch all - throws away unknown messages (This could happen by "accident"
%% so we do not want to crash, but we make a log entry as it is an
%% unwanted behaviour.) 
handle_info(Info, State) ->
    Report = io_lib:format("ODBC: received unexpected info: ~p~n", [Info]),
    error_logger:error_report(Report),
    {noreply, State}.

%%-------------------------------------------------------------------------
%% terminate/2 and code_change/3
%%--------------------------------------------------------------------------

-doc false.
terminate({port_exit, _Reason}, State = #state{reply_to = undefined}) ->
    %% Port program crashed
    gen_tcp:close(State#state.odbc_socket),
    gen_tcp:close(State#state.sup_socket),
    ok;

terminate(_Reason,  State = #state{reply_to = undefined}) ->

    catch gen_tcp:send(State#state.sup_socket, 
		       [?SHUTDOWN, ?STR_TERMINATOR]),
    catch gen_tcp:close(State#state.odbc_socket),
    catch gen_tcp:close(State#state.sup_socket),
    catch port_close(State#state.erlang_port),
    ok;

terminate(Reason, State = #state{reply_to = From}) ->
    gen_server:reply(From, {error, connection_closed}),
    terminate(Reason, State#state{reply_to = undefined}).

%---------------------------------------------------------------------------
-doc false.
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%%========================================================================
%%% Internal functions
%%%========================================================================

connect(ConnectionReferense, ConnectionStr, Options) ->
    {C_AutoCommitMode, ERL_AutoCommitMode} = 
	connection_config(auto_commit, Options),
    TimeOut = connection_config(timeout, Options),
    {C_TraceDriver, _} = connection_config(trace_driver, Options),
    {C_SrollableCursors, ERL_SrollableCursors} = 
	connection_config(scrollable_cursors, Options),
    {C_TupleRow, _} = 
	connection_config(tuple_row, Options),
    {BinaryStrings, _} = connection_config(binary_strings, Options),
    {ExtendedErrors, _} = connection_config(extended_errors, Options),

    ODBCCmd = 
	[?OPEN_CONNECTION, C_AutoCommitMode, C_TraceDriver, 
	 C_SrollableCursors, C_TupleRow, BinaryStrings, ExtendedErrors, ConnectionStr],
    
    %% Send request, to open a database connection, to the control process.
    case call(ConnectionReferense, 
	      {connect, ODBCCmd, ERL_AutoCommitMode, ERL_SrollableCursors},
	      TimeOut) of
	ok ->
	    {ok, ConnectionReferense};
	Error ->
	    Error
    end.

%%-------------------------------------------------------------------------
odbc_send(Socket, Msg) -> %% Note currently all allowed messages are lists
    NewMsg = Msg ++ [?STR_TERMINATOR],
    ok = gen_tcp:send(Socket, NewMsg),
    ok = inet:setopts(Socket, [{active, once}]).

%%--------------------------------------------------------------------------
connection_config(Key, Options) ->
    case lists:keysearch(Key, 1, Options) of
	{value,{Key, on}} ->
	    {?ON, on};
	{value,{Key, off}} ->
	    {?OFF, off};
	{value,{Key, Value}} ->
	    Value;
	_ ->
	    connection_default(Key)
    end.

%%--------------------------------------------------------------------------
connection_default(auto_commit) ->
    {?ON, on};

connection_default(timeout) ->
    ?DEFAULT_TIMEOUT;

connection_default(tuple_row) ->
  {?ON, on};

connection_default(trace_driver) ->
    {?OFF, off};

connection_default(scrollable_cursors) ->
    {?ON, on};
connection_default(binary_strings) ->
    {?OFF, off};
connection_default(extended_errors) ->
    {?OFF, off}.

%%-------------------------------------------------------------------------
call(ConnectionReference, Msg, Timeout) ->
    
    Result = (catch gen_server:call(ConnectionReference, 
				    {self(), Msg, Timeout}, infinity)),
    case Result of
	%% Normal case, the result from the port-program has directly 
	%% been forwarded to the client
	Binary when is_binary(Binary) -> 
	     decode(Binary);
	timeout -> 
	    exit(timeout);
	{'EXIT', _} ->
	    {error, connection_closed};
	%% At some occasions the erlang control process will have an
	%% answer that was not directly received from the port-program.
	Term ->  
	    Term
    end.    

%%-------------------------------------------------------------------------
decode(Binary) ->
    case binary_to_term(Binary) of
	[ResultSet | []] -> 
	    ResultSet;
	param_badarg ->
	    exit({badarg, odbc, param_query, 'Params'}); 
	MultipleResultSets_or_Other ->
	    MultipleResultSets_or_Other
    end.

%%-------------------------------------------------------------------------
param_values(Params) ->
    case Params of    
	[{_, Values} | _] -> 
	    Values;
	[{_, _, Values} | _] -> 
	    Values;
	[] -> 
	    []
    end.

%%-------------------------------------------------------------------------
fix_params({sql_integer, InOut, Values}) ->
    {?USER_INT, fix_inout(InOut), [256 | Values]};
fix_params({sql_smallint, InOut, Values}) ->
    {?USER_SMALL_INT, fix_inout(InOut), [256 | Values]};
fix_params({sql_tinyint, InOut, Values}) ->
    {?USER_TINY_INT, fix_inout(InOut), [256 | Values]};
fix_params({{sql_decimal, Precision, 0}, InOut, 
 	    Values}) when Precision >= 0, Precision =< 9 ->
    {?USER_DECIMAL, Precision, 0, fix_inout(InOut), [256 | Values]};
fix_params({{sql_decimal, Precision, Scale}, InOut, Values}) ->
    {?USER_DECIMAL, Precision, Scale, fix_inout(InOut), Values};
fix_params({{sql_numeric, Precision, 0}, InOut, 
 	    Values}) when Precision >= 0, Precision =< 9 ->
    {?USER_NUMERIC, Precision, 0, fix_inout(InOut), [256 | Values]};
fix_params({{sql_numeric, Precision, Scale}, InOut, Values}) ->
        {?USER_NUMERIC, Precision, Scale, fix_inout(InOut), Values};
fix_params({{sql_char, Max}, InOut, Values}) ->
     NewValues = string_terminate(Values),
    {?USER_CHAR, Max, fix_inout(InOut), NewValues};
fix_params({{sql_varchar, Max}, InOut, Values}) ->
     NewValues = string_terminate(Values),
    {?USER_VARCHAR, Max, fix_inout(InOut), NewValues};
fix_params({{sql_wchar, Max}, InOut, Values}) ->
    NewValues = string_terminate(Values),
    {?USER_WCHAR, Max, fix_inout(InOut), NewValues};
fix_params({{sql_wvarchar, Max}, InOut, Values}) ->
    NewValues = string_terminate(Values),
    {?USER_WVARCHAR, Max, fix_inout(InOut), NewValues};
fix_params({{sql_wlongvarchar, Max}, InOut, Values}) ->
    NewValues = string_terminate(Values),
    {?USER_WLONGVARCHAR, Max, fix_inout(InOut), NewValues};
fix_params({{sql_float, Precision}, InOut, Values}) ->
    {?USER_FLOAT, Precision, fix_inout(InOut), Values};
fix_params({sql_real, InOut, Values}) ->
    {?USER_REAL, fix_inout(InOut), Values};
fix_params({sql_double, InOut, Values}) ->
    {?USER_DOUBLE, fix_inout(InOut), Values};
fix_params({sql_bit, InOut, Values}) ->
    {?USER_BOOLEAN, fix_inout(InOut), Values};
fix_params({'sql_timestamp', InOut, Values}) ->
    NewValues =
 	case (catch 
		  lists:map(
		    fun({{Year,Month,Day},{Hour,Minute,Second}}) ->
			    {Year,Month,Day,Hour,Minute,Second};
		       (null) -> null
		    end, Values)) of
 	    Result ->
 		Result
 	end,
    {?USER_TIMESTAMP, fix_inout(InOut), NewValues};
%% default is IN %%%
fix_params({Type, Values}) ->
    fix_params({Type, in, Values}).

fix_inout(in) ->
    ?IN;
fix_inout(out) ->
    ?OUT;
fix_inout(inout) ->
    ?INOUT.

string_terminate(Values) ->
    case (catch lists:map(fun string_terminate_value/1, Values)) of
	Result ->
	    Result
    end.

string_terminate_value(String) when is_list(String) ->
    String ++ [?STR_TERMINATOR];
string_terminate_value(Binary) when is_binary(Binary) ->
    <<Binary/binary,0:16>>;
string_terminate_value(null) ->
    null.

port_timeout() ->
  application:get_env(?MODULE, port_timeout, ?ODBC_PORT_TIMEOUT).
