-module(analysis).

-moduledoc """
Helper module with functions to access the analysis record.
""".

-include("../include/mailbox.hrl").

-export([forms/1, set_forms/2, push_error/3, errors/1, set_mode/2, mode/1]).
-export([file/1, set_file/2]).

-spec file(#analysis{}) -> file:filename().
file(Analysis) ->
    Analysis#analysis.file.

-spec set_file(#analysis{}, file:filename()) -> #analysis{}.
set_file(Analysis, File) ->
    Analysis#analysis{file = File}.

-spec mode(#analysis{}) -> mode().
mode(Analysis) ->
    Analysis#analysis.mode.

-spec set_mode(#analysis{}, mode()) -> #analysis{}.
set_mode(Analysis, Mode) when Mode == ?CHECK_MODE;
                              Mode == ?DEBUG_MODE ->
    Analysis#analysis{mode = Mode}.

-spec set_forms(Analysis :: #analysis{}, Forms :: list()) -> #analysis{}.
set_forms(Analysis, Forms) ->
    Analysis#analysis{forms = Forms}.

-spec forms(#analysis{}) -> list().
forms(#analysis{forms = Forms}) ->
    Forms.

-spec push_error(Error, Form, Analysis) -> Result when
      Error    :: error(),
      Form     :: {error() | ?NEW_MB | ?USE_MB, dynamic()},
      Analysis :: #analysis{},
      Result   :: #analysis{}.
push_error(Error, Form, #analysis{errors = Errors}=Analysis) ->
    Analysis#analysis{errors = [{Error, Form} | Errors]}.

-spec errors(#analysis{}) -> list().
errors(#analysis{errors = Errors}) ->
    Errors.
