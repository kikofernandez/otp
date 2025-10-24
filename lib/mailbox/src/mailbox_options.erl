-module(mailbox_options).

-include("../include/mailbox.hrl").

-export([set_config/1, mode/1]).

-spec set_config(checker_options()) -> ok.
set_config(#option{}=Option) ->
    erlang:put(config, Option).

-spec mode(checker_options()) -> mode().
mode(#option{}=Options) ->
    Options#option.mode.
