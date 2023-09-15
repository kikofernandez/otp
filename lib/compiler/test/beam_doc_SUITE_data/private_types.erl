-module(private_types).

-export([bar/0, hidden/0]).
-export_type([public_t/0, hidden_export_t/0]).

-type private_t() :: integer(). %% In chunk because referred to by exported bar/0
-type public_t() :: integer(). %% In chunk because exported
-type local_t() :: integer(). %% Not in chunk because only referred by non-exported function

-doc false.
-type hidden_export_t() :: integer(). %% In chunk because exported

-spec bar() -> private_t().
bar() -> baz().

-spec baz() -> local_t().
baz() -> 1.

-type hidden_t() :: integer(). %% Not in chunk because only referred to by hidden function

-doc false.
-spec hidden() -> hidden_t().
hidden() -> 1.
