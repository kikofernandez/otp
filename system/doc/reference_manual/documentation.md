# Documentation

Documentation in Erlang is done through the `-moduledoc` and `-doc` [attributes][]. For example:

    -module(math).
    -moduledoc "
    A module for basic arithmetic.
    ".
    
    -export([add/2]).
    
    -doc "Adds two numbers together."
    add(One, Two) -> One + Two.

The `-doc` attribute always precedes the function/type/callback it documents.
The `-moduledoc` attribute has to be located before any other `-doc` attribute.

By default the format used for documentation attributes is [Markdown][wikipedia]
but that can be changed by setting [module documentation metadata](#moduledoc-metadata).

A good starting point to writing Markdown is [Basic writing and formatting syntax][github].

For details on what is allowed to be part of the `-moduledoc` and `-doc` attributes, see
[Documentation Attributes][doc_attrs].

[attributes]: system/reference_manual:modules#module-attributes
[Earmark]: https://github.com/robertdober/earmark_parser
[wikipedia]: https://en.wikipedia.org/wiki/Markdown
[github]: https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax
[doc_attrs]: system/reference_manual:modules#documentation-attributes

## Documentation metadata

It is possible to add metadata to the documentation entry. You do this by adding
a `-moduledoc` or `-doc` attribute with a map as argument. For example:

    -module(math).
    -moduledoc "
    A module for basic arithmetic.
    ".
    -moduledoc #{ since => "1.0" }.
    
    -export([add/2]).
    
    -doc "Adds two number together."
    -doc(#{ since => "1.0" }).
    add(One, Two) -> One + Two.

The metadata is used by documentation tools to provide extra information to
the user. You can have multiple metadata documentation entries, if you do
then the maps will be merged with the latest taking precedence if there are
duplicate keys. Example:

    -doc "Adds two number together."
    -doc #{ since => "1.0", author => "Joe" }.
    -doc #{ since => "2.0" }.
    add(One, Two) -> One + Two.

This will result in a metadata entry of `#{ since => "2.0", author => "Joe" }`.

The keys in the metadata map can be any type, but it is recommended that only atoms
are used.

## External documentation files

You do not have to have the documentation inline, if you want to you can also place
it in a file somewhere else. If you do so, then you can use `-doc {file, "path/to/doc.md"}`
to point to the documentation. The path used is relative to the file where the `-doc` attribute
is located. For example:

    %% doc/add.md
    Adds two numbers together

and

    %% src/math.erl
    -doc({file,"../doc/add.md"}).
    add(One, Two) -> One + Two.

## Documenting a module

The module description should include details on how to use the API and examples
of the different functions working together. Here is a good place to use images
and other diagrams to better show the usage of the module.

The `moduledoc` should start with a short paragraph describing the module and then
go into greater details. For example:

    -module(math).
    -moduledoc """
       A module for basic arithmetic.
       
       This module can be used to add and subtract values. For example:
       
       ```
       1> math:substract(math:add(2, 3), 1).
       4
       ```
       """.

### Moduledoc metadata

There are three reserved metadata keys for `-moduledoc`:

- `since` - Shows which version of the application the module was added.
- `deprecated` - Shows a text in the documentation explaining that it is deprecated and what to use instead.
- `format` - The format to use for all documentation in this module. The default is `text/markdown`.
  It should be written using the [mime type][] of the format.

Example:

    -moduledoc {file, "../doc/math.asciidoc"}.
    -moduledoc #{ since => "0.1", format => "text/asciidoc" }.
    -moduledoc #{ deprecated => "Use the stdlib math module instead." }.

[mime type]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types

## Documenting a function / type / opaque / callback

You can document functions, type and callbacks using the `-doc` attribute.
Each entry should start with a short paragraph describing the function/type/opaque/callback
and then go into greater detail in needed.

It is not recommended to include images or diagrams in this documentation as
it is used by IDEs and [c:h/1][] to show the documentation to the user.

For example:

    -doc "
    A number that can be used by the math module.
    
    We use a special number here so that we know
    that this number comes from this module.
    ".
    -type number() :: {math, erlang:number()}.
    
    -doc "
    Adds two number together.
    
    ### Example:
    
    ```
    1> math:add(math:number(1), math:number(2)).
    {number, 3}
    ```
    "
    -spec add(number(), number()) -> number().
    add({number, One}, {number, Two}) -> {number, One + Two}.

[c:h/1]: seemfa/stdlib:c#h/1

### Doc metadata

There are three reserved metadata keys for `-moduledoc`:

- `since` - Shows which version of the application the module was added.
- `deprecated` - Shows a text in the documentation explaining that it is deprecated and what to use instead.
- `equiv` - Notes that this function is equivalent to another function in this module.
    You can use either `Func/Arity` or `Func(Args)` to describe the equivalence. For example:

        -doc #{ equiv => add/3 }.
        add(One, Two) -> add(One, Two, []).
        add(One, Two, Options) -> ...
    
    or
    
        -doc #{ equiv => add(One, Two, []) }.
        -spec add(One :: number(), Two :: number()) -> number().
        add(One, Two) -> add(One, Two, []).
        add(One, Two, Options) -> ...
    
    If no documentation exists, documentation will be generated with a text pointing
    the user to `add/3`. In the second example there will be an addition check done
    by the compiler that `One` and `Two` are variable names in the `-spec` of the
    function.
- `exported` - A [boolean/0][] signifying if the entry is `exported` or not. For any `-type`
    attribute this value is automatically set by the compiler to show if the type was exported
    or not.

[boolean/0]: seetype/erts:erlang#boolean

### Doc slogans

The doc slogan is a short text shown to describe the function when hovering.
By default it is taken from the source code by looking at the names of the
arguments:

    add(One, Two) -> One + Two.

will have a slogan of `add(One, Two)`. For types/opaques/callbacks,
the implementation will look at the type/opaque/callback
specification for what to use as slogan. For example:

    -type number(Value) :: {number, Value}.
    %% slogan will be `number(Value)`
    
    -opaque number() :: {number, number()}.
    %% slogan will be `number()`
    
    -callback increment(In :: number()) -> Out.
    %% slogan will be `increment(In)`
    
    -callback increment(In) -> Out when
       In :: number().
    %% slogan will be `increment(In)`

If it cannot "easily" figure out a nice slogan from the code, it will use the
MFA syntax instead, i.e. `add/2`, `number/1`, `increment/1` etc.

It is possible to supply your own slogan by placing it as the first line of
the `-doc` attribute. The provided slogan must be in the form of a function
declaration up until the `->`. For example:

    -doc "
    add(One, Two)
    
    Adds two numbers.
    "
    add(A, B) -> A + B.

Will create the slogan `add(One, Two)`. This works for functions, types, opaques
and callbacks.

## Links

When writing documentation in Markdown links are automatically found in any
inline code segment that looks like an MFA. For example:

    -doc "See `sub/2` for more details"

will create a link to the `sub/2` function in the current module if it exists.
You can also use `` `sub/2` `` as the link target. For example:

    -doc "See [subtract](`sub/2`) for more details"
    -doc "See [`sub/2`] for more details"
    -doc "See [subtract] for more details

    [substract]: `sub/2`
    "
    -doc "See [subtract][1] for more details
    
    [1]: `sub/2`
    "

The above will all result in the same link being created.

The link can also be used to point many other things:

- `remote functions` - Use `module:function/arity` syntax.
  
  Example:
  
      -doc "See `math:sub/2` for more details".
  
- `modules` - Write the module with a `m` prefix. You can also use anchors to
  jump to a specific place in the module.
  
  Example:
  
      -doc "See `m:math` for more details".
      -doc "See `m:math#anchor` for more details".

- `types` - Use same syntax as for local/remote function only add a `t` prefix.
  
  Example:
  
      -doc "See `t:number/0` for more details".
      -doc "See `t:math:number/0` for more details".

- `callbacks` - Use same syntax as for local/remote function only add a `c` prefix.
  
  Example:
  
      -doc "See `c:increment/0` for more details".
      -doc "See `c:math:increment/0` for more details".

- `extra pages` - For extra pages in the current application just use a normal link,
  i.e. "`[release notes](notes.md)`".
  For extra pages in another application you need to use the `e` prefix and state which
  application the page belongs to. You can also use anchors to jump to a specific
  place in the page.
  
  Example:
    
      -doc "See `e:stdlib:unicode_usage` for more details".
      -doc "See `e:stdlib:unicode_usage#notes-about-raw-filenames` for more details".

## What is visible vs hidden?

An Erlang [application][] normally consists of various public and private modules. That is
modules that should be used by other applications and modules that should not. By default
all modules in an application are visible, but by setting `-moduledoc false.` you can hide
specific modules from being listed as part of the available API.

An Erlang [module][] consists of public and private functions and type attributes.
By default, all exported functions, exported types and callbacks are considered
visible and part of the modules public API. In addition, any non-exported
type that is referred to by any other visible type attribute is also visible,
but not considered to be part of the public API. For example:

    -export([example/0]).
    
    -type private() :: one.
    -spec example() -> private().
    example() -> one.

in the above code, the function `example/0` is exported and it referenced the
un-exported type `private/0`. Therefore both `example/0` and `private/0` will
marked as visible. The `private/0` type will have the metadata field `exported`
set to `false` to show that it is not part of the public API.

If you want to make a visible entity hidden you need to set the `-doc` attribute to
`false`. Lets revisit out previous example:

    -export([example/0]).
    
    -type private() :: one.
    -spec example() -> private().
    -doc false.
    example() -> one.

Now, the function `example/0` is exported but explicitly marked as hidden, so therefore
both `example/0` and `private/0` will be hidden.

Any documentation added to an automatically hidden entity (non-exported function or type)
is ignored and will generate a warning. You should use comments to document such functions.

[application]: seeerl/kernel:application
[module]: modules

## Compiling and getting documentation

The Erlang compiler has support for compiling the documentation into [EEP-48][EEP-48]
documentation chunks by passing the `beam_docs` flag to [compile:file/1][], or
`+beam_docs` to [erlc][].

The documentation can then be retrieved using [code:get_doc/1][], or viewed using the
shell built-in command [h()][c:h/1]. For example:

    1> h(math).
    
          math
    
      A module for basic arithmetic.
    
    2> h(math, add).
    
          add(One, Two)
    
      Adds two numbers together.

[EEP-48]: kernel:eep48_chapter
[compile:file/1]: seemfa/compiler:compile#file/1
[erlc]: seecom/erts:erlc
[code:get_doc/1]: seemfa/kernel:code#get_doc/1

## Using ExDoc to generate HTML/ePub documentation

[ExDoc][] has built-in support to generate documentation from Markdown. The simplest
way to use it is by using the [rebar3_ex_doc][] plugin. To setup a rebar3 project to
use [ExDoc][] to generate documentation simply add the following to your `rebar3.config`.

    %% Enable the plugin
    {plugins, [rebar3_ex_doc]}.
    
    %% Configure the compiler to emit documentation
    {profiles, [{docs, [{erl_opts, [beam_docs]}]}]}.
    
    {ex_doc, [
     {extras, ["README.md"]},
     {main, "README.md"},
     {source_url, "https://github.com/namespace/your_app"}
    ]}.

When configured you can run `rebar3 ex_doc` and the documentation will be generated to
`doc/index.html`. For more details and options see the [rebar3_ex_doc][] documentation.

You can also download the [release escript bundle][ex_doc_escript] from github and
run it from the command line. The documentation for how to use the escript can be
found by running `ex_doc --help`.

If you are writing documentation that will be using [ExDoc][] to generate HTML/ePub
it is highly recommended that you read its documentation.

[ExDoc]: https://hexdocs.pm/ex_doc/
[rebar3_ex_doc]: https://hexdocs.pm/rebar3_ex_doc
[ex_doc_escript]: https://github.com/elixir/ex_doc/releases/latest
[Earmark]: https://hexdocs.pm/earmark_parser
