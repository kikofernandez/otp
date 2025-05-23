%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2003-2025. All Rights Reserved.
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

-module(xmerl).
-moduledoc "Functions for exporting XML data to an external format.".

%-compile(export_all).

-export([export/2,
	 export/3,
	 export_content/2,
	 export_element/2,
	 export_element/3,
	 export_simple/2,
	 export_simple/3,
	 export_simple_element/2,
	 export_simple_content/2,
	 callbacks/1]).

-include("xmerl.hrl").
-include("xmerl_internal.hrl").

%% xmerl.hrl
%%
%% Records generated by the scanner
%%
-export_type(
   [xmlDecl/0,
    xmlAttribute/0,
    xmlNamespace/0,
    xmlNsNode/0,
    xmlElement/0,
    xmlText/0,
    xmlComment/0,
    xmlPI/0,
    xmlDocument/0]).
%%
-doc "Record `#xmlDecl{}`.".
-type xmlDecl() :: #xmlDecl{}.
%%
-doc "Record `#xmlAttribute{}`.".
-type xmlAttribute() :: #xmlAttribute{}.
%%
-doc "Record `#xmlNamespace{}`.".
-type xmlNamespace() :: #xmlNamespace{}.
%%
-doc "Record `#xmlNsNode{}`.".
-type xmlNsNode() :: #xmlNsNode{}.
%%
-doc "Record `#xmlElement{}`.".
-type xmlElement() :: #xmlElement{}.
%%
-doc "Record `#xmlText{}`.".
-type xmlText() :: #xmlText{}.
%%
-doc "Record `#xmlComment{}`.".
-type xmlComment() :: #xmlComment{}.
%%
-doc "Record `#xmlPI{}`.".
-type xmlPI() :: #xmlPI{}.
%%
-doc "Record `#xmlDocument{}`.".
-type xmlDocument() :: #xmlDocument{}.

-doc "A callback module or a list of inherited callback modules.".
-type callback() :: module() | [module()].

-export_type([element/0]).
-doc "Normal, well-formed, XML content element.".
-type element() ::
        xmlText() | xmlElement() | xmlPI() | xmlComment() | xmlDecl().

-doc ~s`"Simple-form" XML content element.`.
-type simple_element() ::
        {Tag :: atom(),
         Attributes :: [{Name ::atom(),
                         Value :: iolist() | atom() | integer()}],
         Content :: [simple_element()]} |
        {Tag :: atom(), Content :: [simple_element()]} |
        Tag :: atom() | IOString :: iolist() | element().

-doc ~s`"Simple-form" XML attribute.`.
-type simple_attribute() ::
        xmlAttribute() |
        {fun((_) -> _), term()}  |
        {Key :: atom(), Value :: term()}.

-doc(#{equiv => export(Content, Callback, [])}).
-spec export(Content :: term(), Callback :: term()) ->
          ExportedFormat :: term().
export(Content, Callback) ->
    export(Content, Callback, []).


-doc """
Export XML content.

Exports normal, well-formed XML content, using the specified callback module.

`Element` is any of:

- `#xmlText{}`
- `#xmlElement{}`
- `#xmlPI{}`
- `#xmlComment{}`
- `#xmlDecl{}`

(See `xmerl.hrl` for the record definitions.) Text in `#xmlText{}` elements can
be deep lists of characters and/or binaries.

`RootAttributes` is a list of `#xmlAttribute{}` attributes for the `#root#`
element, which implicitly becomes the parent of the given `Content`. The
tag-handler function for `#root#` is thus called with the complete exported data
of `Content`. Root attributes can be used to specify e.g. encoding or other
metadata of an XML or HTML document.

The `Callback` module should contain hook functions for all tags present in the
data structure. A hook function must have the following format:

```text
    Tag(Data, Attributes, Parents, E)
```

where `E` is the corresponding `#xmlElement{}`, `Data` is the already-exported
contents of `E` and `Attributes` is the list of `#xmlAttribute{}` records of
`E`. Finally, `Parents` is the list of parent nodes of `E`, on the form
`[{ParentTag::atom(), ParentPosition::integer()}]`.

The hook function should return either the data to be exported, or a tuple
`{'#xml-alias#', NewTag::atom()}`, or a tuple `{'#xml-redefine#', Content}`,
where `Content` is a content list (which can be on simple-form; see
[`export_simple/2`](`export_simple/2`) for details).

A callback module can inherit definitions from other callback modules, through
the required function `'#xml-interitance#'() -> [ModuleName::atom()]`.

_See also:_ `export/2`, `export_simple/3`.
""".
-spec export(Content, Callback, RootAttributes) ->
          ExportedFormat :: term() when
      Content        :: [Element],
      Element        :: element(),
      Callback       :: callback(),
      RootAttributes :: [simple_attribute()].
export(Content, Callback, RootAttributes) when is_atom(Callback) ->
    export1(Content, callbacks(Callback), RootAttributes);
export(Content, Callbacks, RootAttrs) when is_list(Callbacks) ->
    export1(Content, Callbacks, RootAttrs).


-doc(#{equiv => export_simple(Content, Callback, [])}).
-spec export_simple(Content, Callback) ->
          ExportedFormat :: term() when
      Content        :: [Element],
      Element        :: simple_element(),
      Callback       :: callback().
export_simple(Content, Callback) ->
    export_simple(Content, Callback, []).


-doc """
Export "simple-form" XML content.

Exports "simple-form" XML content, using the specified callback-module.

`Element` is any of:

- `{Tag, Attributes, Content}`
- `{Tag, Content}`
- `Tag`
- `IOString`
- `#xmlText{}`
- `#xmlElement{}`
- `#xmlPI{}`
- `#xmlComment{}`
- `#xmlDecl{}`

where

- `Tag = atom()`
- `Attributes = [{Name, Value}]`
- `Name = atom()`
- `Value = IOString | atom() | integer()`

Normal-form XML elements can thus be included in the simple-form representation.
Note that content lists must be flat. An `IOString` is a (possibly deep) list of
characters and/or binaries.

`RootAttributes` is a list of:

- `XmlAttributes = #xmlAttribute{}`

See [`export/3`](`export/3`) for details on the callback module and the root
attributes. The XML-data is always converted to normal form before being passed
to the callback module.

_See also:_ `export/3`, `export_simple/2`.
""".
-spec export_simple(Content, Callback, RootAttributes) ->
          ExportedFormat :: term() when
      Content        :: [Element],
      Element        :: simple_element(),
      Callback       :: callback(),
      RootAttributes :: [simple_attribute()].
export_simple(Content, Callback, RootAttrs) when is_atom(Callback) ->
    export_simple1(Content, callbacks(Callback), RootAttrs);
export_simple(Content, Callbacks, RootAttrs) when is_list(Callbacks) ->
    export_simple1(Content, Callbacks, RootAttrs).

export_simple1(Content, Callback, RootAttrs) ->
    export1(xmerl_lib:expand_content(Content), Callback, RootAttrs).

%% This exports proper XML content in root context.

export1(Content, Callbacks, RootAttrs) when is_list(Content) ->
    Result = export_content(Content, Callbacks),
    Attrs = xmerl_lib:expand_attributes(RootAttrs, 1, [{'#root#',1}]),
    Root = #xmlElement{name = '#root#',
		       pos = 1,
		       parents = [],
		       attributes = Attrs},
    Args = [Result, Root#xmlElement.attributes, [], Root],
    tagdef('#root#',1,[],Args,Callbacks).


-doc "Exports simple XML content directly, without further context.".
-spec export_simple_content(Content, Callback) -> _ when
      Content  :: [simple_element()],
      Callback :: callback().
export_simple_content(Content, Callback) when is_atom(Callback) ->
    export_content(xmerl_lib:expand_content(Content),
		   callbacks(Callback));
export_simple_content(Content, Callbacks) when is_list(Callbacks) ->
    export_content(xmerl_lib:expand_content(Content), Callbacks).


-doc """
Export normal XML content directly, without further context.
""".
-spec export_content(Content, Callbacks) -> _ when
	Content   :: [element()],
	Callbacks :: [module()].
export_content([#xmlText{value = Text, type = text} | Es], Callbacks) ->
    [apply_text_cb(Callbacks, Text) | export_content(Es, Callbacks)];
export_content([#xmlText{value = Text, type = cdata} | Es], Callbacks) ->
    [apply_cdata_cb(Callbacks, Text) | export_content(Es, Callbacks)];
export_content([#xmlPI{} | Es], Callbacks) ->
    export_content(Es, Callbacks);
export_content([#xmlComment{} | Es], Callbacks) ->
    export_content(Es, Callbacks);
export_content([#xmlDecl{} | Es], Callbacks) ->
    export_content(Es, Callbacks);
export_content([E | Es], Callbacks) ->
    [export_element(E, Callbacks) | export_content(Es, Callbacks)];
export_content([], _Callbacks) ->
    [].

%% @doc Exports a simple XML element directly, without further context.

-doc "Export a simple XML element directly, without further context.".
-spec export_simple_element(Element, Callback) -> _ when
	Element  :: simple_element(),
	Callback :: callback().
export_simple_element(Element, Callback) when is_atom(Callback) ->
    export_element(xmerl_lib:expand_element(Element),
		   callbacks(Callback));
export_simple_element(Element, Callbacks) when is_list(Callbacks) ->
    export_element(xmerl_lib:expand_element(Element), Callbacks).

-doc "Exports a normal XML element directly, without further context.".
-spec export_element(Element, Callback) -> _ when
      Element  :: element(),
      Callback :: callback().
export_element(E, CB) when is_atom(CB) ->
    export_element(E, callbacks(CB));
export_element(#xmlText{value = Text, type = text}, CBs) ->
    apply_text_cb(CBs, Text);
export_element(#xmlText{value = Text, type = cdata}, CBs) ->
    apply_cdata_cb(CBs, Text);
export_element(E = #xmlElement{name = Tag,
			       pos = Pos,
			       attributes = Attributes,
			       parents = Parents,
			       content = Content}, CBs) ->
    Data = export_content(Content, CBs),
    Args = [Data, Attributes, Parents, E],
    tagdef(Tag,Pos,Parents,Args,CBs);
export_element(#xmlPI{}, _CBs) ->
    [];
export_element(#xmlComment{}, _CBs) ->
    [];
export_element(#xmlDecl{}, _CBs) ->
    [].


-doc """
For on-the-fly exporting during parsing (SAX style) of the XML document.
""".
-spec export_element(Element, Callback, CallbackState) ->
          ExportedFormat when
      Element        :: element(),
      Callback       :: callback(),
      CallbackState  :: term(),
      ExportedFormat :: term().
export_element(E, CallbackModule, CallbackState) when is_atom(CallbackModule) ->
    export_element(E, callbacks(CallbackModule), CallbackState);
export_element(#xmlText{value = Text, type = text},CallbackModule,_CallbackState) ->
%%    apply_cb(CallbackModule, '#text#', '#text#', [Text,CallbackState]);
    apply_text_cb(CallbackModule,Text);
export_element(#xmlText{value = Text, type = cdata},CallbackModule,_CallbackState) ->
    apply_cdata_cb(CallbackModule,Text);
export_element(E=#xmlElement{name = Tag,
			   pos = Pos,
			   parents = Parents,
			   attributes = Attributes,
			   content = Content},Callbacks,CBstate) ->
    Args = [Content, Attributes,CBstate,E],
    tagdef(Tag,Pos,Parents,Args,Callbacks);
export_element(#xmlPI{}, _CallbackModule, CallbackState) ->
    CallbackState;
export_element(#xmlComment{},_CallbackModule, CallbackState) ->
    CallbackState;
export_element(#xmlDecl{},_CallbackModule, CallbackState) ->
    CallbackState.

%% A thing returned with #xml-redefine is assumed to be a content list
%% The data may be on "simple" format.

tagdef(Tag,Pos,Parents,Args,CBs) ->
    case apply_tag_cb(CBs, Tag, Args) of
	{'#xml-alias#', NewTag} ->
	    tagdef(NewTag,Pos,Parents,Args,CBs);
	{'#xml-redefine#', Data} ->
	    export_content(xmerl_lib:expand_content(Data, Pos, Parents),
			   CBs);
	Other ->
	    Other
    end.


-doc "Find the list of inherited callback modules for a given module.".
-spec callbacks(Module :: module()) -> [module()].
callbacks(Module) ->
    Result = check_inheritance(Module, []),
%%%     ?dbg("callbacks = ~p~n", [lists:reverse(Result)]),
    lists:reverse(Result).

callbacks([M|Mods], Visited) ->
    case lists:member(M, Visited) of
	false ->
	    NewVisited = check_inheritance(M, Visited),
	    callbacks(Mods, NewVisited);
	true ->
	    exit({cyclic_inheritance, {M, hd(Visited)}})
    end;
callbacks([], Visited) ->
    Visited.

check_inheritance(M, Visited) ->
%%%     ?dbg("calling ~p:'#xml-inheritance#'()~n", [M]),
    case M:'#xml-inheritance#'() of
	[] ->
	    [M|Visited];
	Mods ->
	    callbacks(Mods, [M|Visited])
    end.

apply_text_cb(Ms, Text) ->
    apply_cb(Ms, '#text#', '#text#', [Text]).

apply_cdata_cb(Ms, Text) ->
    apply_cb(Ms, '#cdata#', '#cdata#', [Text]).

apply_tag_cb(Ms, F, Args) ->
    apply_cb(Ms, F, '#element#', Args).

apply_cb(Ms, F, Df, Args) ->
    apply_cb(Ms, F, Df, Args, length(Args)).

apply_cb(Ms, F, Df, Args, A) ->
    apply_cb(Ms, F, Df, Args, A, Ms).

apply_cb([M|Ms], F, Df, Args, A, Ms0) ->
    case erlang:function_exported(M, F, A) of
        true -> apply(M, F, Args);
        false -> apply_cb(Ms, F, Df, Args, A, Ms0)
    end;
apply_cb([], Df, Df, Args, _A, _Ms0) ->
    exit({unknown_tag, {Df, Args}});
apply_cb([], F, Df, Args, A, Ms0) ->
    apply_cb(Ms0, Df, Df, [F|Args], A+1).
