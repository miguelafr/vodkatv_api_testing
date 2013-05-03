-module(vaa_parser_xml).

-export([parse/1, format/1]).

-include_lib("xmerl/include/xmerl.hrl").

parse(Xml)->
    try
	Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
		      {Acc, P, S};
		 (X, Acc, S) ->
		      {[X|Acc], S}
	      end,
	case xmerl_scan:string(Xml, [{space,preserve}, {acc_fun, Acc}]) of
	    {error, Reason} ->
		{error, Reason};
	    {Xs, _} ->
		{ok, to_erlang(Xs)}
	end
    catch
	_:Error ->
	    {error, Error}
    end.

format(X)->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ++
    do_format(X).

do_format([{Element, Info}])->
    Contents =
	case proplists:get_value(contents, Info) of
	    undefined -> [];
	    C -> C
	end,
    Text =
	case proplists:get_value(text, Info) of
	    undefined -> "";
	    T -> T
	end,
    "<" ++ erlang:atom_to_list(Element) ++ ">" ++
	Text ++
	lists:foldl(
	  fun(E, R)->
		  R ++ do_format([E])
	  end, "", Contents) ++
	"</" ++ erlang:atom_to_list(Element) ++ ">".

to_erlang(Xs) when is_record(Xs, xmlElement) ->
    AttrsResult = {attrs,
		   [{Attr#xmlAttribute.name,
		     [{text, string:strip(Attr#xmlAttribute.value)}]}
		    || Attr <- Xs#xmlElement.attributes]},

    ContentsResult = case Xs#xmlElement.content of
			 [Content | []] when is_record(Content, xmlText) ->
			     to_erlang(Content);
			 Contents ->
			     {contents,
			      lists:map(fun(Content) ->
						[Element] = to_erlang(Content),
						Element
					end, Contents)}
		     end,
    [{Xs#xmlElement.name, [
			   AttrsResult,
			   ContentsResult
			  ]}];

to_erlang(Xs) when is_record(Xs, xmlText) ->
    {text, Xs#xmlText.value}.

