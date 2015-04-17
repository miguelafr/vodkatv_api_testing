%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006,2007,2008 Erlware
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Does a safe parsing of erlang tuple syntax. If it finds an atom it
%%%  uses list_to_existing atom to translate it. This will only work
%%%  if that atom is already in the atom table. This means its a bit quirky
%%%  but it works for what I need.
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt
%%%-------------------------------------------------------------------
-module(ktt_decode).

-compile(export_all).

-export([decode/1, decode/3]).

%%--------------------------------------------------------------------
%% @spec decode(Stream) -> {ParsedTuples, UnparsedRemainder}
%%
%% @doc
%%  Parses the incoming stream into valid erlang objects.
%%  ``
%%   Tuple   ==   Erlang
%%   List        List
%%   String      List
%%   Number      Number
%%   Atom        Atom
%%  ''
%%  This decode function parses a subset of erlang data notation.
%% @end
%%--------------------------------------------------------------------
decode(Stream) ->
   decode(Stream, 0, 0).

decode(Stream, NewLines, Chars) ->
    value(Stream, NewLines, Chars).


%%=============================================================================
%% Internal Functions
%%=============================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Parse a tuple value.
%%
%% @spec value(Stream, NewLines, Chars) -> {Value, Rest, {N, L}}
%% @end
%%--------------------------------------------------------------------
value([$\" | T], NewLines, Chars) ->
    ktuo_parse_utils:stringish_body($\", T, [], NewLines, Chars + 1);
value([$\' | T], NewLines, Chars) ->
    case ktuo_parse_utils:stringish_body($\', T, [], NewLines, Chars + 1) of
        {String, Rest, Pos} ->
            {list_to_existing_atom(String), Rest, Pos};
        Else ->
          Else
    end;
value([$- | T], NewLines, Chars) ->
    ktuo_parse_utils:digit19(T, [$-], NewLines, Chars + 1);
value([$0 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$0], front, NewLines, Chars + 1);
value([$1 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$1], front, NewLines, Chars + 1);
value([$2 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$2], front, NewLines, Chars + 1);
value([$3 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$3], front, NewLines, Chars + 1);
value([$4 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$4], front, NewLines, Chars + 1);
value([$5 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$5], front, NewLines, Chars + 1);
value([$6 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$6], front, NewLines, Chars + 1);
value([$7 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$7], front, NewLines, Chars + 1);
value([$8 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$8], front, NewLines, Chars + 1);
value([$9 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$9], front, NewLines, Chars + 1);
value([$[ | T], NewLines, Chars) ->
    list_body(T, [], NewLines, Chars + 1);
value([${ | T], NewLines, Chars) ->
    tuple_body(T, [], NewLines, Chars + 1);
value([$\s | T], NewLines, Chars) ->
    value(T, NewLines, Chars + 1);
value([$\t | T], NewLines, Chars) ->
    value(T, NewLines, Chars + 1);
value([$\r | T], NewLines, _Chars) ->
    value(T, NewLines + 1, 0);
value([$\n | T], NewLines, _Chars) ->
    value(T, NewLines + 1, 0);
value(Stream, NewLines, Chars) ->
    bare_atom(Stream, [], NewLines, Chars).


%%--------------------------------------------------------------------
%% @doc
%%  Parse a list body. A list is [ elements, ..].
%%
%% @spec list_body(Stream, Acc, NewLines, Chars) -> {List, Rest, {N, L}} | Error
%% @end
%%--------------------------------------------------------------------
list_body([$] | T], Acc, NewLines, Chars) ->
    {lists:reverse(Acc), T, {NewLines, Chars + 1}};
list_body([$, | T], Acc, NewLines, Chars) ->
    list_body(T, Acc, NewLines, Chars + 1);
list_body([$\s | T], Acc, NewLines, Chars) ->
    list_body(T, Acc, NewLines, Chars + 1);
list_body([$\t | T], Acc, NewLines, Chars) ->
    list_body(T, Acc, NewLines, Chars + 1);
list_body([$\n | T], Acc, NewLines, _Chars) ->
    list_body(T, Acc, NewLines + 1, 0);
list_body([$\r | T], Acc, NewLines, _Chars) ->
    list_body(T, Acc, NewLines + 1, 0);
list_body(Stream, Acc, NewLines, Chars) ->
    case value(Stream, NewLines, Chars) of
        {Value, Rest, {N, C}} ->
            list_body(Rest, [Value | Acc], N, C);
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Parse the tuple body. Tuple bodies are of the form { element, ..}.
%%
%% @spec tuple_body(Stream, Acc, NewLines, Chars) -> {Tuple, Rest, {N, C}} | Error
%% @end
%%--------------------------------------------------------------------
tuple_body([$} | T], Acc, NewLines, Chars) ->
    {list_to_tuple(lists:reverse(Acc)), T, {NewLines, Chars + 1}};
tuple_body([$, | T], Acc, NewLines, Chars) ->
    tuple_body(T, Acc, NewLines, Chars + 1);
tuple_body([$\s | T], Acc, NewLines, Chars) ->
    tuple_body(T, Acc, NewLines, Chars + 1);
tuple_body([$\t | T], Acc, NewLines, Chars) ->
    tuple_body(T, Acc, NewLines, Chars + 1);
tuple_body([$\n | T], Acc, NewLines, _Chars) ->
    tuple_body(T, Acc, NewLines + 1, 0);
tuple_body([$\r | T], Acc, NewLines, _Chars) ->
    tuple_body(T, Acc, NewLines + 1, 0);
tuple_body(Stream, Acc, NewLines, Chars) ->
    case value(Stream, NewLines, Chars) of
        {Value, Rest, {N, C}} ->
            tuple_body(Rest, [Value | Acc], N, C);
        Else ->
            Else
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Parse an atom that doesn't have single quote delimeters.
%%
%% @spec bare_atom(Stream, Acc, NewLines, Chars) -> {Atom, Rest, {N, C}} | Error
%% @end
%%--------------------------------------------------------------------
bare_atom([H | T], Acc, NewLines, Chars) when H >= $a, H =< $z ->
    bare_atom(T, [H | Acc], NewLines, Chars + 1);
bare_atom([H | T], Acc, NewLines, Chars) when H >= $A, H =< $Z ->
    bare_atom(T, [H | Acc], NewLines, Chars + 1);
bare_atom([$_ | T], Acc, NewLines, Chars) ->
    bare_atom(T, [$_ | Acc], NewLines, Chars + 1);
bare_atom([H | T], Acc, NewLines, Chars) when H >= $0, H =< $9 ->
    bare_atom(T, [ H | Acc], NewLines, Chars + 1);
bare_atom([$\s | T], Acc, NewLines, Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), T, {NewLines, Chars + 1}};
bare_atom([$\t | T], Acc, NewLines, Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), T, {NewLines, Chars + 1}};
bare_atom([$\r | T], Acc, NewLines, _Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), T, {NewLines + 1, 0}};
bare_atom([$\n | T], Acc, NewLines, _Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), T, {NewLines + 1, 0}};
bare_atom([], Acc, NewLines, Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), [], {NewLines, Chars}};
bare_atom(Else, Acc, NewLines, Chars) when length(Acc) > 0->
    {list_to_existing_atom(lists:reverse(Acc)), Else, {NewLines, Chars}}.
