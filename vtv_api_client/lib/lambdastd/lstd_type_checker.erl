%%%-------------------------------------------------------------------
%%% @author Samuel Rivas <samuel.rivas@lambdastream.com>
%%% @copyright (C) 2008, Samuel Rivas
%%% @doc Functions to check generic types
%%%
%%% Developers must follow next steps to introduce new types in
%%% this module:
%%% <ul>
%%% <li>Add the type name to the documentation of `type()' type.</li>
%%% <li>Suposing new type is `whatever,' implement the function
%%%     `check_whatever/1' and export it in the public api.</li>
%%% <li>Add a positive test to `check_type_test_().'</li>
%%% <li>Write required unit tests, including calls to `check_whatever' and
%%%     adding test cases to `check_types_test_().'</li>
%%% <li>`check_types/1' should not need any changes.</li>
%%% </ul>
%%%
%%% @type bad_type(Type, What) = {bad_type, {Type, What}}.
%%%
%%% @type type() = type | string | integer | boolean | atom | pid
%%%		   | {custom, (term()) -> none()}.
%%% List of available types must grow in the future. `{custom, Fun}' is
%%% used to check extenal types in `check_types/1.'
%%%
%%% @end
%%% Created : 24 Feb 2008 by Samuel Rivas <samuel.rivas@lambdastream.com>
%%%-------------------------------------------------------------------
-module(lstd_type_checker).

%% API
-export([check_string/1, check_integer/1, check_boolean/1, check_atom/1,
	 check_pid/1, check_type/1, check_types/1, throw_bad_type/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs type-checking ver a list of terms
%%
%% If `Type' is `{custom, Fun}, Fun' will be used as type-checking
%% function. Type checking is considered sucessfull if `Fun' ends.
%% The value it returns is ignored.
%%
%% Since it is an external fucntion, we do not control the exceptions it throws,
%% but we encourage using something like {@link bad_type()}.
%%
%% @spec check_types([{Type::type(), [term()]}]) -> true
%% @throws bad_type(type(), term())
%% @end
%%--------------------------------------------------------------------
check_types(Types) ->
    lists:foreach(fun internal_check_type/1, Types),
    true.

%%--------------------------------------------------------------------
%% @doc Checks that a term is a {@link type()}
%% @spec check_type(What::term()) -> true
%% @throws bad_type(type, What)
%% @end
%%--------------------------------------------------------------------
check_type(type) ->
    true;
check_type(integer) ->
    true;
check_type(string) ->
    true;
check_type(boolean) ->
    true;
check_type(atom) ->
    true;
check_type(pid) ->
    true;
check_type(What) ->
    throw_bad_type(type, What).

%%--------------------------------------------------------------------
%% @doc Checks that a term is a string()
%% @spec check_string(What::term()) -> true
%% @throws bad_type(string, What)
%% @end
%%--------------------------------------------------------------------
check_string(What) when is_list(What) ->
    case io_lib:char_list(What) of
	true ->
	    true;
	false ->
	    throw_bad_type(string, What)
    end;
check_string(What) ->
    throw_bad_type(string, What).

%%--------------------------------------------------------------------
%% @doc Checks that a term is an integer()
%% @spec check_integer(What::term()) -> true
%% @throws bad_type(integer, What)
%% @end
%%--------------------------------------------------------------------
check_integer(N) when is_integer(N) ->
    true;
check_integer(What) ->
    throw_bad_type(integer, What).

%%--------------------------------------------------------------------
%% @doc Checks that a term is a boolean()
%% @spec check_boolean(What::term()) -> true
%% @throws bad_type(boolean, What)
%% @end
%%--------------------------------------------------------------------
check_boolean(true) ->
    true;
check_boolean(false) ->
    true;
check_boolean(What) ->
    throw_bad_type(boolean, What).


%%--------------------------------------------------------------------
%% @doc Checks that a term is an atom()
%% @spec check_atom(What::term()) -> true
%% @throws bad_type(atom, What)
%% @end
%%--------------------------------------------------------------------
check_atom(Atom) when is_atom(Atom) ->
    true;
check_atom(What) ->
    throw_bad_type(atom, What).


%%--------------------------------------------------------------------
%% @doc Checks that a term is a pid()
%% @spec check_pid(What::term()) -> true
%% @throws bad_type(pid, What)
%% @end
%%--------------------------------------------------------------------
check_pid(Pid) when is_pid(Pid) ->
    true;
check_pid(What) ->
    throw_bad_type(pid, What).


%%--------------------------------------------------------------------
%% @doc Trows a bad type exception
%% @spec throw_bad_type(Type::term(), What::term()) -> none()
%% @throws bad_type(Type, What)
%% @end
%%--------------------------------------------------------------------
throw_bad_type(Type, What) ->
    throw({bad_type, {Type, What}}).

%%====================================================================
%% Internal functions
%%====================================================================

%% @throws term()
internal_check_type({{custom, F}, Terms}) ->
    lists:foreach(F, Terms),
    true;

%% @throws bad_type()
internal_check_type({Type, Terms}) ->
    check_type(Type),
    F = fun(X) ->
		Function = list_to_atom("check_" ++ atom_to_list(Type)),
		?MODULE:Function(X)
	end,
    lists:foreach(F, Terms),
    true.

