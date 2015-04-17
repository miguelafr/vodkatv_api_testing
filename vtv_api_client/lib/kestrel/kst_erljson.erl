%%%-------------------------------------------------------------------
%%% @author Diana Parra Corbacho <diana.corbacho@lambdastream.com>
%%% @copyright (C) 2008, Diana Parra Corbacho
%%% @doc Parses an Erlang term into a json structure.
%%%
%%% Only key-value tuples are allowed in Erlang lists.
%%%
%%% Strings are encoded as binary.
%%%
%%% @type in_value() = string() | binary() | [in_value()] | int() | float()
%%%                  | atom() | [{Key, in_value()}] | [{}]
%%%
%%% @type out_value() = binary() | [out_value()] | int() | float()                 
%%%                  | [{string(), out_value()}]
%%% @end
%%% Created :  6 Nov 2008 by Diana Parra Corbacho <diana.corbacho@lambdastream.com>
%%%-------------------------------------------------------------------
-module(kst_erljson).

-export([erl_to_json/1, json_to_erl/1]).

%%=============================================================================
%% API
%%=============================================================================
%%--------------------------------------------------------------------
%% @doc Parses a Erlang term into a json structure.
%% 
%% @spec erl_to_json(DataObjects::in_value()) -> Output::out_value()
%% @throws invalid_term
%% @end
%%--------------------------------------------------------------------  
erl_to_json([{}]) ->
    {obj, []};   
erl_to_json(Data) when is_list(Data) ->
    etj_array(Data);
erl_to_json(Data) when is_binary(Data) ->
    Data;
erl_to_json(Data) when is_atom(Data) ->
    Data;
erl_to_json(Data) when is_float(Data) ->
    Data;
erl_to_json(Data) when is_integer(Data) ->
    Data;
erl_to_json(_Data) ->
    throw(invalid_term).


%%--------------------------------------------------------------------
%% @doc Parses a json structure into a Erlang term.
%% 
%% @spec json_to_erl(DataObjects::out_value()) -> Output::in_value()
%% @throws invalid_term
%% @end
%%--------------------------------------------------------------------
json_to_erl({obj, []}) ->
    [{}];
json_to_erl({obj, Value}) ->
    jte_object(Value, []);    
json_to_erl(Data) when is_list(Data) ->
    jte_array(Data, []);
json_to_erl(Data) when is_binary(Data) ->
    Data;
json_to_erl(Data) when is_atom(Data) ->
    Data;
json_to_erl(Data) when is_float(Data) ->
    Data;
json_to_erl(Data) when is_integer(Data) ->
    Data;
json_to_erl(_Data) ->
    throw(invalid_term).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc Encode a Erlang property list into a json object.
%% 
%% @spec etj_object(PropList, TAcc::list()) -> object()
%% @throws invalid_term
%% @end
%%--------------------------------------------------------------------
etj_object([{Key, Value} | T], TAcc) ->
    etj_object(T, [{Key, erl_to_json(Value)} | TAcc]);
etj_object([], TAcc) ->
    {obj, lists:reverse(TAcc)};
etj_object(_, _) ->
    throw(invalid_term).

%%--------------------------------------------------------------------
%% @private
%% @doc Encode a Erlang list into a json array.
%% Strings are encoded as binary.
%%
%% @spec etj_array(Data::list()) -> EncodedList::list() | object()
%% @throws invalid_term
%% @end
%%--------------------------------------------------------------------
etj_array(Data) ->
    try
	%% encodes string into binary
	lstd_type_checker:check_string(Data),
	list_to_binary(Data)	
    catch
	_ ->
	    %% encodes any other list
	    case lists:all(fun(T) -> is_tuple(T) end, Data) of
		true ->
		    etj_object(Data, []);
		false ->
		    etj_array(Data, [])
	    end
    end.

etj_array([H | T], TAcc) ->
    etj_array(T, [erl_to_json(H) | TAcc]);
etj_array([], TAcc) ->
    lists:reverse(TAcc).

%%--------------------------------------------------------------------
%% @private
%% @doc Decode a json object into a Erlang term.
%%
%% @spec jte_object(PropList, TAcc::list()) -> TupleList::list()
%% @throws invalid_term
%% @end
%%--------------------------------------------------------------------
jte_object([{Key, Value} | T], TAcc) ->    
    jte_object(T, [{json_to_erl(Key), json_to_erl(Value)} | TAcc]);
jte_object([], TAcc) ->
    lists:reverse(TAcc);
jte_object(_, _) ->
    throw(invalid_term).

%%--------------------------------------------------------------------
%% @private
%% @doc Decode json array into a Erlang list.
%% 
%% @spec jte_array(Data::list(), TAcc::list()) -> DecodedList::list()
%% @throws invalid_term
%% @end
%%--------------------------------------------------------------------
jte_array([H | T], TAcc) ->
    jte_array(T, [json_to_erl(H) | TAcc]);
jte_array([], TAcc) ->
    lists:reverse(TAcc);
jte_array(_, _) ->
    throw(invalid_term).

