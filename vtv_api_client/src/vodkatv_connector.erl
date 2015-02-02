-module(vodkatv_connector).

-export([login/3]).

-define(BASE_URL, "http://193.144.63.20:8082/vodkatv/").

login(AppId, UserId, Password) ->
    GetParams = generate_get_params([{"appId", AppId}, {"userId", UserId},
        {"password", Password}]),
    Url = add_get_params(?BASE_URL ++ "external/client/core/Login.do", GetParams),
    http_request('GET', Url,
                fun(Data) -> 
                    jiffy:decode(Data)
                end).

%%---------------------------------------------------------------
%% Utilities
%%---------------------------------------------------------------
http_request(Method, Url, FunParse)->
    http_request(Method, Url, "", FunParse).

http_request(Method, Url, Body, FunParse)->
    case do_http_request(Method, Url, Body) of
   	{ok, {{_Protocol, 200, _Msg}, _Headers, Response}} ->
		           {ok, FunParse(Response)};
    	{ok, {{_Protocol, Code, Msg}, _Headers, _Response}} ->
  	            {error, {Code, Msg}};
    	{error, Reason} ->
	            {error, Reason}
    end.

do_http_request('GET', Url, _Body)->
    httpc:request(get, {Url, []}, [], []);

do_http_request('POST', Url, Body)->
    httpc:request(post, {Url, [], "application/json", Body}, [], []).

generate_get_params([]) ->
    "";
generate_get_params([{_Name, undefined}]) ->
    "";
generate_get_params([{_Name, ""}]) ->
    "";
generate_get_params([{Name, Value}]) ->
    Name ++ "=" ++ encode_get_param(Value);
generate_get_params([{_Name, undefined}|Params]) ->
    generate_get_params(Params);
generate_get_params([{_Name, ""}|Params]) ->
    generate_get_params(Params);
generate_get_params([Param | Params]) ->
    case generate_get_params(Params) of 
        "" ->
            generate_get_params([Param]);
        Str ->
           generate_get_params([Param]) ++  "&" ++ Str
    end.

encode_get_param(Param) when is_integer(Param) ->
    encode_get_param(integer_to_list(Param));
encode_get_param(Param) ->
    http_uri:encode(Param).

add_get_params(Url, "") ->
    Url;
add_get_params(Url, Params) ->
    Url ++ "?" ++ Params.
