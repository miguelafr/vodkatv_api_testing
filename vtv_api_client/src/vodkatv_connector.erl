-module(vodkatv_connector).

-include_lib("xmerl/include/xmerl.hrl").

-compile(export_all).

-define(APP_ID, "HJ8n59WO0Jcmr9l0U0FLXYlXaQOyzn").
-define(BASE_URL, "http://193.144.63.20:8083/vodkatv/").
-define(HTTP_REQUEST_HEADERS_JSON,
    [{"user-agent", "Chrome"},
    {"Accept", "application/json"},
    {"Content-Type", "application/json"}]).

%%---------------------------------------------------------------
%% JSON API
%%---------------------------------------------------------------
login(UserId, Password) ->
    GetParams = generate_get_params([{"appId", ?APP_ID}, {"userId", UserId},
        {"password", Password}]),
    Url = add_get_params(?BASE_URL ++ "external/client/core/Login.do", GetParams),
    http_request('GET', Url,
                get_http_headers_request(),
                fun(Data) ->
                    %io:format("login(~p, ~p)->~n    ~p~n~n",
                    %        [UserId, Password, Data]), 
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

logout(Token) ->
    Url = ?BASE_URL ++ "external/client/core/Logout.do",
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    %io:format("logout(~p)->~n    ~p~n~n", [Token, Data]), 
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

find_user_info(Token) ->
    Url = ?BASE_URL ++ "external/client/core/FindUserInfo.do",
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    %io:format("find_user_info(~p)->~n    ~p~n~n", [Token, Data]), 
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

register_user(UserName, Password) ->
    Url = ?BASE_URL ++ "external/client/v2/core/users",
    User = [{"userSession", [
            {"guest", [{"name", UserName}]},
            {"accounts", [[
                {"access", [
                    {"loginName", UserName},
                    {"password", Password}
                ]}
            ]]}
        ]}],
    Json = ktj_encode:encode(kst_erljson:erl_to_json(User)),
    http_request('POST', Url,
                get_http_headers_request(),
                list_to_binary(Json),
                fun(Data) -> 
                    %io:format("register_user(~p, ~p)->~n    ~p~n~n",
                    %        [UserName, Password, Data]), 
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

activate_user(ActivationCode) ->
    Url = ?BASE_URL ++ "external/client/v2/core/users/" ++ ActivationCode ++
            "/activation?authenticate=true",
    http_request('PUT', Url,
                get_http_headers_request(),
                fun(Data) -> 
                    %io:format("activate_user(~p)->~n    ~p~n~n",
                    %        [ActivationCode, Data]), 
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

password_recovery(UserId) ->
    Url = ?BASE_URL ++ "external/client/v2/core/users/" ++ UserId ++
            "/passwordrecovery",
    http_request('PUT', Url,
                get_http_headers_request(),
                fun(Data) -> 
                    %io:format("password_recovery(~p)->~n    ~p~n~n",
                    %        [UserId, Data]), 
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

change_password_from_code(Code, Password) ->
    Url = ?BASE_URL ++ "external/client/v2/core/users/" ++ Code ++
            "/passwordchange",
    http_request('PUT', Url,
                get_http_headers_request(),
                list_to_binary(Password),
                fun(Data) -> 
                    %io:format("change_password_from_code(~p, ~p)->~n    ~p~n~n",
                    %        [Code, Password, Data]), 
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

%%---------------------------------------------------------------
%% XML API
%%---------------------------------------------------------------
find_all_rooms() ->
    Url = ?BASE_URL ++ "external/admin/configuration/FindAllRooms.do",
    http_request('GET', Url, [],
                fun(Data) -> 
                    %io:format("find_all_rooms()->~n    ~p~n~n", [Data]), 
                    {RootEl, _Misc} = xmerl_scan:string(Data),
                    Rooms = RootEl#xmlElement.content,
                    lists:map(fun(Room) ->
                        [[RoomId]] = [RoomAttr#xmlElement.content ||
                            RoomAttr <- Room#xmlElement.content,
                            RoomAttr#xmlElement.name == roomId],
                        [{"roomId", RoomId#xmlText.value}]
                    end, Rooms)
                end).

delete_room_device_session(RoomId) ->
    GetParams = generate_get_params([{"roomId", RoomId}]),
    Url = add_get_params(?BASE_URL ++
        "external/admin/configuration/DeleteRoomDeviceSession.do", GetParams),
    http_request('GET', Url, [],
                fun(Data) -> 
                    %io:format("delete_room_device_session(~p)->~n    ~p~n~n",
                    %        [RoomId, Data]), 
                    ok
                end).

get_activation_code(UserId) ->
    GetParams = generate_get_params([{"userId", UserId}]),
    Url = add_get_params(?BASE_URL ++
        "external/admin/accounting/FindActivationCodeByUserId.do", GetParams),
    http_request('GET', Url, [],
                fun(Data) -> 
                    %io:format("get_activation_code(~p)->~n    ~p~n~n",
                    %        [UserId, Data]), 
                    {RootEl, _Misc} = xmerl_scan:string(Data),
                    [AccountRemoteAccess] = RootEl#xmlElement.content,
                    [ActivationCode] = AccountRemoteAccess#xmlElement.content,
                    ActivationCode#xmlText.value
                end).

get_password_recovery_code(UserId) ->
    GetParams = generate_get_params([{"userId", UserId}]),
    Url = add_get_params(?BASE_URL ++
        "external/admin/accounting/FindPasswordRecoveryCodeByUserId.do", GetParams),
    http_request('GET', Url, [],
                fun(Data) -> 
                    %io:format("get_password_recovery_code(~p)->~n    ~p~n~n",
                    %        [UserId, Data]), 
                    {RootEl, _Misc} = xmerl_scan:string(Data),
                    [AccountRemoteAccess] = RootEl#xmlElement.content,
                    [ActivationCode] = AccountRemoteAccess#xmlElement.content,
                    ActivationCode#xmlText.value
                end).

%%---------------------------------------------------------------
%% Utilities
%%---------------------------------------------------------------
get_http_headers_request() ->
    ?HTTP_REQUEST_HEADERS_JSON.

get_http_headers_request(Token) ->
    get_http_headers_request() ++ [{"Cookie", "t=" ++ Token}].

http_request(Method, Url, Headers, FunParse)->
    http_request(Method, Url, Headers, "", FunParse).

http_request(Method, Url, Headers, Body, FunParse)->
    case do_http_request(Method, Url, Headers, Body) of
   	{ok, {{_Protocol, 200, _Msg}, _Headers, Response}} ->
		           {ok, FunParse(Response)};
    	{ok, {{_Protocol, Code, Msg}, _Headers, Response}} ->
  	            {error, {Code, Msg, FunParse(Response)}};
    	{error, Reason} ->
	            {error, Reason}
    end.

do_http_request('GET', Url, Headers, _Body)->
    httpc_request(get, {Url, Headers}, [], []);

do_http_request('PUT', Url, Headers, Body)->
    httpc_request(put, {Url, Headers, "application/json", Body}, [], []);

do_http_request('POST', Url, Headers, Body)->
    httpc_request(post, {Url, Headers, "application/json", Body}, [], []).

httpc_request(Method, Request, HTTPOptions, Options) ->
    case httpc:request(Method, Request, HTTPOptions, Options) of
        {error,socket_closed_remotely} ->
            io:format("Warning: socket_closed_remotely error. Retrying request...~n"),
            httpc_request(Method, Request, HTTPOptions, Options);
        R ->
            R
    end.

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


