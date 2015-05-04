-module(vodkatv_connector).

-include_lib("xmerl/include/xmerl.hrl").

-compile(export_all).

-define(APP_ID, "HJ8n59WO0Jcmr9l0U0FLXYlXaQOyzn").
-define(BASE_URL, "http://193.144.63.20:8083/vodkatv/").
-define(HTTP_REQUEST_HEADERS_JSON,
    [{"user-agent", "Chrome"},
    {"Accept", "application/json"},
    {"Content-Type", "application/json"}]).
-define(DEBUG, false).

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
                    case ?DEBUG of
                        true -> 
                            io:format("login(~p, ~p)->~n    ~s~n~n",
                                [UserId, Password, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

logout(Token) ->
    Url = ?BASE_URL ++ "external/client/core/Logout.do",
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) ->
                    case ?DEBUG of
                        true -> 
                            io:format("logout(~p)->~n    ~s~n~n", [Token, Data]);
                        false ->
                            ok
                    end, 
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

find_user_info(Token) ->
    Url = ?BASE_URL ++ "external/client/core/FindUserInfo.do",
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("find_user_info(~p)->~n    ~s~n~n",
                                [Token, Data]);
                        false ->
                            ok
                    end,
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
                    case ?DEBUG of
                        true -> 
                            io:format("register_user(~p, ~p)->~n    ~s~n~n",
                                    [UserName, Password, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

activate_user(ActivationCode) ->
    Url = ?BASE_URL ++ "external/client/v2/core/users/" ++ ActivationCode ++
            "/activation?authenticate=true",
    http_request('PUT', Url,
                get_http_headers_request(),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("activate_user(~p)->~n    ~s~n~n",
                                    [ActivationCode, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

password_recovery(UserId) ->
    Url = ?BASE_URL ++ "external/client/v2/core/users/" ++ UserId ++
            "/passwordrecovery",
    http_request('PUT', Url,
                get_http_headers_request(),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("password_recovery(~p)->~n    ~s~n~n",
                                    [UserId, Data]);
                        false ->
                            ok
                    end,
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
                    case ?DEBUG of
                        true -> 
                            io:format("change_password_from_code(~p, ~p)->~n    ~s~n~n",
                                    [Code, Password, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

find_products(Token) ->
    Url = ?BASE_URL ++ "external/client/plugins/store/FindCatalogue.do",
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("find_products(~p)->~n    ~s~n~n",
                                [Token, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    Result = kst_erljson:json_to_erl(R),
                    case proplists:get_value("subscriptions", Result) of
                        undefined ->
                            Result;
                        Subscriptions ->
                            lists:map(fun(Product) ->
                                proplists:get_value("product", Product)
                            end, Subscriptions)
                    end
                end).

purchase_product(Token, ProductId) ->
    GetParams = generate_get_params([{"productId", ProductId}]),
    Url = add_get_params(?BASE_URL ++ "external/client/plugins/store/PurchaseProduct.do",
            GetParams),
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("purchase_product(~p, ~p)->~n    ~s~n~n",
                                [Token, ProductId, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

find_tv_channels(Token) ->
    Url = ?BASE_URL ++ "external/client/plugins/television/FindChannels.do",
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("find_tv_channels(~p)->~n    ~s~n~n",
                                [Token, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

add_tv_channel_to_favourite_channels(Token, TVChannelId) ->
    GetParams = generate_get_params([{"vodkatvChannelId", TVChannelId}]),
    Url = add_get_params(?BASE_URL ++
        "external/client/plugins/television/AddToFavoriteChannels.do",
        GetParams),
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("add_tv_channel_to_favourite_channels(~p, ~p)->~n    ~s~n~n",
                                [Token, TVChannelId, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

remove_tv_channel_from_favourite_channels(Token, TVChannelId) ->
    GetParams = generate_get_params([{"vodkatvChannelId", TVChannelId}]),
    Url = add_get_params(?BASE_URL ++
        "external/client/plugins/television/RemoveFromFavoriteChannels.do",
        GetParams),
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("remove_tv_channel_from_favourite_channels(~p, ~p)->~n    ~s~n~n",
                                [Token, TVChannelId, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

find_tv_favourite_channels(Token) ->
    Url = ?BASE_URL ++ "external/client/plugins/television/FindFavoriteChannels.do",
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("find_tv_favourite_channels(~p)->~n    ~s~n~n",
                                [Token, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

find_vod_movies(Token, StartIndex, Count) ->
    GetParams = generate_get_params([{"startIndex", StartIndex},
        {"count", Count}]),
    Url = add_get_params(?BASE_URL ++
        "external/client/plugins/videoclub/SearchAssets.do",
        GetParams),
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("find_vod_movies(~p, ~p, ~p)->~n    ~s~n~n",
                                [Token, StartIndex, Count, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

find_vod_rented_movies(Token, StartIndex, Count) ->
    GetParams = generate_get_params([{"startIndex", StartIndex},
        {"count", Count}]),
    Url = add_get_params(?BASE_URL ++
        "external/client/plugins/videoclub/FindRentals.do",
        GetParams),
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) ->
                    case ?DEBUG of
                        true -> 
                            io:format("find_rented_movies(~p, ~p, ~p)->~n    ~s~n~n",
                                [Token, StartIndex, Count, Data]);
                        false ->
                            ok
                    end,
                    {R, _, _} = ktj_decode:decode(Data),
                    kst_erljson:json_to_erl(R)
                end).

rent_vod_movie(Token, MovieId, Price, Currency) ->
    GetParams = generate_get_params([{"assetId", MovieId},
        {"price", Price}, {"currency", Currency}]),
    Url = add_get_params(?BASE_URL ++
        "external/client/plugins/videoclub/RentAsset.do",
        GetParams),
    http_request('GET', Url,
                get_http_headers_request(Token),
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("rent_vod_movie(~p, ~p)->~n    ~s~n~n",
                                [Token, MovieId, Data]);
                        false ->
                            ok
                    end,
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
                    case ?DEBUG of
                        true -> 
                            io:format("find_all_rooms()->~n    ~s~n~n", [Data]);
                        false ->
                            ok
                    end,
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
                    case ?DEBUG of
                        true -> 
                            io:format("delete_room_device_session(~p)->~n    ~s~n~n",
                                [RoomId, Data]);
                        false ->
                            ok
                    end,
                    ok
                end).

delete_purchase(PurchaseId) ->
    GetParams = generate_get_params([{"purchaseId", PurchaseId}]),
    Url = add_get_params(?BASE_URL ++
        "external/admin/business/DeletePurchase.do", GetParams),
    http_request('GET', Url, [],
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("delete_purchase(~p)->~n    ~s~n~n",
                                [PurchaseId, Data]);
                        false ->
                            ok
                    end,
                    ok
                end).

get_activation_code(UserId) ->
    GetParams = generate_get_params([{"userId", UserId}]),
    Url = add_get_params(?BASE_URL ++
        "external/admin/accounting/FindActivationCodeByUserId.do", GetParams),
    http_request('GET', Url, [],
                fun(Data) -> 
                    case ?DEBUG of
                        true -> 
                            io:format("get_activation_code(~p)->~n    ~s~n~n",
                                [UserId, Data]);
                        false ->
                            ok
                    end,
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
                    case ?DEBUG of
                        true -> 
                            io:format("get_password_recovery_code(~p)->~n    ~s~n~n",
                                [UserId, Data]);
                        false ->
                            ok
                    end,
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

get_http_headers_request(undefined) ->
    get_http_headers_request();
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


