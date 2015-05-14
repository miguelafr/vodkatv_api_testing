-module(vodkatv_eqc_text_pilot).

-include_lib("eqc/include/eqc.hrl"). 
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-define(EQC_PREFFIX, "eqc_user_").
-define(MAX_VOD_MOVIES, 10).

-record(state, {
    valid_users, % users that have been registered and activated
    not_activated_user, % user that has been registered
    activation_code, % the code to activate last registered user
    current_user_id, % user that is currently logged in
    current_token, % a user that has logged in is given a token that has to be passed to all VoDKATV operations
    product_television, % identifier of a TV service
    product_videoclub,  % identifier of a VOD service
    purchase_videoclub, % id of the VOD purchase, used for cancellation
    tv_channels, % list of all possible channels, obtained at initialisation via admin service
    vod_movies, 
    vod_rented_movies,

    recovery_user,
    recovery_code,
    current_tv,
    favourites
}).

initial_state() ->
    {ok, TVChannels} = vodkatv_connector:find_all_channels(),
    #state {
        valid_users = [],
        not_activated_user = undefined,
        activation_code = undefined,
        current_user_id = undefined,
        current_token = undefined,
        product_television = undefined,
        product_videoclub = undefined,
        purchase_videoclub = undefined,
        tv_channels = TVChannels,
        vod_movies = [],
        vod_rented_movies = [],

       recovery_user=undefined,
       recovery_code=undefined,
       current_tv=undefined,
       favourites=[]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Login
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
login(UserId, Password) ->
    case vodkatv_connector:login(UserId, Password) of
        {ok, R} ->
            proplists:get_value("token", R);
        Other ->
            {error, Other}
    end.

login_args(S) ->
    ?LET({UserId, Password}, elements(S#state.valid_users), [UserId, Password]).

login_pre(S)->
    S#state.current_token == undefined andalso not_activated_user == undefined.

login_next(S, V, [UserId, _Password]) ->
    S#state {
        current_user_id = UserId,
        current_token = V,
        product_television = undefined,
        product_videoclub = undefined,
        purchase_videoclub = undefined,
        vod_movies = [],

        favourites = undefined,
       current_tv = undefined
    }.

login_post(_S, _Args, {error, Error}) ->
    tag([{{login, Error}, false}]);
login_post(_S, _Args, R) ->
    tag([{login, (R /= undefined) andalso (R /= "")}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Login error
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
login_error(UserId, Password) ->
    vodkatv_connector:login(UserId, Password).

login_error_args(_S) ->
    elements([["invalid_user1", "invalid_password1"],
            ["invalid_user2", "invalid_password2"],
            ["invalid_user3", "invalid_password3"]]).

login_error_pre(S)->
    S#state.current_token == undefined andalso S#state.not_activated_user == undefined andalso S#state.recovery_user == undefined.

login_error_post(_S, _Args, {ok, R}) ->
    tag([{{login_error, R},
        (proplists:get_value("errors", R) /= undefined)}]);
login_error_post(_S, _Args, R) ->
    tag([{{login_error, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logout(Token) ->
    vodkatv_connector:logout(Token).

logout_args(S) ->
    [S#state.current_token].

logout_pre(S)->
    S#state.current_token =/= undefined andalso S#state.current_tv == undefined andalso S#state.purchase_videoclub == undefined.

logout_next(S, _V, _Args) ->
    S#state {
        current_user_id = undefined,
        current_token = undefined,
        product_television = undefined,
        product_videoclub = undefined,
        purchase_videoclub = undefined,
        vod_movies = []
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find user info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_user_info(Token) ->
    vodkatv_connector:find_user_info(Token).

find_user_info_args(S) ->
    [S#state.current_token].

find_user_info_pre(S)->
    S#state.current_token =/= undefined.

find_user_info_post(S, _Args, {ok, R}) ->
    UserInfo = proplists:get_value("userInfo", R),
    UserId = proplists:get_value("userId", UserInfo),
    tag([{find_user_info, (UserId == S#state.current_user_id)}]);
find_user_info_post(_S, _Args, R) ->
    tag([{{find_user_info, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register user
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_user(UserId, Password) ->
    vodkatv_connector:register_user(UserId, Password).

register_user_next(S, _V, [UserId, Password]) ->
    S#state {
        not_activated_user = {UserId, Password}
    }.

register_user_args(S) ->
    ExistingUsers = S#state.valid_users,
    ?LET(N,
        ?SUCHTHAT(N, nat(), (lists:keyfind(user_id(N), 1, ExistingUsers) == false)),
        [user_id(N), gen_password()]).

register_user_pre(S)->
    S#state.current_token == undefined andalso S#state.not_activated_user == undefined andalso S#state.recovery_user == undefined.

register_user_post(_S, [UserId, _Password], {ok, R}) ->
    UserSession = proplists:get_value("userSession", R),
    [Account | []] = proplists:get_value("accounts", UserSession),
    Access = proplists:get_value("access", Account),
    ReturnedUserId = proplists:get_value("loginName", Access),
    tag([{{register_user, UserId, ReturnedUserId}, (UserId == ReturnedUserId)}]);
register_user_post(_S, _Args, R) ->
    tag([{{register_user, R}, false}]).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register user duplicated
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_user_duplicated(UserId, Password) ->
    vodkatv_connector:register_user(UserId, Password).

register_user_duplicated_args(S) ->
    ?LET({UserId, _Password},
        elements(S#state.valid_users),
        [UserId, gen_password()]).

register_user_duplicated_pre(S)->
    length(S#state.valid_users) > 0 andalso S#state.current_token == undefined andalso S#state.not_activated_user == undefined andalso S#state.recovery_user == undefined.

register_user_duplicated(S)->
    S#state.current_token == undefined andalso S#state.not_activated_user == undefined andalso S#state.recovery_user == undefined.

register_user_duplicated_post(_S, [UserId, _Password],
        {error, {409, _Msg, R}}) ->
    [Error | []] = proplists:get_value("errors", R),
    [ErrorParam | []] = proplists:get_value("params", Error),
    ReturnedUserId = proplists:get_value("value", ErrorParam),
    tag([{register_user_duplicated, (UserId == ReturnedUserId)}]);
register_user_duplicated_post(_S, _Args, R) ->
    tag([{{register_user_duplicated, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get activation code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_activation_code(UserId) ->
    case vodkatv_connector:get_activation_code(UserId) of
        {ok, R} ->
            R;
        Other ->
            {error, Other}
    end.

get_activation_code_args(S) ->
    {UserId, _Password}=S#state.not_activated_user, 
    [UserId].

get_activation_code_pre(S)->
    S#state.not_activated_user =/= undefined andalso S#state.activation_code == undefined.

get_activation_code_next(S, V, [UserId]) ->
    S#state {
        activation_code = {UserId, V}
    }.

get_activation_code_post(_S, _Args, {error, Error}) ->
    tag([{{activation_code, Error}, false}]);
get_activation_code_post(_S, _Args, R) ->
    tag([{activation_code, (R /= undefined) andalso (R /= "")}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Activate user
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
activate_user(ActivationCode) ->
    case vodkatv_connector:activate_user(ActivationCode) of
        {ok, R} ->
            proplists:get_value("token", R);
        Other ->
            {error, Other}
    end.

activate_user_args(S) ->
    {_UserId, ActivationCode}=S#state.activation_code, 
    [ActivationCode].

activate_user_pre(S)->
    S#state.not_activated_user =/= undefined andalso S#state.activation_code =/= undefined.

activate_user_next(S, V, [ActivationCode]) ->
    {UserId, ActivationCode} = S#state.activation_code,
    {_UserId, Password} = S#state.not_activated_user,
    S#state {
        valid_users = [{UserId, Password} | S#state.valid_users],
        not_activated_user = undefined,
        activation_code = undefined,
        current_user_id = UserId,
        current_token = V
    }.

activate_user_post(_S, _Args, {error, Error}) ->
    tag([{{activate_user, Error}, false}]);
activate_user_post(_S, _Args, R) ->
    tag([{activate_user, (R /= undefined) andalso (R /= "")}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Activate user wrong activation code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
activate_user_wrong_activation_code(ActivationCode) ->
    vodkatv_connector:activate_user(ActivationCode).

activate_user_wrong_activation_code_args(_S) ->
    ?LET(N, nat(), [integer_to_list(N)]).

activate_user_wrong_activation_code_pre(S)->
    S#state.not_activated_user =/= undefined andalso S#state.activation_code =/= undefined.

activate_user_wrong_activation_code_post(_S, [ActivationCode],
        {error, {404, _Msg, R}}) ->
    [Error] = proplists:get_value("errors", R),
    Code = proplists:get_value("code", Error),
    [Params] = proplists:get_value("params", Error),
    ParamName = proplists:get_value("name", Params),
    ParamValue = proplists:get_value("value", Params),
    tag([{{activate_user_wrong_activation_code, R},
        (Code == "not_found" andalso
        ParamName == "access/activationCode" andalso
        ParamValue == ActivationCode)}]);
activate_user_wrong_activation_code_post(_S, _Args, R) ->
    tag([{{activate_user_wrong_activation_code, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Password recovery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
password_recovery(UserId) ->
    vodkatv_connector:password_recovery(UserId).

password_recovery_args(S) ->
    ?LET({UserId, _Password}, elements(S#state.valid_users), [UserId]). % YOU NEED TO WRITE THIS, using login as an illustration. Functions proplists:delete(Key,List), lists:delete(Elem,List) and proplists:get_value(Key,List) could be useful for password recovery routines.

password_recovery_pre(S) ->
    length(S#state.valid_users) >0 andalso S#state.not_activated_user == undefined andalso S#state.activation_code == undefined andalso S#state.recovery_user == undefined. % YOU NEED TO WRITE THIS

password_recovery_next(S, _V, [UserId]) ->
    S#state{recovery_user=UserId}. % YOU NEED TO WRITE THIS

password_recovery_post(_S, [_UserId], {ok, _R}) ->
    true;
password_recovery_post(_S, [_UserId], _) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get password recovery code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_password_recovery_code(UserId) ->
    case vodkatv_connector:get_password_recovery_code(UserId) of
        {ok, R} ->
            R;
        Other ->
            {error, Other}
    end.

get_password_recovery_code_args(S) ->
   [S#state.recovery_user]. % YOU NEED TO WRITE THIS, in part using login as an illustration

get_password_recovery_code_pre(S)->
    S#state.recovery_user =/= undefined.

get_password_recovery_code_next(S, V, [_UserId]) ->
    S#state{recovery_code=V}. % YOU NEED TO WRITE THIS

get_password_recovery_code_post(_S, _Args, {error, _Other}) ->
    false;
get_password_recovery_code_post(_S, _Args, _R) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change password from code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_password_from_code(PasswordRecoveryCode, Password) ->
    vodkatv_connector:change_password_from_code(PasswordRecoveryCode, Password).

change_password_from_code_args(S) ->
    [S#state.recovery_code,gen_password()]. % YOU NEED TO WRITE THIS

change_password_from_code_pre(S) ->
    S#state.recovery_code =/= undefined.

change_password_from_code_next(S, _V,[_PasswordRecoveryCode, Password]) ->
    S#state{valid_users=[{S#state.recovery_user,Password}|proplists:delete(S#state.recovery_user,S#state.valid_users)],recovery_user = undefined, recovery_code = undefined}. % YOU NEED TO WRITE THIS, do not forget that new password needs to be stored for the benefit of login

change_password_from_code_post(_S, [_PasswordRecoveryCode, _Password], {ok, _R}) ->
    true;
change_password_from_code_post(_S, [_PasswordRecoveryCode, _Password], _) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find products
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_products(Token) ->
    case vodkatv_connector:find_products(Token) of
        {ok, R} ->
            {find_product("television", R),
            find_product("videoclub", R),
            find_product("radio", R),
            find_product("preferences", R)};
        Other ->
            {error, Other}
    end.

find_products_args(S) ->
    [S#state.current_token].

find_products_pre(S)->
    S#state.current_token =/= undefined.

find_products_next(S, V, [_Token]) ->
    S#state {
        product_television = {call, erlang, element, [1, V]},
        product_videoclub = {call, erlang, element, [2, V]}  
    }.

find_products_post(_S, _Args, {error, R}) ->
    tag([{{find_products, R}, false}]);
find_products_post(_S, _Args, _R) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase television
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
purchase_television_product(Token, ProductId)->
    case vodkatv_connector:purchase_product(Token, ProductId) of
        {ok, R} ->
            Purchase = proplists:get_value("purchase", R),
            proplists:get_value("id", Purchase);
        Other ->
            {error, Other}
    end.

purchase_television_product_pre(S) ->
    S#state.product_television =/= undefined andalso S#state.current_token =/= undefined andalso S#state.current_tv == undefined andalso S#state.purchase_videoclub == undefined. % YOU NEED TO WRITE THIS, but note that we are modelling behaviour from a single user point of view, hence only one user can use a television at a time. 

purchase_television_product_args(S) ->
    ProductId = case S#state.product_television of
        undefined ->
            undefined;
        Product ->
            {call, proplists, get_value, ["id", Product]}
    end,
    [S#state.current_token, ProductId].

purchase_television_product_next(S, V, [_Token, _ProductId]) ->
    S#state{current_tv=V}. % YOU NEED TO WRITE THIS, but note that we are modelling behaviour from a single user point of view, hence only one user can use a television at a time. 

purchase_television_product_post(_S, _Args, {error, R}) ->
    tag([{{purchase_television_product, R}, false}]);
purchase_television_product_post(_S, _Args, R) ->
    tag([{{purchase_television_product, R}, (R /= undefined)}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel television
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cancel_television_product(PurchaseId) ->
    vodkatv_connector:delete_purchase(PurchaseId).

cancel_television_product_args(S) ->
    [S#state.current_tv]. % YOU NEED TO WRITE THIS

cancel_television_product_pre(S)->
    S#state.current_tv =/= undefined.

cancel_television_product_next(S, _V, [_ProductId]) ->
    S#state{current_tv = undefined,favourites=[]}. % YOU NEED TO WRITE THIS

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find tv channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_tv_channels(Token) ->
    case vodkatv_connector:find_tv_channels(Token) of
        {ok, R} ->
            Channels = proplists:get_value("channels", R),
            proplists:get_value("elements", Channels);
        Other ->
            {error, Other}
    end.

find_tv_channels_args(S) -> 
    [S#state.current_token].

find_tv_channels_pre(S) ->
    S#state.current_tv =/= undefined.

find_tv_channels_post(_S, _Args, {error, R}) ->
    tag([{{find_tv_channels, R}, false}]);
find_tv_channels_post(S, _Args, R) ->
    tag([{{find_tv_channels, R}, equals_tv_channels(R, S#state.tv_channels)}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add tv channel to favourite channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_tv_channel_to_favourite_channels(Token, TVChannel) ->
    TVChannelId = proplists:get_value("vodkatvChannelId", TVChannel),
    case vodkatv_connector:add_tv_channel_to_favourite_channels(Token, TVChannelId) of
        {ok, R} ->
            Channels = proplists:get_value("channels", R),
            proplists:get_value("elements", Channels);
        Other ->
            {error, Other}
    end.

add_tv_channel_to_favourite_channels_args(S) -> 
    [S#state.current_token,?SUCHTHAT(TVChannel,elements(S#state.tv_channels),not lists:member(TVChannel,S#state.favourites))]. % YOU NEED TO WRITE THIS, probably using ?SUCHTHAT(X,Generator,Condition)

add_tv_channel_to_favourite_channels_pre(S) -> 
    S#state.current_tv =/= undefined andalso length(S#state.tv_channels) > length(S#state.favourites).

add_tv_channel_to_favourite_channels_next(S, _V, [_Token, TVChannel]) ->
    S#state{favourites=[TVChannel|S#state.favourites]}. % YOU NEED TO WRITE THIS

add_tv_channel_to_favourite_channels_post(_S, _Args, {error, _R}) ->
    false;
add_tv_channel_to_favourite_channels_post(_S, _Args, _) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remove tv channel from favourite channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_from_favourite_channels(Token, TVChannel) ->
    TVChannelId = proplists:get_value("vodkatvChannelId", TVChannel),
    case vodkatv_connector:remove_tv_channel_from_favourite_channels(Token, TVChannelId) of
        {ok, R} ->
            Channels = proplists:get_value("channels", R),
            proplists:get_value("elements", Channels);
        Other ->
            {error, Other}
    end.

remove_from_favourite_channels_pre(S) ->
    S#state.current_tv =/= undefined andalso length(S#state.favourites) > 0. % YOU NEED TO WRITE THIS

remove_from_favourite_channels_args(S) -> 
    [S#state.current_token,elements(S#state.favourites)]. % YOU NEED TO WRITE THIS

remove_from_favourite_channels_next(S, _V, [_Token, TVChannel]) ->
    S#state{favourites=lists:delete(TVChannel,S#state.favourites)}. % YOU NEED TO WRITE THIS

remove_from_favourite_channels_post(_S, _Args, {error, R}) ->
    tag([{{remove_tv_channel_from_favourite_channels, R}, false}]);
remove_from_favourite_channels_post(_S, [_Token, TVChannel], R) ->
    tag([{{remove_tv_channel_from_favourite_channels, R}, not contains_tv_channel(TVChannel, R)}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase videoclub
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
purchase_videoclub_product(Token, ProductId)->
    case vodkatv_connector:purchase_product(Token, ProductId) of
        {ok, R} ->
            Purchase = proplists:get_value("purchase", R),
            proplists:get_value("id", Purchase);
        Other ->
            {error, Other}
    end.

purchase_videoclub_product_pre(S) ->
   S#state.product_videoclub =/= undefined andalso S#state.current_token =/= undefined andalso S#state.current_tv == undefined andalso S#state.purchase_videoclub == undefined.

purchase_videoclub_product_args(S) ->
    ProductId = case S#state.product_videoclub of
        undefined ->
            undefined;
        Product ->
            {call, proplists, get_value, ["id", Product]}
    end,
    [S#state.current_token, ProductId].

purchase_videoclub_product_next(S, V, [_Token, _ProductId]) ->
    S#state {
        purchase_videoclub = V
    }.

purchase_videoclub_product_post(_S, _Args, {error, R}) ->
    tag([{{purchase_videoclub_product, R}, false}]);
purchase_videoclub_product_post(_S, _Args, R) ->
    tag([{{purchase_videoclub_product, R}, (R /= undefined)}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel videoclub
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cancel_videoclub_product(PurchaseId) ->
    vodkatv_connector:delete_purchase(PurchaseId).

cancel_videoclub_product_args(S) ->
    [S#state.purchase_videoclub].

cancel_videoclub_product_pre(S) ->
    S#state.purchase_videoclub =/= undefined.

cancel_videoclub_product_next(S, _V, [_ProductId]) ->
    S#state {
        purchase_videoclub = undefined
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find tv channels not allowed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_tv_channels_not_allowed(Token) ->
    vodkatv_connector:find_tv_channels(Token).

find_tv_channels_not_allowed_args(S) -> 
    [S#state.current_token].

find_tv_channels_not_allowed_pre(S) ->
    S#state.current_token =/= undefined andalso S#state.current_tv == undefined andalso S#state.purchase_videoclub == undefined.

find_tv_channels_not_allowed_post(_S, _Args, {ok, R}) ->
    [Error] = proplists:get_value("errors", R),
    Code = proplists:get_value("code", Error),
    Reason = proplists:get_value("reason", Error),
    PluginId = proplists:get_value("pluginId", Error),
    tag([{{find_tv_channels_not_allowed, R},
        (Code == "access_denied" andalso
        Reason == "access_right_plugin_not_found" andalso
        PluginId == "television")}]);
find_tv_channels_not_allowed_post(_S, _Args, R) ->
    tag([{{find_tv_channels_not_allowed, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find tv channels not logged
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_tv_channels_not_logged(Token) ->
    vodkatv_connector:find_tv_channels(Token).

find_tv_channels_not_logged_args(S) -> 
    [S#state.current_token].

find_tv_channels_not_logged_pre(S)->
    S#state.current_token == undefined.

find_tv_channels_not_logged_post(_S, _Args, {ok, R}) ->
    [Error] = proplists:get_value("errors", R),
    Code = proplists:get_value("code", Error),
    Reason = proplists:get_value("reason", Error),
    tag([{{find_tv_channels_not_logged, R},
        (Code == "access_denied" andalso
        Reason == "not_authenticated")}]);
find_tv_channels_not_logged_post(_S, _Args, R) ->
    tag([{{find_tv_channels_not_logged, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find vod movies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_vod_movies(Token, StartIndex, Count) ->
    case vodkatv_connector:find_vod_movies(Token, StartIndex, Count) of
        {ok, R} ->
            proplists:get_value("assets", R);
        Other ->
            {error, Other}
    end.

find_vod_movies_args(S) -> 
    [S#state.current_token, 1, ?MAX_VOD_MOVIES].

find_vod_movies_pre(S)->
    S#state.purchase_videoclub =/= undefined.

find_vod_movies_next(S, V, _Args) ->
    S#state {
        vod_movies = V
    }.

find_vod_movies_post(_S, _Args, {error, R}) ->
    tag([{{find_vod_movies, R}, false}]);
find_vod_movies_post(_S, _Args, R) ->
    tag([{{find_vod_movies, R}, is_list(R) andalso length(R) >= 0}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find vod movies not allowed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_vod_movies_not_allowed(Token, StartIndex, Count) ->
    vodkatv_connector:find_vod_movies(Token, StartIndex, Count).

find_vod_movies_not_allowed_args(S) -> 
    [S#state.current_token, 1, ?MAX_VOD_MOVIES].

find_vod_movies_not_allowed_pre(S) -> 
    S#state.current_token =/= undefined andalso S#state.purchase_videoclub == undefined.

find_vod_movies_not_allowed_post(_S, _Args, {ok, R}) ->
    [Error] = proplists:get_value("errors", R),
    Code = proplists:get_value("code", Error),
    Reason = proplists:get_value("reason", Error),
    PluginId = proplists:get_value("pluginId", Error),
    tag([{{find_vod_movies_not_allowed, R},
        (Code == "access_denied" andalso
        Reason == "access_right_plugin_not_found" andalso
        PluginId == "videoclub")}]);
find_vod_movies_not_allowed_post(_S, _Args, R) ->
    tag([{{find_vod_movies_not_allowed, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find vod movies not logged
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_vod_movies_not_logged(Token, StartIndex, Count) ->
    vodkatv_connector:find_vod_movies(Token, StartIndex, Count).

find_vod_movies_not_logged_args(S) -> 
    [S#state.current_token, 1, ?MAX_VOD_MOVIES].

find_vod_movies_not_logged_pre(S) ->
    S#state.current_token == undefined.

find_vod_movies_not_logged_post(_S, _Args, {ok, R}) ->
    [Error] = proplists:get_value("errors", R),
    Code = proplists:get_value("code", Error),
    Reason = proplists:get_value("reason", Error),
    tag([{{find_vod_movies_not_logged, R},
        (Code == "access_denied" andalso
        Reason == "not_authenticated")}]);
find_vod_movies_not_logged_post(_S, _Args, R) ->
    tag([{{find_vod_movies_not_logged, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rent vod movie
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rent_vod_movie(Token, Movie) ->
    MovieId = proplists:get_value("id", Movie),
    case vodkatv_connector:rent_vod_movie(Token, MovieId, 0, "EUR") of
        {ok, R} ->
            R;
        Other ->
            {error, Other}
    end.

rent_vod_movie_pre(S) ->
    S#state.vod_movies =/= [] andalso % assumes that find_vod_rented_movies will always return a non-empty list
	S#state.purchase_videoclub =/= undefined.

rent_vod_movie_args(S) -> 
    [S#state.current_token,
        {call, lists, nth, [1, S#state.vod_movies]}
    ].

rent_vod_movie_next(S, _V, [_Token, Movie]) ->
    UserId = S#state.current_user_id,
    UserRentedVodMovies = case lists:keyfind(UserId, 1, S#state.vod_rented_movies) of
        false ->
            {UserId, [Movie]};
        {UserId, Movies} ->
            {UserId, [Movie | lists:delete(Movie, Movies)]}
    end,
    S#state {
        vod_rented_movies = [UserRentedVodMovies |
                lists:keydelete(UserId, 1, S#state.vod_rented_movies)]
    }.

rent_vod_movie_post(_S, _Args, {error, R}) ->
    tag([{{rent_vod_movie, R}, false}]);
rent_vod_movie_post(_S, [_Token, Movie], R) ->
    ReturnedMovie = proplists:get_value("asset", R),
    tag([{{rent_vod_movie, R}, (proplists:get_value("id", ReturnedMovie) ==
            proplists:get_value("id", Movie))}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find vod rented movies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_vod_rented_movies(Token, StartIndex, Count) ->
    case vodkatv_connector:find_vod_rented_movies(Token, StartIndex, Count) of
        {ok, R} ->
            proplists:get_value("assets", R);
        Other ->
            {error, Other}
    end.

find_vod_rented_movies_args(S) -> 
    [S#state.current_token, 1, ?MAX_VOD_MOVIES].

find_vod_rented_movies_pre(S) ->
    S#state.purchase_videoclub =/= undefined.

find_vod_rented_movies_post(_S, _Args, {error, R}) ->
    tag([{{find_vod_rented_movies, R}, false}]);
find_vod_rented_movies_post(S, _Args, R) ->
    UserId = S#state.current_user_id,
    UserVoDRentedMovies = case lists:keyfind(UserId, 1, S#state.vod_rented_movies) of
        false ->
            [];
        {UserId, Movies} ->
            Movies
    end,
    tag([{{find_vod_rented_movies, UserVoDRentedMovies, R},
        equals_vod_movies(UserVoDRentedMovies, R)}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user_id(N)->
    ?EQC_PREFFIX ++ integer_to_list(N).

gen_password() ->
    ?LET(N, nat(), ?EQC_PREFFIX ++ "pass_" ++ integer_to_list(N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tag([]) ->
    true;
tag([{_Name, true} | MoreTags]) ->
    tag(MoreTags);
tag([{Name, false} | _MoreTags]) ->
    Name.

find_product(Name, List) ->
    Product = lists:filter(fun(Element) ->
        case is_list(Element) of
            true ->
                proplists:get_value("name", Element) == Name;
            false ->
                false
        end
    end, List),
    case Product of
        [] ->
            undefined;
        [P | _] ->
            P
    end.

contains_tv_channel(TVChannel, TVChannels) ->
    Id = proplists:get_value("vodkatvChannelId", TVChannel),
    lists:any(fun(T) ->
        proplists:get_value("vodkatvChannelId", T) == Id
    end, TVChannels).

equals_tv_channels(TVChannels1, TVChannels2) ->
    lists:all(fun(TVChannel) ->
        contains_tv_channel(TVChannel, TVChannels2)
    end, TVChannels1) andalso
    lists:all(fun(TVChannel) ->
        contains_tv_channel(TVChannel, TVChannels1)
    end, TVChannels2).

contains_vod_movie(Movie, Movies) ->
    Id = proplists:get_value("id", Movie),
    lists:any(fun(T) ->
        proplists:get_value("id", T) == Id
    end, Movies).

equals_vod_movies(Movies1, Movies2) ->
    lists:all(fun(Movie) ->
        contains_vod_movie(Movie, Movies2)
    end, Movies1) andalso
    lists:all(fun(Movie) ->
        contains_vod_movie(Movie, Movies1)
    end, Movies2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setup/teardown
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setup() ->
    initialize_vodkatv(),
    fun teardown/0.

teardown() ->
    initialize_vodkatv().

initialize_vodkatv() ->
    {ok, Rooms} = vodkatv_connector:find_all_rooms(),
    lists:map(fun(Room) ->
        RoomId = proplists:get_value("roomId", Room),
        case string:str(RoomId, ?EQC_PREFFIX) of
            1 ->
                vodkatv_connector:delete_room_device_session(RoomId);
            _ ->
                ok
        end
    end, Rooms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Property
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop() ->
    ?SETUP(fun setup/0,
        ?FORALL(Cmds, noshrink(commands(?MODULE)),
		numtests(
		  300,
		  begin
		      initialize_vodkatv(),
		      {H, S, Res} = run_commands(?MODULE, Cmds),
		      pretty_commands(?MODULE, Cmds, {H, S, Res},
			    aggregate(command_names(Cmds),
				      Res == ok))
		  end))).

start()->
    case eqc:quickcheck(prop()) of
        true ->
            eqc_fsm:visualize(?MODULE);
        Error ->
            Error
    end.
