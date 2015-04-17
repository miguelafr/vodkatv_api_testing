-module(vodkatv_eqc_fsm).

-include_lib("eqc/include/eqc.hrl"). 
-include_lib("eqc/include/eqc_fsm.hrl"). 

-compile(export_all).

-define(EQC_PREFFIX, "eqc_user_").

-record(state, {
    valid_users,
    not_activated_users,
    current_user_id,
    current_token%,
    %channels,
    %favorite_channels
}).

initial_state() ->
    not_logged.

initial_state_data() ->
    #state {
        valid_users = [],
        not_activated_users = [],
        current_user_id = undefined,
        current_token = undefined %,
        %channels = [],
        %favorite_channels = []
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% States
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logged() ->
    [{not_logged, logout}].%,
     %{logged, find_user_info}]. %,
     %{logged, op},
     %{tv_purchased, purchase_tv},
     %{vod_purchased, purchase_vod},
     %{radio_purchased, purchase_radio},
     %{preferences_purchased, purchase_preferences}].

not_logged() ->
    [{logged, login},
     %{not_logged, login_error},
     {not_logged, register_user},
     {not_logged, register_user_duplicated}].%,
     %{waiting_for_activation, register_user},
     %{password_recovery, activate_password_recovery}].

%waiting_for_activation()->
%    [{logged, activate}].

%password_recovery()->
%    [{not_logged, change_password_from_code}].

%tv_purchased() ->
%    [{logged, cancel_tv},
%     {tv_purchased, find_channels},
%     {tv_purchased, find_channel_by_id}].

%vod_purchased() ->
%    [{logged, cancel_vod}].

%radio_purchased() ->
%    [{logged, cancel_radio}].

%preferences_purchased() ->
%    [{logged, cancel_preferences}].

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

login_args(_From, _To, S) ->
    elements(S#state.valid_users).

login_next(_From, _To, S, V, [UserId, _Password]) ->
    S#state {
        current_user_id = UserId,
        current_token = V
    }.

login_post(_From, _To, _S, _Args, {error, Error}) ->
    tag([{{login_error, Error}, false}]);
login_post(_From, _To, _S, _Args, R) ->
    tag([{login_error, (R /= undefined) andalso (R /= "")}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Login error
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
login_error(UserId, Password) ->
    vodkatv_connector:login(UserId, Password).

login_error_args(_From, _To, _S) ->
    elements([["invalid_user1", "invalid_password1"],
            ["invalid_user2", "invalid_password2"],
            ["invalid_user3", "invalid_password3"]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logout(Token) ->
    vodkatv_connector:logout(Token).

logout_args(_From, _To, S) ->
    [S#state.current_token].

logout_next(_From, _To, S, _V, _Args) ->
    S#state {
        current_user_id = undefined,
        current_token = undefined
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find user info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_user_info(Token) ->
    vodkatv_connector:find_user_info(Token).

find_user_info_args(_From, _To, S) ->
    [S#state.current_token].

find_user_info_post(_From, _To, _S, _Args, {error, _Error}) ->
    tag([{find_user_info, false}]);
find_user_info_post(_From, _To, S, _Args, {ok, Result}) ->
    UserInfo = proplists:get_value("userInfo", Result),
    UserId = proplists:get_value("userId", UserInfo),
    tag([{find_user_info_error, (UserId == S#state.current_user_id)}]);
find_user_info_post(_From, _To, _S, _Args, _Result) ->
    tag([{find_user_info, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register user
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_user(UserName, Password) ->
    vodkatv_connector:register_user(UserName, Password).

register_user_next(_From, _To, S, _V, [UserName, Password]) ->
    S#state {
        not_activated_users = [[UserName, Password]| S#state.not_activated_users]
    }.

register_user_args(_From, _To, S) ->
    ExistingUsers = S#state.valid_users ++ S#state.not_activated_users,
    ?LET(N,
        ?SUCHTHAT(N, nat(), not lists:member(user_id(N), ExistingUsers)),
        user_id(N)).

register_user_post(_From, _To, _S, [UserName, _Password],
        {ok, Result}) ->
    UserSession = proplists:get_value("userSession", Result),
    [Account | []] = proplists:get_value("accounts", UserSession),
    Access = proplists:get_value("access", Account),
    UserId = proplists:get_value("loginName", Access),
    tag([{{register_user, UserId, UserName}, (UserId == UserName)}]);
register_user_post(_From, _To, _S, _Args, _Result) ->
    tag([{register_user, false}]).

user_id(N)->
    [?EQC_PREFFIX ++ integer_to_list(N), ?EQC_PREFFIX ++ integer_to_list(N)].
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register user duplicated
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_user_duplicated(UserName, Password) ->
    vodkatv_connector:register_user(UserName, Password).

register_user_duplicated_args(_From, _To, S) ->
    oneof(S#state.valid_users ++ S#state.not_activated_users).

register_user_duplicated_post(_From, _To, _S, [UserName, _Password],
        {error, {409, _Msg, Result}}) ->
    [Error | []] = proplists:get_value("errors", Result),
    [ErrorParam | []] = proplists:get_value("params", Error),
    UserId = proplists:get_value("value", ErrorParam),
    tag([{register_user_duplicated, (UserId == UserName)}]);
register_user_duplicated_post(_From, _To, _S, _Args, _Result) ->
    tag([{register_user_duplicated, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Op
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%op() ->
    %io:format("Op~n").
%    ok.

%op_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register user
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%register_user() ->
    %io:format("Register user~n").
%    ok.

%register_user_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Activate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%activate() ->
    %io:format("Activate~n").
%    ok.

%activate_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Password Recovery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%activate_password_recovery() ->
    %io:format("Password Recovery~n").
%    ok.

%activate_password_recovery_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change password from code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%change_password_from_code() ->
    %io:format("Change password from code~n").
%    ok.

%change_password_from_code_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase tv
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%purchase_tv() ->
    %io:format("Purchase tv~n").
%    ok.

%purchase_tv_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel tv
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cancel_tv() ->
    %io:format("Cancel tv~n").
%    ok.

%cancel_tv_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase vod
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%purchase_vod() ->
    %io:format("Purchase vod~n").
%    ok.

%purchase_vod_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel vod
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cancel_vod() ->
    %io:format("Cancel vod~n").
%    ok.

%cancel_vod_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase radio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%purchase_radio() ->
    %io:format("Purchase radio~n").
%    ok.

%purchase_radio_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel radio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cancel_radio() ->
    %io:format("Cancel vod~n").
%    ok.

%cancel_radio_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase preferences
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%purchase_preferences() ->
    %io:format("Purchase preferences~n").
%    ok.

%purchase_preferences_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel preferences
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cancel_preferences() ->
    %io:format("Cancel preferences~n").
%    ok.

%cancel_preferences_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%find_channels() ->
    %io:format("Find channels~n").
%    ok.

%find_channels_args(_From, _To, _S) -> [].

%find_channels_next(From, To, S, _V, Args) ->
%    S#state {
%        channels = ["c1"]
%    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find channel by id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%find_channel_by_id(ChannelId) ->
    %io:format("find_channel_by_id~n").
%    ok.

%find_channel_by_id_pre(_From, _To, S, _Args) ->
%    true.

%find_channel_by_id_args(_From, _To, S) ->
%    [oneof(S#state.channels)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tag([]) ->
    true;
tag([{_Name, true} | MoreTags]) ->
    tag(MoreTags);
tag([{Name, false} | _MoreTags]) ->
    Name.

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
        ?FORALL(Cmds, (commands(?MODULE)),
        begin
            initialize_vodkatv(),
            {H, S, Res} = run_commands(?MODULE, Cmds),
            ?WHENFAIL((io:format("H: ~p ~n S: ~p ~n Res: ~p ~n", [H, S, Res])),
            (aggregate(command_names(Cmds), Res == ok)))
        end)).

start()->
    eqc:quickcheck(prop()).
