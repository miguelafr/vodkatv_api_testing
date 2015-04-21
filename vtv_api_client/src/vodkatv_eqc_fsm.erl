-module(vodkatv_eqc_fsm).

-include_lib("eqc/include/eqc.hrl"). 
-include_lib("eqc/include/eqc_fsm.hrl"). 

-compile(export_all).

-define(EQC_PREFFIX, "eqc_user_").

-record(state, {
    valid_users,
    not_activated_users,
    activation_codes,
    password_forgotten_users,
    password_recovery_codes,
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
        activation_codes = [],
        password_forgotten_users = [],
        password_recovery_codes = [],
        current_user_id = undefined,
        current_token = undefined %,
        %channels = [],
        %favorite_channels = []
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% States
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logged() ->
    [{not_logged, logout},
     {logged, find_user_info}]. %,
     %{tv_purchased, purchase_tv},
     %{vod_purchased, purchase_vod},
     %{radio_purchased, purchase_radio},
     %{preferences_purchased, purchase_preferences}].

not_logged() ->
    [{logged, login},
     {not_logged, login_error},
     {not_logged, register_user_duplicated},
     {waiting_for_activation_code, register_user},
     {waiting_for_password_recovery_code, password_recovery}].

waiting_for_activation_code() ->
    [{activation_code_received, get_activation_code}].

activation_code_received() ->
    [{logged, activate_user}].

waiting_for_password_recovery_code() ->
    [{password_recovery_code_received, get_password_recovery_code}].

password_recovery_code_received()->
    [{not_logged, change_password_from_code}].

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
    ?LET({UserId, Password}, elements(S#state.valid_users), [UserId, Password]).

login_next(_From, _To, S, V, [UserId, _Password]) ->
    S#state {
        current_user_id = UserId,
        current_token = V
    }.

login_post(_From, _To, _S, _Args, {error, Error}) ->
    tag([{{login, Error}, false}]);
login_post(_From, _To, _S, _Args, R) ->
    tag([{login, (R /= undefined) andalso (R /= "")}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Login error
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
login_error(UserId, Password) ->
    vodkatv_connector:login(UserId, Password).

login_error_args(_From, _To, _S) ->
    elements([["invalid_user1", "invalid_password1"],
            ["invalid_user2", "invalid_password2"],
            ["invalid_user3", "invalid_password3"]]).

login_error_post(_From, _To, _S, _Args, {ok, R}) ->
    tag([{{login_error, R},
        (proplists:get_value("errors", R) /= undefined)}]);
login_error_post(_From, _To, _S, _Args, R) ->
    tag([{{login_error, R}, false}]).

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

find_user_info_post(_From, _To, S, _Args, {ok, R}) ->
    UserInfo = proplists:get_value("userInfo", R),
    UserId = proplists:get_value("userId", UserInfo),
    tag([{find_user_info, (UserId == S#state.current_user_id)}]);
find_user_info_post(_From, _To, _S, _Args, R) ->
    tag([{{find_user_info, R}, false}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register user
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_user(UserId, Password) ->
    vodkatv_connector:register_user(UserId, Password).

register_user_next(_From, _To, S, _V, [UserId, Password]) ->
    S#state {
        not_activated_users = [{UserId, Password}| S#state.not_activated_users]
    }.

register_user_args(_From, _To, S) ->
    ExistingUsers = S#state.valid_users ++ S#state.not_activated_users,
    ?LET(N,
        ?SUCHTHAT(N, nat(), (lists:keyfind(user_id(N), 1, ExistingUsers) == false)),
        [user_id(N), gen_password()]).

register_user_post(_From, _To, _S, [UserId, _Password], {ok, R}) ->
    UserSession = proplists:get_value("userSession", R),
    [Account | []] = proplists:get_value("accounts", UserSession),
    Access = proplists:get_value("access", Account),
    ReturnedUserId = proplists:get_value("loginName", Access),
    tag([{{register_user, UserId, ReturnedUserId}, (UserId == ReturnedUserId)}]);
register_user_post(_From, _To, _S, _Args, R) ->
    tag([{{register_user, R}, false}]).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register user duplicated
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_user_duplicated(UserId, Password) ->
    vodkatv_connector:register_user(UserId, Password).

register_user_duplicated_args(_From, _To, S) ->
    ?LET({UserId, _Password},
        elements(S#state.valid_users ++ S#state.not_activated_users),
        [UserId, gen_password()]).

register_user_duplicated_post(_From, _To, _S, [UserId, _Password],
        {error, {409, _Msg, Result}}) ->
    [Error | []] = proplists:get_value("errors", Result),
    [ErrorParam | []] = proplists:get_value("params", Error),
    ReturnedUserId = proplists:get_value("value", ErrorParam),
    tag([{register_user_duplicated, (UserId == ReturnedUserId)}]);
register_user_duplicated_post(_From, _To, _S, _Args, R) ->
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

get_activation_code_args(_From, _To, S) ->
    ?LET({UserId, _Password}, elements(S#state.not_activated_users), [UserId]).

get_activation_code_next(_From, _To, S, V, [UserId]) ->
    S#state {
        activation_codes = [{UserId, V} | S#state.activation_codes]
    }.

get_activation_code_post(_From, _To, _S, _Args, {error, Error}) ->
    tag([{{activation_code, Error}, false}]);
get_activation_code_post(_From, _To, _S, _Args, R) ->
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

activate_user_args(_From, _To, S) ->
    ?LET({_UserId, ActivationCode}, elements(S#state.activation_codes),
        [ActivationCode]).

activate_user_next(_From, _To, S, V, [ActivationCode]) ->
    {UserId, _ActivationCode} = lists:keyfind(ActivationCode, 2, S#state.activation_codes),
    {_UserId, Password} = lists:keyfind(UserId, 1, S#state.not_activated_users),
    S#state {
        valid_users = [{UserId, Password} | S#state.valid_users],
        not_activated_users = lists:keydelete(UserId, 1,
            S#state.activation_codes),
        activation_codes = lists:keydelete(UserId, 1,
            S#state.activation_codes),
        current_user_id = UserId,
        current_token = V
    }.

activate_user_post(_From, _To, _S, _Args, {error, Error}) ->
    tag([{{activate_user, Error}, false}]);
activate_user_post(_From, _To, _S, _Args, R) ->
    tag([{activate_user, (R /= undefined) andalso (R /= "")}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Password recovery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
password_recovery(UserId) ->
    vodkatv_connector:password_recovery(UserId).

password_recovery_args(_From, _To, S) ->
    ?LET({UserId, _Password}, elements(S#state.valid_users), [UserId]).

password_recovery_next(_From, _To, S, _V, [UserId]) ->
    S#state {
        password_forgotten_users = [UserId | S#state.password_forgotten_users]
    }.

password_recovery_post(_From, _To, _S, [UserId], {ok, R}) ->
    ReturnedUserId = proplists:get_value("loginName", R),
    tag([{{password_recovery, UserId, ReturnedUserId}, (UserId == ReturnedUserId)}]);
password_recovery_post(_From, _To, _S, _Args, R) ->
    tag([{{password_recovery, R}, false}]).

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

get_password_recovery_code_args(_From, _To, S) ->
    [elements(S#state.password_forgotten_users)].

get_password_recovery_code_next(_From, _To, S, V, [UserId]) ->
    S#state {
        password_forgotten_users = lists:delete(UserId,
                S#state.password_forgotten_users),
        password_recovery_codes = [{UserId, V} | S#state.password_recovery_codes]
    }.

get_password_recovery_code_post(_From, _To, _S, _Args, {error, Error}) ->
    tag([{{password_recovery_code, Error}, false}]);
get_password_recovery_code_post(_From, _To, _S, _Args, R) ->
    tag([{password_recovery_code, (R /= undefined) andalso (R /= "")}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change password from code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_password_from_code(PasswordRecoveryCode, Password) ->
    vodkatv_connector:change_password_from_code(PasswordRecoveryCode, Password).

change_password_from_code_args(_From, _To, S) ->
    ?LET({_UserId, PasswordRecoveryCode},
        elements(S#state.password_recovery_codes),
        [PasswordRecoveryCode, gen_password()]).

change_password_from_code_next(_From, _To, S, _V,
        [PasswordRecoveryCode, Password]) ->
    {UserId, _PasswordRecoveryCode} = lists:keyfind(PasswordRecoveryCode, 2,
        S#state.password_recovery_codes),
    S#state {
        valid_users = [{UserId, Password} | lists:keydelete(UserId, 1,
            S#state.valid_users)],
        password_recovery_codes = lists:keydelete(UserId, 1,
            S#state.password_recovery_codes)
    }.

change_password_from_code_post(_From, _To, S, [PasswordRecoveryCode, _Password],
        {ok, R}) ->
    {UserId, _PasswordRecoveryCode} = lists:keyfind(PasswordRecoveryCode, 2,
        S#state.password_recovery_codes),
    ReturnedUserId = proplists:get_value("loginName", R),
    tag([{{change_password_from_code, UserId, ReturnedUserId},
        (UserId == ReturnedUserId)}]);
change_password_from_code_post(_From, _To, _S, _Args, R) ->
    tag([{{change_password_from_code, R}, false}]).

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
    case eqc:quickcheck(prop()) of
        true ->
            eqc_fsm:visualize(?MODULE);
        Error ->
            Error
    end.
