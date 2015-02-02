-module(vodkatv_eqc_fsm).

-include_lib("eqc/include/eqc.hrl"). 
-include_lib("eqc/include/eqc_fsm.hrl"). 

-compile(export_all).

-record(state, {
    channels,
    favorite_channels
}).

initial_state() ->
    not_logged.

initial_state_data() ->
    #state {
        channels = [],
        favorite_channels = []
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% States
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logged() ->
    [{not_logged, logout},
     {logged, op},
     {tv_purchased, purchase_tv},
     {vod_purchased, purchase_vod},
     {radio_purchased, purchase_radio},
     {preferences_purchased, purchase_preferences}].

not_logged() ->
    [{logged, login},
     {waiting_for_activation, register_user},
     {password_recovery, activate_password_recovery}].

waiting_for_activation()->
    [{logged, activate}].

password_recovery()->
    [{not_logged, change_password_from_code}].

tv_purchased() ->
    [{logged, cancel_tv},
     {tv_purchased, find_channels},
     {tv_purchased, find_channel_by_id}].

vod_purchased() ->
    [{logged, cancel_vod}].

radio_purchased() ->
    [{logged, cancel_radio}].

preferences_purchased() ->
    [{logged, cancel_preferences}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Login
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
login() ->
    %io:format("Login~n").
    ok.

login_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logout() ->
    %io:format("Logout~n").
    ok.

logout_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Op
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
op() ->
    %io:format("Op~n").
    ok.

op_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register user
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_user() ->
    %io:format("Register user~n").
    ok.

register_user_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Activate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
activate() ->
    %io:format("Activate~n").
    ok.

activate_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Password Recovery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
activate_password_recovery() ->
    %io:format("Password Recovery~n").
    ok.

activate_password_recovery_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Change password from code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_password_from_code() ->
    %io:format("Change password from code~n").
    ok.

change_password_from_code_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase tv
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
purchase_tv() ->
    %io:format("Purchase tv~n").
    ok.

purchase_tv_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel tv
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cancel_tv() ->
    %io:format("Cancel tv~n").
    ok.

cancel_tv_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase vod
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
purchase_vod() ->
    %io:format("Purchase vod~n").
    ok.

purchase_vod_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel vod
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cancel_vod() ->
    %io:format("Cancel vod~n").
    ok.

cancel_vod_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase radio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
purchase_radio() ->
    %io:format("Purchase radio~n").
    ok.

purchase_radio_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel radio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cancel_radio() ->
    %io:format("Cancel vod~n").
    ok.

cancel_radio_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purchase preferences
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
purchase_preferences() ->
    %io:format("Purchase preferences~n").
    ok.

purchase_preferences_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cancel preferences
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cancel_preferences() ->
    %io:format("Cancel preferences~n").
    ok.

cancel_preferences_args(_From, _To, _S) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_channels() ->
    %io:format("Find channels~n").
    ok.

find_channels_args(_From, _To, _S) -> [].

find_channels_next(From, To, S, _V, Args) ->
    S#state {
        channels = ["c1"]
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find channel by id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_channel_by_id(ChannelId) ->
    %io:format("find_channel_by_id~n").
    ok.

find_channel_by_id_pre(_From, _To, S, _Args) ->
    true.

find_channel_by_id_args(_From, _To, S) ->
    [oneof(S#state.channels)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Property
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop() ->
    ?FORALL(Cmds,commands(?MODULE),
  	    begin
      		{H,S,Res} = run_commands(?MODULE,Cmds),
      		Res == ok
  	    end).
