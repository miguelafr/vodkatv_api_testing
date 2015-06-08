-module(vodkatv_eqc_new_simple).

-include("vodkatv.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-define(SUT, vodkatv_sut).

-compile(export_all).

-record(state, {
        rooms,
        devices
    }).

%%===============================================================
%% Prop
%%===============================================================
prop_state_machine() ->
    error_logger:tty(false),
    inets:start(),
    vodkatv_sut:start(),
    ?SETUP(
       fun setup/0,
       ?FORALL(
          Cmds, commands(?MODULE),
          numtests(
            100,
            begin
                setup(),
                %A = now(),
                {H, S, Res} = run_commands(?MODULE, Cmds),
                %B = now(),
                %io:format("~n> ~p requests in ~p ms. (~p requests per second)",
                %    [length(Cmds), timer:now_diff(B,A)/1000,
                %     (length(Cmds) / (timer:now_diff(B,A)/1000000))]),
                teardown(),
                pretty_commands(?MODULE, Cmds, {H,S,Res}, Res==ok)
            end))).

%%===============================================================
%% eqc callbacks
%%===============================================================
%%---------------------------------------------------------------
%% initial_state
%%---------------------------------------------------------------
initial_state()->
    #state {
       rooms = [],
       devices = []
      }.

%%---------------------------------------------------------------
%% create_room
%%---------------------------------------------------------------
create_room_args(S) ->
    [gen_room_id(S), gen_description()].

create_room(RoomId, Description)->
    sut_result(?SUT:create_room(RoomId, Description, undefined)).

create_room_pre(_S) ->
    true.

create_room_post(_S, Params, {error, Reason})->
    {error, {create_room, Params}, Reason};
create_room_post(_S, ["", _Description], Result) ->
    vodkatv_test_utils:check_simple_errors(Result#room.errors,
        "required", "roomId", "");
create_room_post(S, [RoomId, Description], Result)->
    case vodkatv_test_utils:search_room(RoomId, S#state.rooms) of
        {value, _Room} ->
            vodkatv_test_utils:check_simple_errors(Result#room.errors,
                "duplicated", "roomId", RoomId);
        false ->
            Result#room.roomId == RoomId
                andalso Result#room.description == get_description(Description)
    end.

create_room_next(S, _R, [RoomId, _Description])
    when RoomId =:= "" -> S;
create_room_next(S, _R, [RoomId, Description])->
    case vodkatv_test_utils:search_room(RoomId, S#state.rooms) of
        {value, _Room} ->
            S;
        false ->
            NewRoom = #roomType{
                         anyAttrs = [],
                         roomId = RoomId,
                         description = get_description(Description)
                        },
            S#state {
              rooms = vodkatv_test_utils:add(NewRoom, S#state.rooms)
            }
    end.

%%---------------------------------------------------------------
%% find_all_rooms
%%---------------------------------------------------------------
find_all_rooms_args(_S) ->
    [].

find_all_rooms() ->
    sut_result(?SUT:find_all_rooms()).

find_all_rooms_pre(_S) ->
    true.

find_all_rooms_post(_S, Params, {error, Reason})->
    {error, {create_room, Params}, Reason};
find_all_rooms_post(S, [], Result)->
    Rooms = get_value_or_empty_list(Result#rooms.room),
    lists:all(
        fun(Room) ->
            lists:member(Room, S#state.rooms)
        end, Rooms)
    andalso
    lists:all(
        fun(Room) ->
            lists:member(Room, Rooms)
        end, S#state.rooms).

%%---------------------------------------------------------------
%% delete_room
%%---------------------------------------------------------------
delete_room_args(S) ->
    [gen_room_ids(S)].

delete_room(RoomIds)->
    sut_result(?SUT:delete_room(RoomIds)).

delete_room_pre(_S) ->
    true.

delete_room_post(_S, Params, {error, Reason})->
    {error, {create_room, Params}, Reason};
delete_room_post(_S, [[]], Result)->
    vodkatv_test_utils:check_simple_errors(Result#rooms.errors,
        "required", "roomId", "");
delete_room_post(_S, [[""]], Result)->
    vodkatv_test_utils:check_simple_errors(Result#rooms.errors,
        "required", "roomId", "");
delete_room_post(S, [RoomIds], Result)->
    lists:all(
        fun(RoomId) ->
            case RoomId of
                "" ->
                    true;
                _ ->
                    case vodkatv_test_utils:search_room(RoomId, S#state.rooms) of
                        {value, _Room} ->
                            lists:member(
                                #roomType {
                                    anyAttrs = [],
                                    roomId = RoomId
                                },
                                Result#rooms.room);
                        false ->
                            lists:any(
                                fun(Error) ->
                                    vodkatv_test_utils:check_simple_error(
                                        Error, "not_found", "roomId", RoomId)
                                end, (Result#rooms.errors)#errors.error)
                    end
            end
      end, RoomIds).

delete_room_next(S, _R, [RoomIds])->
    lists:foldl(
        fun(RoomId, NewState) ->
            case vodkatv_test_utils:search_room(RoomId,
                    NewState#state.rooms) of
                {value, Room} ->
                    NewState#state {
                        rooms = lists:delete(Room, NewState#state.rooms),
                        devices = lists:filter(
                                fun(Device) ->
                                        Device#deviceType.roomId /= RoomId
                                end, NewState#state.devices)
                    };
                false ->
                    NewState
            end
      end, S, RoomIds).

%%---------------------------------------------------------------
%% Generators
%%---------------------------------------------------------------
gen_char()->
    oneof([choose($a, $z), choose($A, $Z), choose($0, $9)]).

gen_string() ->
    list(gen_char()).

gen_room_id(S)->
    gen_new_or_in_use(
      fun() -> gen_string() end,
      S#state.rooms,
      fun(Room) -> Room#roomType.roomId end).

gen_room_ids(S)->
    gen_list_without_undefined(
        fun() ->
            gen_list_without_duplicates(
                fun() ->
                    list(gen_room_id(S))
                end)
        end).

gen_description() ->
    gen_string().

gen_new_or_in_use(FunGenNew, Used, FunAttrUsed)->
    oneof([?LET(X, oneof(Used), FunAttrUsed(X)) || Used /= []]
	  ++ [FunGenNew()]).

gen_list_without_duplicates(G)->
    ?LET(R, G(), sets:to_list(sets:from_list(R))).

gen_list_without_undefined(G)->
    %% Alternative implementation:
    %% ?SUCHTHAT(X, G(), not lists:member(undefined, X)).
    ?LET(X, G(), lists:delete(undefined, X)).

%%---------------------------------------------------------------
%% Utilities
%%---------------------------------------------------------------
setup()->
    delete_all_rooms(),
    fun teardown/0.

teardown()->
    %inets:stop().
    ok.

get_description([]) ->
    undefined;
get_description(Description) ->
    Description.

get_value_or_empty_list(undefined) ->
    [];
get_value_or_empty_list(Value) ->
    Value.

sut_result({ok, Result}) ->
    Result;
sut_result({error, Reason}) ->
    {error, Reason}.

delete_all_rooms()->
    case vodkatv_sut:find_all_rooms() of
        {ok, Response} ->
            Rooms = get_value_or_empty_list(Response#rooms.room),
            lists:map(
                fun(Room) ->
                    vodkatv_sut:delete_room([Room#roomType.roomId])
                end, Rooms);
        Error ->
            throw(Error)
    end.
