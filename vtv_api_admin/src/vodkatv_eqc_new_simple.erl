-module(vodkatv_eqc_new).

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
            1000,
            begin
                setup(),
                A = now(),
                {H, S, Res} = run_commands(?MODULE, Cmds),
                B = now(),
                io:format("~n> ~p requests in ~p ms. (~p requests per second)",
                    [length(Cmds), timer:now_diff(B,A)/1000,
                     (length(Cmds) / (timer:now_diff(B,A)/1000000))]),
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
    check_simple_errors(Result#room.errors, "required", "roomId", "");
create_room_post(S, [RoomId, Description], Result)->
    case search_room(RoomId, S#state.rooms) of
        false ->
            Result#room.roomId == RoomId
                andalso Result#room.description == get_description(Description);
        {ok, _Room} ->
            check_simple_errors(Result#room.errors, "duplicated",
                                "roomId", RoomId)
    end.

create_room_next(S, _R, [RoomId, _Description])
  when RoomId =:= "" -> S;
create_room_next(S, _R, [RoomId, Description])->
    case search_room(RoomId, S#state.rooms) of
        false ->
            NewRoom = #roomType{
                         anyAttrs = [],
                         roomId = RoomId,
                         description = get_description(Description)
                        },
            S#state {
              rooms = [NewRoom |  S#state.rooms]
             };
        {ok, _Room} ->
            S
    end.

%%---------------------------------------------------------------
%% Generators
%%---------------------------------------------------------------
gen_char()->
    oneof([choose($a, $z), choose($A, $Z), choose($0, $9)]).

gen_string() ->
    list(gen_char()).

gen_mac()->
    ?LET({A, B, C, D, E, F, G, H, I, J, L, M},
	 {gen_char(), gen_char(),
	  gen_char(), gen_char(),
	  gen_char(), gen_char(),
	  gen_char(), gen_char(),
	  gen_char(), gen_char(),
	  gen_char(), gen_char()},
	 [A, B, $:, C, D, $:, E, F, $:, G, H, $:, I, J, $:, L, M]).

gen_order()->
    gen_undefined_or_value(
      fun() ->
	      elements(["ascending", "descending"])
      end).

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

gen_device_id(S)->
    gen_new_or_in_use(
      fun() ->
        gen_undefined_or_value(
        fun() ->
            nat()
        end)
      end,
      S#state.devices,
      fun(Device) -> Device#deviceType.id end).

gen_device_ids(S)->
    gen_list_without_undefined(
      fun() ->
	      gen_list_without_duplicates(
		fun() ->
			list(gen_device_id(S))
		end)
      end).

gen_physical_id(S)->
    gen_new_or_in_use(
      fun() -> gen_empty_string_or_value(fun gen_mac/0) end,
      S#state.devices,
      fun(Device) -> Device#deviceType.physicalId end).

gen_device_class()->
    elements(["STBHED",
	      "IPHONE",
	      "SMARTPHONE",
	      "MOBILEPHONE",
	      "TABLET",
	      "PC",
	      "OTHER",
	      undefined]).

gen_description() ->
    gen_string().

gen_devices_sort_by() ->
    gen_undefined_or_value(
      fun() ->
	      elements(["physicalId", "description"])
      end).

gen_devices_query()->
    gen_undefined_or_value(fun gen_string/0).

gen_start_index([])->
    ?SUCHTHAT(I, nat(), I > 0);
gen_start_index(Elements)->
    frequency([
        {80, choose(1, length(Elements))},
        {20, ?SUCHTHAT(I, nat(), I > length(Elements))}
    ]).

gen_count()->
    ?SUCHTHAT(I, nat(), I > 0).

gen_list_without_duplicates(G)->
    ?LET(R, G(), sets:to_list(sets:from_list(R))).

gen_new_or_in_use(FunGenNew, Used, FunAttrUsed)->
    oneof([?LET(X, oneof(Used), FunAttrUsed(X)) || Used /= []]
	  ++ [FunGenNew()]).

gen_empty_string_or_value(G)->
    frequency([{80, G()}, {20, return("")}]).

gen_undefined_or_value(G)->
    frequency([{80, G()}, {20, return(undefined)}]).

gen_list_without_undefined(G)->
    %% Alternative implementation:
    %% ?SUCHTHAT(X, G(), not lists:member(undefined, X)).
    ?LET(X, G(), lists:delete(undefined, X)).

%%---------------------------------------------------------------
%% Rooms
%%---------------------------------------------------------------
%% @throws room_not_found
-spec search_room(string(), list(#room{})) -> #room{}.
search_room(RoomId, Rooms) ->
    case search(RoomId, Rooms,
                fun(Id, Room) -> Room#roomType.roomId == Id end) of
        {value, Room} -> Room;
        false -> throw(room_not_found)
    end.

%%---------------------------------------------------------------
%% Devices
%%---------------------------------------------------------------
%% @throws device_not_found
%-spec search_device(string(), list(#device{})) -> #device{}.
search_device(DeviceId, Devices) ->
    case search(DeviceId, Devices,
                fun(Id, Device) -> Device#deviceType.id == Id end) of
        {value, Device} -> Device;
        false -> throw(device_not_found)
    end.

%% @throws device_by_physical_id_not_found
-spec search_device_by_physical_id(string(), list(#device{})) -> #device{}.
search_device_by_physical_id(PhysicalId, Devices) ->
    case search(PhysicalId, Devices,
                fun(Id, Device) ->
			string:to_lower(Device#deviceType.physicalId) == string:to_lower(Id)
		end) of
        {value, Device} -> Device;
        false -> throw(device_by_physical_id_not_found)
    end.

filter_devices(Devices, undefined)->
    Devices;
filter_devices(Devices, "")->
    Devices;
filter_devices([], _Query)->
    [];
filter_devices([Device | Devices], Query) ->
    Match = lists:any(
        fun(Field)->
            case Field of
                undefined ->
                    false;
                _ ->
                    case re:run(string:to_lower(Field),
                        string:to_lower(Query)) of
                            nomatch ->
                                false;
                            _ ->
                                true
                    end
            end
        end,
        [Device#deviceType.physicalId,
            Device#deviceType.roomId,
            Device#deviceType.description]),

        case Match of
            true ->
                [Device | filter_devices(Devices, Query)];
            false ->
                filter_devices(Devices, Query)
        end.

%%---------------------------------------------------------------
%% Device class
%%---------------------------------------------------------------
get_device_class(undefined) ->
    "OTHER";
get_device_class(X) ->
    X.

get_description([]) ->
    undefined;
get_description(Description) ->
    Description.

equals_device_class(D1, D2) ->
    get_device_class(D1) == get_device_class(D2).

%%---------------------------------------------------------------
%% Errors
%%---------------------------------------------------------------
check_simple_errors(_Errors, _ErrorCode, _ParamName, _ParamValue)->
    false.

%check_simple_errors(undefined, _ErrorCode, _ParamName, _ParamValue)->
%    false;

%check_simple_errors(Errors, ErrorCode, ParamName, ParamValue)->
%    Error = lists:nth(1, Errors#errors.error),
%    check_simple_error(Error, ErrorCode, ParamName, ParamValue).

%check_simple_error(Error, ErrorCode, ParamName, ParamValue)
%        when is_integer(ParamValue) ->
%    check_simple_error(Error, ErrorCode, ParamName, 
%        integer_to_list(ParamValue));

%check_simple_error(Error, ErrorCode, ParamName, ParamValue)
%        when Error#error.code == ErrorCode,
%            length((Error#error.params)#errorParams.param) == 1 ->
%    ErrorParam = lists:nth(1, (Error#error.params)#errorParams.param),
%    ErrorParam#errorParam.name == ParamName
%        andalso ErrorParam#errorParam.value == ParamValue;

%check_simple_error(_Error, _ErrorCode, _ParamName, _ParamValue) ->
%    false.

%%---------------------------------------------------------------
%% Utilities
%%---------------------------------------------------------------
setup()->
    delete_all_rooms(),
    fun teardown/0.

teardown()->
    %inets:stop().
    ok.

search(_Id, [], _Eq)->
    false;
search(Id, [X | Xs], Eq) ->
    case Eq(Id, X) of
	true ->
	    {value, X};
	false ->
	    search(Id, Xs, Eq)
    end.

check_order(_F, [], _Order) ->
    true;
check_order(_F, [_A], _Order) ->
    true;
check_order(F, [A, B], "ascending")->
    F(A, B);
check_order(F, [A, B], "descending") ->
    F(B, A);
check_order(F, [A, B| C], Order)->
    check_order(F, [A, B], Order) andalso
        check_order(F, [B | C], Order).

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

%%-----------------------------------------------------------------------------
%% Adapter functions
%%-----------------------------------------------------------------------------
find_all_rooms() ->
    sut_result(?SUT:find_all_rooms()).

delete_room(RoomIds)->
    sut_result(?SUT:delete_room(RoomIds)).

create_device(PhysicalId, DeviceClass, RoomId, Description) ->
    case sut_result(?SUT:create_device(PhysicalId, DeviceClass, RoomId,
                    Description, undefined)) of
        {error, Reason} -> {error, Reason};
        Result -> {Result#device.id, Result}
    end.

find_devices(StartIndex, Count, SortBy, Order, Query) ->
    sut_result(?SUT:find_devices(StartIndex, Count, SortBy, Order, Query)).

find_devices_by_room(RoomId) ->
    sut_result(?SUT:find_devices_by_room(RoomId)).

find_device_by_id(DeviceId) ->
    sut_result(?SUT:find_device_by_id(DeviceId)).

update_device(DeviceId, PhysicalId, DeviceClass, RoomId, Description) ->
    sut_result(?SUT:update_device(DeviceId, PhysicalId, DeviceClass, RoomId,
            Description, undefined)).

delete_device(DeviceIds)->
    sut_result(?SUT:delete_device(DeviceIds)).
