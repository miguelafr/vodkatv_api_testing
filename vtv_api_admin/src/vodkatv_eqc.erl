-module(vodkatv_eqc).

-include("vodkatv.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-define(SUT, vodkatv_sut).

-compile(export_all).

%% Prop
-export([prop_state_machine/0]).

%% eqc callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
    next_state/3]).

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
%% command
%%---------------------------------------------------------------
command(S) ->
    oneof(
      [{call, ?MODULE, create_room, [gen_room_id(S), gen_description()]},
       {call, ?MODULE, find_all_rooms, []},
       {call, ?MODULE, delete_room, [gen_room_ids(S)]},

       {call, ?MODULE, create_device, [gen_physical_id(S), gen_device_class(),
                                       gen_room_id(S), gen_description()]},
       {call, ?MODULE, find_devices, [gen_start_index(S#state.devices), gen_count(),
	   			      gen_devices_sort_by(), gen_order(),
	   			      gen_devices_query()]},
       {call, ?MODULE, find_devices_by_room, [gen_room_id(S)]},
       {call, ?MODULE, find_device_by_id, [gen_device_id(S)]},
       {call, ?MODULE, update_device, [gen_device_id(S), gen_physical_id(S),
       				       gen_device_class(), gen_room_id(S),
       				       gen_description()]},
       {call, ?MODULE, delete_device, [gen_device_ids(S)]}
      ]).

%%---------------------------------------------------------------
%% precondition
%%---------------------------------------------------------------
precondition(_S, _C)->
    true.

%%---------------------------------------------------------------
%% postcondition:
%%---------------------------------------------------------------
postcondition(_S, {call, ?MODULE, Op, Params}, {error, Reason})->
    {error, {Op, Params}, Reason};

postcondition(_S, {call, ?MODULE, create_room, ["", _Description]},
            Result)->
    check_simple_errors(Result#room.errors, "required", "roomId", "");
postcondition(S, {call, ?MODULE, create_room, [RoomId, Description]},
            Result)->
    try
        _Room = search_room(RoomId, S#state.rooms),
        check_simple_errors(Result#room.errors, "duplicated",
                            "roomId", RoomId)
    catch room_not_found ->
            Result#room.roomId == RoomId
                andalso Result#room.description == get_description(Description)
    end;

postcondition(S, {call, ?MODULE, find_all_rooms, []}, Result)->
    Rooms = get_value_or_empty_list(Result#rooms.room),
    lists:all(
        fun(Room) ->
            lists:member(Room, S#state.rooms)
        end, Rooms)
    andalso
    lists:all(
        fun(Room) ->
            lists:member(Room, Rooms)
        end, S#state.rooms);

postcondition(_S, {call, ?MODULE, delete_room, [[]]}, Result)->
    check_simple_errors(Result#rooms.errors, "required", "roomId", "");
postcondition(_S, {call, ?MODULE, delete_room, [[""]]}, Result)->
    check_simple_errors(Result#rooms.errors, "required", "roomId", "");
postcondition(S, {call, ?MODULE, delete_room, [RoomIds]}, Result)->
    lists:all(
        fun(RoomId) ->
            case RoomId of
                "" ->
                    true;
                _ ->
                    try
                        _Room = search_room(RoomId, S#state.rooms),
                        lists:member(
                            #roomType {
                                anyAttrs = [],
                                roomId = RoomId
                            },
                            Result#rooms.room)
                    catch room_not_found ->
                        lists:any(
                            fun(Error) ->
                                check_simple_error(
                                    Error, "not_found", "roomId", RoomId)
                            end, (Result#rooms.errors)#errors.error)
                    end
            end
      end, RoomIds);

postcondition(_S, {call, ?MODULE, create_device,
            [PhysicalId, _DeviceClass, _RoomId, _Description]},
            {_DeviceId, Result}) when PhysicalId =:= "" ->
    check_simple_errors(Result#device.errors, "required", "physicalId", "");
postcondition(_S, {call, ?MODULE, create_device,
            [_PhysicalId, _DeviceClass, RoomId, _Description]},
            {_DeviceId, Result}) when RoomId =:= "" ->
    check_simple_errors(Result#device.errors, "required", "roomId", "");
postcondition(S, {call, ?MODULE, create_device,
            [PhysicalId, DeviceClass, RoomId, Description]},
            {_DeviceId, Result})->
    try
        _Room = search_room(RoomId, S#state.rooms),
        _Device = search_device_by_physical_id(PhysicalId, S#state.devices),
        check_simple_errors(Result#device.errors, "duplicated",
                "physicalId", PhysicalId)
    catch
        room_not_found ->
            check_simple_errors(Result#device.errors, "not_found",
                    "roomId", RoomId);
        device_by_physical_id_not_found ->
            is_integer(Result#device.id)
                andalso Result#device.physicalId == PhysicalId
                andalso equals_device_class(Result#device.deviceClass, DeviceClass)
                andalso Result#device.roomId == RoomId
                andalso Result#device.description == get_description(Description)
    end;

postcondition(S, {call, ?MODULE, find_devices,
            [StartIndex, Count, SortBy, Order, Query]},
            Result) ->
    FilteredDevices = filter_devices(S#state.devices, Query),
    Devices = get_value_or_empty_list(Result#devices.device),
    if
        (StartIndex > length(FilteredDevices)) ->
            length(Devices) == 0
            andalso Result#devices.existsMore == false
            andalso Result#devices.countTotal == length(FilteredDevices);
        true ->
            length(Devices) ==
            min(Count, length(FilteredDevices) - StartIndex + 1)
            andalso
            if
                (Count >= length(FilteredDevices) - StartIndex + 1) ->
                    Result#devices.existsMore == false;
                true ->
                    Result#devices.existsMore == true
            end
            andalso Result#devices.countTotal == length(FilteredDevices)
            andalso
                lists:all(
                    fun(Device) ->
                        lists:member(Device, S#state.devices)
                    end, Devices)
            andalso
                case SortBy of
                    "" ->
                        true;
                    undefined ->
                        true;
                    "physicalId" ->
                        check_order(
                            fun(D1, D2) ->
                                D1#deviceType.physicalId =< D2#deviceType.physicalId
                            end, Devices, Order);
                    "description" ->
                        check_order(
                            fun(D1, D2) ->
                                D1#deviceType.description =< D2#deviceType.description
                            end, Devices, Order)
                end
    end;

postcondition(S, {call, ?MODULE, find_devices_by_room, [RoomId]}, Result)->
    Devices = get_value_or_empty_list(Result#devices.device),
    lists:all(
        fun(Device) ->
            Device#deviceType.roomId == RoomId
            andalso lists:member(Device, S#state.devices)
        end, Devices)
    andalso
    lists:all(
        fun(Device) ->
            case Device#deviceType.roomId of
                RoomId ->
                    lists:member(Device, Devices);
                _ ->
                    true
            end
        end, S#state.devices);

postcondition(_S, {call, ?MODULE, find_device_by_id, [DeviceId]},
            Result) when DeviceId =:= undefined;
        DeviceId =:= "" ->
    check_simple_errors(Result#device.errors, "required", "deviceId", "");
postcondition(S, {call, ?MODULE, find_device_by_id, [DeviceId]},
            Result)->
    try
        Device = search_device(DeviceId, S#state.devices),
        Result#device.id == DeviceId
            andalso Result#device.physicalId == Device#deviceType.physicalId
            andalso equals_device_class(Result#device.deviceClass,
                    Result#device.deviceClass)
            andalso Result#device.roomId == Device#deviceType.roomId
            andalso Result#device.description == get_description(
                    Device#deviceType.description)

    catch device_not_found ->
            check_simple_errors(Result#device.errors,
                                "not_found", "deviceId", DeviceId)
    end;

postcondition(_S, {call, ?MODULE, update_device,
            [_DeviceId, PhysicalId, _DeviceClass, _RoomId, _Description]},
            Result) when PhysicalId =:= "" ->
    check_simple_errors(Result#device.errors, "required", "physicalId", "");
postcondition(_S, {call, ?MODULE, update_device,
            [_DeviceId, _PhysicalId, _DeviceClass, RoomId, _Description]},
            Result) when RoomId =:= "" ->
    check_simple_errors(Result#device.errors, "required", "roomId", "");
postcondition(_S, {call, ?MODULE, update_device,
            [DeviceId, _PhysicalId, _DeviceClass, _RoomId, _Description]},
            Result) when DeviceId =:= undefined ->
    check_simple_errors(Result#device.errors, "required", "deviceId", "");
postcondition(S, {call, ?MODULE, update_device,
            [DeviceId, PhysicalId, DeviceClass, RoomId, Description]},
            Result)->
    try
        _Room = search_room(RoomId, S#state.rooms),
        _Device = search_device(DeviceId, S#state.devices),
        DevicePhysicalId =
            search_device_by_physical_id(PhysicalId, S#state.devices),
        case DevicePhysicalId#deviceType.id /= DeviceId of
            true ->
                check_simple_errors(Result#device.errors,
                                    "duplicated", "physicalId", PhysicalId);
            false ->
                %% It was really found, but it is the same case
                throw(device_by_physical_id_not_found)
        end
    catch room_not_found ->
            check_simple_errors(Result#device.errors,
                                "not_found", "roomId", RoomId);
          device_not_found ->
            check_simple_errors(Result#device.errors, "not_found",
                                "deviceId", DeviceId);
          device_by_physical_id_not_found ->
            Result#device.id == DeviceId
                andalso Result#device.physicalId == PhysicalId
                andalso equals_device_class(Result#device.deviceClass, DeviceClass)
                andalso Result#device.roomId == RoomId
                andalso Result#device.description == get_description(Description)
    end;

postcondition(_S, {call, ?MODULE, delete_device, [[undefined]]}, Result) ->
    check_simple_errors(Result#devices.errors, "required", "deviceId", "");
postcondition(S, {call, ?MODULE, delete_device, [DeviceIds]}, Result) ->
    lists:all(
        fun(DeviceId) ->
            try
                _Device = search_device(DeviceId, S#state.devices),
                lists:member(
                    #deviceType { anyAttrs = [], id = DeviceId },
                    Result#devices.device)
            catch device_not_found ->
                lists:any(
                    fun(Error) ->
                        check_simple_error(
                            Error, "not_found", "deviceId", DeviceId)
                    end, (Result#devices.errors)#errors.error)
            end
    end, DeviceIds);

postcondition(_S, C, _R)->
    {unknown_operation, C}.

%%---------------------------------------------------------------
%% next_state
%%---------------------------------------------------------------
next_state(S, _R, {call, ?MODULE, create_room, [RoomId, _Description]})
  when RoomId =:= "" -> S;
next_state(S, _R, {call, ?MODULE, create_room, [RoomId, Description]})->
    try
        _Room = search_room(RoomId, S#state.rooms),
        S
    catch room_not_found ->
            NewRoom = #roomType{
                         anyAttrs = [],
                         roomId = RoomId,
                         description = get_description(Description)
                        },
            S#state {
              rooms = [NewRoom |  S#state.rooms]
             }
    end;

next_state(S, _R, {call, ?MODULE, find_all_rooms, []})->
    S;

next_state(S, _R, {call, ?MODULE, delete_room, [RoomIds]})->
    lists:foldl(
        fun(RoomId, NewState) ->
            try
                Room = search_room(RoomId, NewState#state.rooms),
                NewState#state {
                rooms = lists:delete(Room, NewState#state.rooms),
                devices = lists:filter(
                            fun(Device) ->
                                    Device#deviceType.roomId /= RoomId
                            end, NewState#state.devices)
                }
            catch room_not_found -> NewState
          end
      end, S, RoomIds);

next_state(S, _R, {call, ?MODULE, create_device,
        [PhysicalId, _DeviceClass, RoomId, _Description]})
    when  PhysicalId =:= ""
    ;RoomId =:= "" -> S;
next_state(S, R, {call, ?MODULE, create_device,
        [PhysicalId, DeviceClass, RoomId, Description]})->
    try
        _Room = search_room(RoomId, S#state.rooms),
        _Device = search_device_by_physical_id(PhysicalId, S#state.devices),
        S
    catch room_not_found -> S;
        device_by_physical_id_not_found ->
            DeviceId = {call, erlang, element, [1, R]},
            NewDevice = #deviceType{
                anyAttrs = [],
                id = DeviceId,
                physicalId = PhysicalId,
                deviceClass = get_device_class(DeviceClass),
                roomId = RoomId,
                description = get_description(Description)
            },
            S#state {
                devices = [NewDevice |  S#state.devices]
            }
    end;

next_state(S, _R, {call, ?MODULE, find_devices,
        [_StartIndex, _Count, _SortBy, _Order, _Query]})->
    S;

next_state(S, _R, {call, ?MODULE, find_devices_by_room, [_RoomId]})->
    S;

next_state(S, _R, {call, ?MODULE, find_device_by_id, [_DeviceId]})->
    S;

next_state(S, _R, {call, ?MODULE, update_device,
            [DeviceId, PhysicalId, _DeviceClass, RoomId, _Description]})
  when  PhysicalId =:= ""
        ;RoomId =:= ""
        ;DeviceId =:= undefined -> S;
next_state(S, _R, {call, ?MODULE, update_device,
            [DeviceId, PhysicalId, DeviceClass, RoomId, Description]})->
    try
        _Room = search_room(RoomId, S#state.rooms),
        Device = search_device(DeviceId, S#state.devices),
        try
            DevicePhysicalId =
                search_device_by_physical_id(PhysicalId, S#state.devices),
            case DevicePhysicalId#deviceType.id /= DeviceId of
                true -> S;
                false ->
                    %% It was really found, but it is the same case
                    throw(device_by_physical_id_not_found)
            end
        catch device_by_physical_id_not_found ->
                NewDevice = #deviceType {
                               anyAttrs = [],
                               id = DeviceId,
                               physicalId = PhysicalId,
                               deviceClass = get_device_class(DeviceClass),
                               roomId = RoomId,
                               description = get_description(Description)
                              },
                S#state {
                  devices = [NewDevice
                             | lists:delete(Device, S#state.devices)]}
        end
    catch room_not_found -> S;
          device_not_found -> S
    end;

next_state(S, _R, {call, ?MODULE, delete_device, [DeviceIds]})->
    lists:foldl(
        fun(DeviceId, NewState) ->
            try
                Device = search_device(DeviceId, NewState#state.devices),
                NewState#state {
                    devices = lists:delete(Device, NewState#state.devices)
                }
            catch
                device_not_found -> NewState
            end
      end, S, DeviceIds);

next_state(S, _R, _C)->
    S.

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
check_simple_errors(undefined, _ErrorCode, _ParamName, _ParamValue)->
    false;

check_simple_errors(Errors, ErrorCode, ParamName, ParamValue)->
    Error = lists:nth(1, Errors#errors.error),
    check_simple_error(Error, ErrorCode, ParamName, ParamValue).

check_simple_error(Error, ErrorCode, ParamName, ParamValue)
        when is_integer(ParamValue) ->
    check_simple_error(Error, ErrorCode, ParamName, 
        integer_to_list(ParamValue));

check_simple_error(Error, ErrorCode, ParamName, ParamValue)
        when Error#error.code == ErrorCode,
            length((Error#error.params)#errorParams.param) == 1 ->
    ErrorParam = lists:nth(1, (Error#error.params)#errorParams.param),
    ErrorParam#errorParam.name == ParamName
        andalso ErrorParam#errorParam.value == ParamValue;

check_simple_error(_Error, _ErrorCode, _ParamName, _ParamValue) ->
    false.

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
create_room(RoomId, Description)->
    sut_result(?SUT:create_room(RoomId, Description, undefined)).

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
