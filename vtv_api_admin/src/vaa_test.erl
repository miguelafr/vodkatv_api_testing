-module(vaa_test).

-include("vodkatv.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-define(SUT, vaa_sut).

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
    ?SETUP(
       fun setup/0,
       ?FORALL(
          Cmds, commands(?MODULE),
          numtests(
            1000,
            begin
                setup(),
                {H, S, Res} = run_commands(?MODULE, Cmds),
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
       {call, ?MODULE, find_devices, [gen_start_index(), gen_count()]},
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
precondition(_S, {call, ?MODULE, delete_device, [DeviceIds]}) ->
    %% deviceId can not be the empty string "".
    %% This precondition is needed for shrinking.
    not lists:member("", DeviceIds);
%precondition(_S, {call, ?MODULE, update_device,
%		  ["", _PhysicalId, _DeviceClass, _RoomId, _Description]}) ->
    %% deviceId can not be the empty string "".
    %% This precondition is needed for shrinking.
%    false;
precondition(_S, _C)->
    true.

%%---------------------------------------------------------------
%% postcondition:
%%---------------------------------------------------------------
postcondition(_S, {call, ?MODULE, Op, Params}, {error, Reason})->
    {error, {Op, Params}, Reason};

postcondition(_S, {call, ?MODULE, create_room, ["", _Description]},
	      Result)->
    check_simple_errors(Result#createRoomResponse.errors, "required", "roomId", "");
postcondition(S, {call, ?MODULE, create_room, [RoomId, Description]},
	      Result)->
    try
        _Room = search_room(RoomId, S#state.rooms),
        check_simple_errors(Result#createRoomResponse.errors, "duplicated",
                            "roomId", RoomId)
    catch room_not_found ->
            Result#createRoomResponse.roomId == RoomId
                andalso Result#createRoomResponse.description == Description
    end;

postcondition(S, {call, ?MODULE, find_all_rooms, []}, Result)->
    lists:all(
      fun(Room) ->
	      lists:member(Room, S#state.rooms)
      end, Result#findAllRoomsResponse.rooms)
	andalso
	lists:all(
	  fun(Room) ->
		  lists:member(Room, Result#findAllRoomsResponse.rooms)
	  end, S#state.rooms);

postcondition(S, {call, ?MODULE, delete_room, [RoomIds]}, Result)->
    lists:all(
      fun(RoomId) ->
              try
                  _Room = search_room(RoomId, S#state.rooms),
                  lists:member(
		    #deletedRoom {
		       roomId = RoomId
		      },
		    Result#deleteRoomResponse.rooms)
              catch room_not_found ->
		      lists:any(
			fun(Error) ->
				check_simple_error(
				  Error, "not_found", "roomId", RoomId)
			end, Result#deleteRoomResponse.errors)
	      end
      end, RoomIds);

postcondition(_S, {call, ?MODULE, create_device,
		   [PhysicalId, _DeviceClass, _RoomId, _Description]},
 	      {_DeviceId, Result}) when PhysicalId =:= "" ->
    check_simple_errors(Result#createDeviceResponse.errors, "required", "physicalId", "");
postcondition(_S, {call, ?MODULE, create_device,
                   [_PhysicalId, _DeviceClass, RoomId, _Description]},
 	      {_DeviceId, Result}) when RoomId =:= "" ->
    check_simple_errors(Result#createDeviceResponse.errors, "required", "roomId", "");
postcondition(S, {call, ?MODULE, create_device,
		  [PhysicalId, DeviceClass, RoomId, Description]},
 	      {_DeviceId, Result})->
    try
        _Room = search_room(RoomId, S#state.rooms),
        _Device = search_device_by_physical_id(PhysicalId, S#state.devices),
        check_simple_errors(Result#createDeviceResponse.errors, "duplicated",
                            "physicalId", PhysicalId)
    catch room_not_found ->
            check_simple_errors(Result#createDeviceResponse.errors, "not_found",
                                "roomId", RoomId);
          device_by_physical_id_not_found ->
            is_integer(list_to_integer(Result#createDeviceResponse.deviceId))
                andalso Result#createDeviceResponse.physicalId == PhysicalId
                andalso equals_device_class(DeviceClass, Result#createDeviceResponse.deviceClass)
                andalso Result#createDeviceResponse.roomId == RoomId
                andalso Result#createDeviceResponse.description == Description
    end;

postcondition(S, {call, ?MODULE, find_devices, [StartIndex, _Count]},
 	      Result) when StartIndex > length(S#state.devices) ->
    length(Result#findDevicesResponse.devices) == 0;
postcondition(S, {call, ?MODULE, find_devices, [StartIndex, Count]},
 	      Result) ->
    length(Result#findDevicesResponse.devices) ==
	min(Count, length(S#state.devices) - StartIndex + 1)
        andalso
        lists:all(
          fun(Device) ->
                  lists:member(Device, S#state.devices)
          end, Result#findDevicesResponse.devices);

postcondition(S, {call, ?MODULE, find_devices_by_room, [RoomId]}, Result)->
    lists:all(
      fun(Device) ->
	      Device#device.roomId == RoomId
		  andalso lists:member(Device, S#state.devices)
      end, Result#findDevicesByRoomResponse.devices)
	andalso
	lists:all(
	  fun(Device) ->
		  case Device#device.roomId of
		      RoomId ->
			  lists:member(Device, Result#findDevicesByRoomResponse.devices);
		      _ ->
			  true
		  end
	  end, S#state.devices);

postcondition(_S, {call, ?MODULE, find_device_by_id, [DeviceId]},
	      Result) when DeviceId =:= undefined;
			   DeviceId =:= "" ->
    check_simple_errors(Result#findDeviceByIdResponse.errors, "required", "deviceId", "");
postcondition(S, {call, ?MODULE, find_device_by_id, [DeviceId]},
	      Result)->
    try
        Device = search_device(DeviceId, S#state.devices),
        Result#findDeviceByIdResponse.deviceId == DeviceId
            andalso Result#findDeviceByIdResponse.physicalId == Device#device.physicalId
            andalso equals_device_class(Device#device.deviceClass,
                                        Result#findDeviceByIdResponse.deviceClass)
            andalso Result#findDeviceByIdResponse.roomId == Device#device.roomId
            andalso Result#findDeviceByIdResponse.description == Device#device.description
    catch device_not_found ->
            check_simple_errors(Result#findDeviceByIdResponse.errors,
                                "not_found", "deviceId", DeviceId)
    end;

postcondition(_S, {call, ?MODULE, update_device,
		   [_DeviceId, PhysicalId, _DeviceClass, _RoomId, _Description]},
	      Result) when PhysicalId =:= "" ->
    check_simple_errors(Result#updateDeviceResponse.errors, "required", "physicalId", "");
postcondition(_S, {call, ?MODULE, update_device,
		   [_DeviceId, _PhysicalId, _DeviceClass, RoomId, _Description]},
	      Result) when RoomId =:= "" ->
    check_simple_errors(Result#updateDeviceResponse.errors, "required", "roomId", "");
postcondition(S, {call, ?MODULE, update_device,
		  [DeviceId, PhysicalId, DeviceClass, RoomId, Description]},
	      Result)->
    try
        _Room = search_room(RoomId, S#state.rooms),
        case DeviceId of
            undefined ->
                check_simple_errors(Result#updateDeviceResponse.errors, "not_found",
                                    "deviceId", "0");
            _ ->
                _Device = search_device(DeviceId, S#state.devices),
                DevicePhysicalId =
                    search_device_by_physical_id(PhysicalId, S#state.devices),
                case DevicePhysicalId#device.deviceId /= DeviceId of
                    true ->
                        check_simple_errors(Result#updateDeviceResponse.errors,
                                            "duplicated", "physicalId", PhysicalId);
                    false ->
                        %% It was really found, but it is the same case
                        throw(device_by_physical_id_not_found)
                end
        end
    catch room_not_found ->
            check_simple_errors(Result#updateDeviceResponse.errors,
                                "not_found", "roomId", RoomId);
          device_not_found ->
            check_simple_errors(Result#updateDeviceResponse.errors, "not_found",
                                "deviceId", DeviceId);
          device_by_physical_id_not_found ->
            Result#updateDeviceResponse.deviceId == DeviceId
                andalso Result#updateDeviceResponse.physicalId == PhysicalId
                andalso equals_device_class(DeviceClass, Result#updateDeviceResponse.deviceClass)
                andalso Result#updateDeviceResponse.roomId == RoomId
                andalso Result#updateDeviceResponse.description == Description
    end;

postcondition(S, {call, ?MODULE, delete_device, [DeviceIds]}, Result) ->
    lists:all(
      fun(DeviceId) ->
              try
                  _Device = search_device(DeviceId, S#state.devices),
                  lists:member(
		    #deletedDevice { deviceId = DeviceId },
		    Result#deleteDeviceResponse.devices)
              catch device_not_found ->
		      lists:any(
			fun(Error) ->
				check_simple_error(
				  Error, "not_found", "deviceId", DeviceId)
			end, Result#deleteDeviceResponse.errors)
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
            NewRoom = #room{
                         roomId = RoomId,
                         description = Description
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
                                        Device#device.roomId /= RoomId
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
            NewDevice = #device{
                           deviceId = DeviceId,
                           physicalId = PhysicalId,
                           deviceClass = get_device_class(DeviceClass),
                           roomId = RoomId,
                           description = Description
                          },
            S#state {
              devices = [NewDevice |  S#state.devices]
             }
    end;

next_state(S, _R, {call, ?MODULE, find_devices, [_StartIndex, _Count]})->
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
            case DevicePhysicalId#device.deviceId /= DeviceId of
                true -> S;
                false ->
                    %% It was really found, but it is the same case
                    throw(device_by_physical_id_not_found)
            end
        catch device_by_physical_id_not_found ->
                NewDevice = #device{
                               deviceId = DeviceId,
                               physicalId = PhysicalId,
                               deviceClass = get_device_class(DeviceClass),
                               roomId = RoomId,
                               description = Description
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
                    devices = lists:delete(Device, NewState#state.devices)}
              catch device_not_found -> NewState
              end
      end, S, DeviceIds);

next_state(S, _R, _C)->
    S.

%%---------------------------------------------------------------
%% Generators
%%---------------------------------------------------------------
gen_char()->
    elements([choose($a, $z), choose($A, $Z), choose($0, $9)]).

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

gen_room_id(S)->
    gen_new_or_in_use(
      fun() -> gen_string() end,
      S#state.rooms,
      fun(Room) -> Room#room.roomId end).

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
			?LET(X, int(), integer_to_list(X))
		end)
      end,
      S#state.devices,
      fun(Device) -> Device#device.deviceId end).

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
      fun(Device) -> Device#device.physicalId end).

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

gen_start_index()->
    ?SUCHTHAT(I, nat(), I > 0).

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
                fun(Id, Room) -> Room#room.roomId == Id end) of
        {value, Room} -> Room;
        false -> throw(room_not_found)
    end.

%%---------------------------------------------------------------
%% Devices
%%---------------------------------------------------------------
%% @throws device_not_found
-spec search_device(string(), list(#device{})) -> #device{}.
search_device(DeviceId, Devices) ->
    case search(DeviceId, Devices,
                fun(Id, Device) -> Device#device.deviceId == Id end) of
        {value, Device} -> Device;
        false -> throw(device_not_found)
    end.

%% @throws device_by_physical_id_not_found
-spec search_device_by_physical_id(string(), list(#device{})) -> #device{}.
search_device_by_physical_id(PhysicalId, Devices) ->
    case search(PhysicalId, Devices,
                fun(Id, Device) -> Device#device.physicalId == Id end) of
        {value, Device} -> Device;
        false -> throw(device_by_physical_id_not_found)
    end.

%%---------------------------------------------------------------
%% Device class
%%---------------------------------------------------------------
get_device_class(undefined) ->
    "OTHER";
get_device_class(X) ->
    X.

equals_device_class(D1, D2) ->
    get_device_class(D1) == get_device_class(D2).

%%---------------------------------------------------------------
%% Errors
%%---------------------------------------------------------------
check_simple_errors([], _ErrorCode, _ParamName, _ParamValue)->
    false;

check_simple_errors(Errors, ErrorCode, ParamName, ParamValue)->
    Error = lists:nth(1, Errors),
    check_simple_error(Error, ErrorCode, ParamName, ParamValue).

check_simple_error(Error, ErrorCode, ParamName, ParamValue)
  when Error#error.code == ErrorCode,
       length(Error#error.params) == 1 ->

    ErrorParam = lists:nth(1, Error#error.params),
    ErrorParam#errorParam.name == ParamName
        andalso ErrorParam#errorParam.value == ParamValue;
check_simple_error(_Error, _ErrorCode, _ParamName, _ParamValue) ->
    false.

%%---------------------------------------------------------------
%% Utilities
%%---------------------------------------------------------------
setup()->
    error_logger:tty(false),
    inets:start(),
    delete_all_rooms(),
    fun teardown/0.

teardown()->
    inets:stop().

search(_Id, [], _Eq)->
    false;
search(Id, [X | Xs], Eq) ->
    case Eq(Id, X) of
	true ->
	    {value, X};
	false ->
	    search(Id, Xs, Eq)
    end.

sut_result({ok, Result}) ->
    Result;
sut_result({error, Reason}) ->
    {error, Reason}.

delete_all_rooms()->
    case vaa_sut:find_all_rooms() of
	{ok, Response} ->
	    lists:map(
	      fun(Room) ->
		      vaa_sut:delete_room([Room#room.roomId])
	      end, Response#findAllRoomsResponse.rooms);
	Error ->
	    throw(Error)
    end.

%%-----------------------------------------------------------------------------
%% Adapter functions
%%-----------------------------------------------------------------------------
create_room(RoomId, Description)->
    sut_result(?SUT:create_room(RoomId, Description)).

find_all_rooms() ->
    sut_result(?SUT:find_all_rooms()).

delete_room(RoomIds)->
    sut_result(?SUT:delete_room(RoomIds)).

create_device(PhysicalId, DeviceClass, RoomId, Description) ->
    case sut_result(?SUT:create_device(PhysicalId, DeviceClass, RoomId, Description)) of
        {error, Reason} -> {error, Reason};
        Result -> {Result#createDeviceResponse.deviceId, Result}
    end.

find_devices(StartIndex, Count) ->
    sut_result(?SUT:find_devices(StartIndex, Count)).

find_devices_by_room(RoomId) ->
    sut_result(?SUT:find_devices_by_room(RoomId)).

find_device_by_id(DeviceId) ->
    sut_result(?SUT:find_device_by_id(DeviceId)).

update_device(DeviceId, PhysicalId, DeviceClass, RoomId, Description) ->
    sut_result(?SUT:update_device(DeviceId, PhysicalId, DeviceClass, RoomId,
				  Description)).

delete_device(DeviceIds)->
    sut_result(?SUT:delete_device(DeviceIds)).
