-module(vaa_sut).

-include("vodkatv.hrl").

-define(BASE_URL, "http://localhost:8082/vodkatv/external/admin/").

%% SUT API
-export([create_room/2, find_all_rooms/0, delete_room/1,
	 create_device/4, find_devices/5, find_devices_by_room/1,
	 find_device_by_id/1, update_device/5, delete_device/1]).

%%---------------------------------------------------------------
%% SUT API
%%---------------------------------------------------------------
create_room(RoomId, Description)->
    PostData = create_room_params(RoomId, Description),
    Url = ?BASE_URL ++ "configuration/CreateRoom.do",
    http_request(post, Url, PostData,
     		 fun(Data) ->
     			 create_room_response(Data)
     		 end).

find_all_rooms() ->
    Url = ?BASE_URL ++ "configuration/FindAllRooms.do",
    http_request(get, Url,
		 fun(Data) ->
			 find_all_rooms_response(Data)
		 end).


delete_room(RoomIds)->
    GetParams = delete_room_params(RoomIds),
    Url = add_get_params(?BASE_URL ++ "configuration/DeleteRoom.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 delete_room_response(Data)
		 end).

create_device(PhysicalId, DeviceClass, RoomId, Description) ->
    PostData = create_device_params(PhysicalId, DeviceClass, RoomId,
				    Description),
    Url = ?BASE_URL ++ "configuration/CreateDevice.do",
    http_request(post, Url, PostData,
		 fun(Data) ->
			 create_device_response(Data)
		 end).

find_devices(StartIndex, Count, SortBy, Order, Query) ->
    GetParams = find_devices_params(StartIndex, Count, SortBy, Order, Query),
    Url = add_get_params(?BASE_URL ++ "configuration/FindDevices.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 find_devices_response(Data)
		 end).

find_devices_by_room(RoomId) ->
    GetParams = find_devices_by_room_params(RoomId),
    Url = add_get_params(?BASE_URL ++ "configuration/FindDevicesByRoom.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 find_devices_by_room_response(Data)
		 end).

find_device_by_id(DeviceId) ->
    GetParams = find_device_by_id_params(DeviceId),
    Url = add_get_params(?BASE_URL ++ "configuration/FindDeviceById.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 find_device_by_id_response(Data)
		 end).

update_device(DeviceId, PhysicalId, DeviceClass, RoomId, Description) ->
    PostData = update_device_params(DeviceId, PhysicalId, DeviceClass, RoomId,
				    Description),
    Url = ?BASE_URL ++ "configuration/UpdateDevice.do",
    http_request(post, Url, PostData,
		 fun(Data) ->
			 update_device_response(Data)
		 end).

delete_device(DeviceIds)->
    GetParams = delete_device_params(DeviceIds),
    Url = add_get_params(?BASE_URL ++ "configuration/DeleteDevice.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 delete_device_response(Data)
		 end).

%%---------------------------------------------------------------
%% Rooms
%%---------------------------------------------------------------
create_room_params(RoomId, Description)->
    vaa_parser_xml:format(
      [{room,
	[{contents,
	  [{roomId, [{text, RoomId}]},
	   {description, [{text, Description}]}
	  ]}
	]}]).

create_room_response(Data)->
    RoomAttrs = get_list(room, Data),
    #createRoomResponse {
       roomId = get_text(roomId, RoomAttrs),
       description = get_text(description, RoomAttrs),
       errors = errors_response(Data)
      }.

find_all_rooms_response(Data)->
    Rooms = lists:map(
      fun(Room) ->
	      room([Room])
      end, get_list(rooms, Data)),
    #findAllRoomsResponse {
       rooms = Rooms,
       errors = errors_response(Data)
      }.

delete_room_params(RoomIds) ->
    generate_get_params(lists:map(
			  fun(RoomId) ->
				  {"roomId", RoomId}
			  end, RoomIds)).

delete_room_response(Data)->
    DeletedRooms = lists:map(fun(Room) ->
				     deletedRoom([Room])
			     end, get_list(rooms, Data)),
    #deleteRoomResponse {
       rooms = DeletedRooms,
       errors = errors_response(Data)
      }.

room(Data)->
    RoomAttrs = get_list(room, Data),
    #room {
       roomId = get_text(roomId, RoomAttrs),
       description = get_text(description, RoomAttrs)
      }.

deletedRoom(Data)->
    RoomAttrs = get_list(room, Data),
    #deletedRoom {
       roomId = get_text(roomId, RoomAttrs)
      }.

%%---------------------------------------------------------------
%% Devices
%%---------------------------------------------------------------
create_device_params(PhysicalId, DeviceClass, RoomId, Description)->
    vaa_parser_xml:format(
      [{device,
	[{contents,
	  [{physicalId, [{text, PhysicalId}]},
	   {deviceClass, [{text, DeviceClass}]},
	   {roomId, [{text, RoomId}]},
	   {description, [{text, Description}]}
	  ]}
	]}]).

create_device_response(Data) ->
    DeviceAttrs = get_list(device, Data),
    #createDeviceResponse {
       deviceId = get_text(id, DeviceAttrs),
       physicalId = get_text(physicalId, DeviceAttrs),
       deviceClass = get_text(deviceClass, DeviceAttrs),
       roomId = get_text(roomId, DeviceAttrs),
       description = get_text(description, DeviceAttrs),
       errors = errors_response(Data)
      }.

find_devices_params(StartIndex, Count, SortBy, Order, Query) ->
    generate_get_params(
      [{"startIndex", StartIndex}, {"count", Count},
       {"sortBy", SortBy}, {"order", Order},
       {"query", Query}]).

find_devices_response(Data) ->
    DevicesAttrs = get_attrs(devices, Data),
    Devices = lists:map(
		fun(Device) ->
			device([Device])
		end, get_list(devices, Data)),
    #findDevicesResponse {
       devices = Devices,
       existsMore = get_text(existsMore, DevicesAttrs),
       countTotal = get_text(countTotal, DevicesAttrs),
       errors = errors_response(Data)
      }.

find_devices_by_room_params(RoomId)->
    generate_get_params([{"roomId", RoomId}]).

find_devices_by_room_response(Data) ->
    Devices = lists:map(
		fun(Device) ->
			device([Device])
		end, get_list(devices, Data)),
    #findDevicesByRoomResponse {
       devices = Devices,
       errors = errors_response(Data)
      }.

find_device_by_id_params(DeviceId) ->
    generate_get_params([{"deviceId", DeviceId}]).

find_device_by_id_response(Data) ->
    DeviceAttrs = get_list(device, Data),
    #findDeviceByIdResponse {
       deviceId = get_text(id, DeviceAttrs),
       physicalId = get_text(physicalId, DeviceAttrs),
       deviceClass = get_text(deviceClass, DeviceAttrs),
       roomId = get_text(roomId, DeviceAttrs),
       description = get_text(description, DeviceAttrs),
       errors = errors_response(Data)
      }.

update_device_params(DeviceId, PhysicalId, DeviceClass, RoomId, Description)->
    vaa_parser_xml:format(
      [{device,
	[{contents,
	  [{id, [{text, DeviceId}]},
	   {physicalId, [{text, PhysicalId}]},
	   {deviceClass, [{text, DeviceClass}]},
	   {roomId, [{text, RoomId}]},
	   {description, [{text, Description}]}
	  ]}
	]}]).

update_device_response(Data) ->
    DeviceAttrs = get_list(device, Data),
    #updateDeviceResponse {
       deviceId = get_text(id, DeviceAttrs),
       physicalId = get_text(physicalId, DeviceAttrs),
       deviceClass = get_text(deviceClass, DeviceAttrs),
       roomId = get_text(roomId, DeviceAttrs),
       description = get_text(description, DeviceAttrs),
       errors = errors_response(Data)
      }.

delete_device_params(DeviceIds)->
    generate_get_params(lists:map(
			  fun(DeviceId) ->
				  {"deviceId", DeviceId}
			  end, DeviceIds)).

delete_device_response(Data) ->
    Devices = lists:map(
		fun(Device) ->
			deletedDevice([Device])
		end, get_list(devices, Data)),
    #deleteDeviceResponse {
       devices = Devices,
       errors = errors_response(Data)
      }.

device(Data)->
    DeviceAttrs = get_list(device, Data),
    #device {
       deviceId = get_text(id, DeviceAttrs),
       physicalId = get_text(physicalId, DeviceAttrs),
       deviceClass = get_text(deviceClass, DeviceAttrs),
       roomId = get_text(roomId, DeviceAttrs),
       description = get_text(description, DeviceAttrs)
      }.

deletedDevice(Data)->
    DeviceAttrs = get_list(device, Data),
    #deletedDevice {
       deviceId = get_text(id, DeviceAttrs)
      }.

%%---------------------------------------------------------------
%% Errors
%%---------------------------------------------------------------
errors_response([{_Tag, Data}]) ->
    Contents = proplists:get_value(contents, Data),
    lists:map(
      fun(Error) ->
	      ErrorAttrs = get_list(error, [Error]),
	      Params =
		  lists:map(
		    fun({param, ParamData}) ->
			    ParamAttrs = get_attrs(ParamData),
			    #errorParam {
			       name = get_text(name, ParamAttrs),
			       value = get_text(value, ParamAttrs)
			      }
		    end, get_list(params, ErrorAttrs)),
	      #error {
		 code = get_text(code, ErrorAttrs),
		 description = get_text(description, ErrorAttrs),
		 params = Params
		}
      end, get_list(errors, Contents)).

%%---------------------------------------------------------------
%% Utilities
%%---------------------------------------------------------------
generate_get_params([]) ->
    "";
generate_get_params([{_Name, undefined}]) ->
    "";
generate_get_params([{Name, Value}]) ->
    Name ++ "=" ++ encode_get_param(Value);
generate_get_params([{_Name, undefined}|Params]) ->
    generate_get_params(Params);
generate_get_params([Param | Params]) ->
    generate_get_params([Param]) ++  "&" ++ generate_get_params(Params).

encode_get_param(Param) when is_integer(Param) ->
    encode_get_param(integer_to_list(Param));
encode_get_param(Param) ->
    http_uri:encode(Param).

add_get_params(Url, "") ->
    Url;
add_get_params(Url, Params) ->
    Url ++ "?" ++ Params.

get_attrs(Name, Element)->
    case proplists:get_value(Name, Element) of
	undefined ->
	    [];
	Data ->
	    get_attrs(Data)
    end.

get_attrs(Element) ->
    case proplists:get_value(attrs, Element) of
	undefined ->
	    [];
	Data ->
	    Data
    end.

get_list(Name, Element)->
    case proplists:get_value(Name, Element) of
	undefined ->
	    [];
	Data ->
	    lists:filter(
	      fun(C) ->
		      case C of
			  {errors, _} -> false;
			  _ -> true
		      end
	      end, proplists:get_value(contents, Data))
    end.

get_text(Name, Element) ->
    case proplists:get_value(Name, Element) of
	undefined ->
	    "";
	Data ->
	    case proplists:get_value(text, Data) of
		undefined ->
		    "";
		Text ->
		    Text
	    end
    end.

http_request(Method, Url, FunParse)->
    http_request(Method, Url, "", FunParse).

http_request(Method, Url, Body, FunParse)->
    case do_http_request(Method, Url, Body) of
	{ok, {{_Protocol, 200, _Msg}, _Headers, Response}} ->
	    case vaa_parser_xml:parse(Response) of
		{ok, Result} ->
		    {ok, FunParse(Result)};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{ok, {{_Protocol, Code, Msg}, _Headers, _Response}} ->
	    {error, {Code, Msg}};
	{error, Reason} ->
	    {error, Reason}
    end.

do_http_request(get, Url, _Body)->
    httpc:request(get, {Url, []}, [], []);

do_http_request(post, Url, Body)->
    httpc:request(post, {Url, [], "text/xml", Body}, [], []).
