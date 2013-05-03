-module(vaa_sut).

-include("vodkatv.hrl").

-define(BASE_URL, "http://localhost:8082/vodkatv/external/admin/").

%% SUT API
-export([create_room/2, find_all_rooms/0, delete_room/1,
	 create_device/4, find_devices/2, find_devices_by_room/1,
	 find_device_by_id/1, update_device/5, delete_device/1]).

%%---------------------------------------------------------------
%% SUT API
%%---------------------------------------------------------------
create_room(RoomId, Description)->
    PostData = room_post_request(RoomId, Description),
    Url = ?BASE_URL ++ "configuration/CreateRoom.do",
    http_request(post, Url, PostData,
		 fun(Data) ->
			 room_response(Data)
		 end).

find_all_rooms() ->
    Url = ?BASE_URL ++ "configuration/FindAllRooms.do",
    http_request(get, Url,
		 fun(Data) ->
			 rooms_response(Data)
		 end).


delete_room(RoomIds)->
    GetParams = room_ids_get_params(RoomIds),
    Url = add_get_params(?BASE_URL ++ "configuration/DeleteRoom.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 room_ids_response(Data)
		 end).

create_device(PhysicalId, DeviceClass, RoomId, Description) ->
    PostData = device_post_request(PhysicalId, DeviceClass, RoomId,
				   Description),
    Url = ?BASE_URL ++ "configuration/CreateDevice.do",
    http_request(post, Url, PostData,
		 fun(Data) ->
			 device_response(Data)
		 end).

find_devices(StartIndex, Count) ->
    GetParams = iterator_get_params(StartIndex, Count),
    Url = add_get_params(?BASE_URL ++ "configuration/FindDevices.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 devices_response(Data)
		 end).

find_devices_by_room(RoomId) ->
    GetParams = room_ids_get_params([RoomId]),
    Url = add_get_params(?BASE_URL ++ "configuration/FindDevicesByRoom.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 devices_response(Data)
		 end).

find_device_by_id(DeviceId) ->
    GetParams = device_ids_get_params([DeviceId]),
    Url = add_get_params(?BASE_URL ++ "configuration/FindDeviceById.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 device_response(Data)
		 end).

update_device(DeviceId, PhysicalId, DeviceClass, RoomId, Description) ->
    PostData = device_post_request(DeviceId, PhysicalId, DeviceClass, RoomId,
				   Description),
    Url = ?BASE_URL ++ "configuration/UpdateDevice.do",
    http_request(post, Url, PostData,
		 fun(Data) ->
			 device_response(Data)
		 end).

delete_device(DeviceIds)->
    GetParams = device_ids_get_params(DeviceIds),
    Url = add_get_params(?BASE_URL ++ "configuration/DeleteDevice.do",
			 GetParams),
    http_request(get, Url,
		 fun(Data) ->
			 device_ids_response(Data)
		 end).

%%---------------------------------------------------------------
%% Rooms
%%---------------------------------------------------------------
room_post_request(RoomId, Description)->
    vaa_parser_xml:format(
      [{room,
	[{contents,
	  [{roomId, [{text, RoomId}]},
	   {description, [{text, Description}]}
	  ]}
	]}]).

room_ids_get_params(RoomIds) ->
    generate_get_params(lists:map(
      fun(RoomId) ->
	      {"roomId", RoomId}
      end, RoomIds)).

rooms_response(Data)->
    Rooms = get_list(rooms, Data),
    lists:map(
      fun(Room) ->
	      room_response([Room])
      end, Rooms).

room_response(Data)->
    RoomAttrs = get_list(room, Data),
    #room {
       roomId = get_text(roomId, RoomAttrs),
       description = get_text(description, RoomAttrs),
       errors = errors_response(Data)
    }.

room_ids_response(Data)->
    Rooms = get_list(rooms, Data),
    RoomIds = lists:map(fun(Room) ->
		      RoomAttrs = get_list(room, [Room]),
		      get_text(roomId, RoomAttrs)
	      end, Rooms),
    #deletedRoom {
       roomId = RoomIds,
       errors = errors_response(Data)
    }.

%%---------------------------------------------------------------
%% Devices
%%---------------------------------------------------------------
device_post_request(DeviceId, PhysicalId, DeviceClass, RoomId, Description)->
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

device_post_request(PhysicalId, DeviceClass, RoomId, Description)->
    vaa_parser_xml:format(
      [{device,
	[{contents,
	  [{physicalId, [{text, PhysicalId}]},
	   {deviceClass, [{text, DeviceClass}]},
	   {roomId, [{text, RoomId}]},
	   {description, [{text, Description}]}
	  ]}
	]}]).

device_ids_get_params(DeviceIds) ->
    generate_get_params(lists:map(
      fun(DeviceId) ->
	      {"deviceId", DeviceId}
      end, DeviceIds)).

devices_response(Data)->
    Devices = get_list(devices, Data),
    lists:map(
      fun(Device) ->
	      device_response([Device])
      end, Devices).

device_response(Data)->
    DeviceAttrs = get_list(device, Data),
    #device {
	  deviceId = get_text(id, DeviceAttrs),
	  physicalId = get_text(physicalId, DeviceAttrs),
	  deviceClass = get_text(deviceClass, DeviceAttrs),
	  roomId = get_text(roomId, DeviceAttrs),
	  description = get_text(description, DeviceAttrs),
	  errors = errors_response(Data)
	 }.

device_ids_response(Data)->
    Devices = get_list(devices, Data),
    DeviceIds = lists:map(fun(Device) ->
		      DeviceAttrs = get_list(device, [Device]),
		      get_text(id, DeviceAttrs)
	      end, Devices),
    #deletedDevice {
       deviceId = DeviceIds,
       errors = errors_response(Data)
    }.

%%---------------------------------------------------------------
%% Iterator
%%---------------------------------------------------------------
iterator_get_params(StartIndex, Count) ->
    generate_get_params(
      [{"startIndex", StartIndex}, {"count", Count}]).

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
