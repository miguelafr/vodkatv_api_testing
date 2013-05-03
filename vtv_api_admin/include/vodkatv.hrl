-record(createRoomResponse, {
	  roomId,
	  description,
	  errors = []
	 }).

-record(findAllRoomsResponse, {
	  rooms,
	  errors = []
	 }).

-record(deleteRoomResponse, {
	  rooms,
	  errors = []
	 }).

-record(createDeviceResponse, {
	  deviceId,
	  physicalId,
	  deviceClass,
	  roomId,
	  description,
	  errors = []
	 }).

-record(findDevicesResponse, {
	  devices,
	  errors = []
	 }).

-record(findDevicesByRoomResponse, {
	  devices,
	  errors = []
	 }).

-record(findDeviceByIdResponse, {
	  deviceId,
	  physicalId,
	  deviceClass,
	  roomId,
	  description,
	  errors = []
	 }).

-record(updateDeviceResponse, {
	  deviceId,
	  physicalId,
	  deviceClass,
	  roomId,
	  description,
	  errors = []
	 }).

-record(deleteDeviceResponse, {
	  devices,
	  errors = []
	 }).

-record(room, {
	  roomId,
	  description
	 }).

-record(deletedRoom, {
	  roomId
	 }).

-record(device, {
	  deviceId,
	  physicalId,
	  deviceClass,
	  roomId,
	  description
	 }).

-record(deletedDevice, {
	  deviceId
	 }).

-record(error, {
	  code,
	  params,
	  description
	 }).

-record(errorParam, {
	  name,
	  value
	 }).
