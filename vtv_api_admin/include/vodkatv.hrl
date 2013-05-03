-record(room, {
	  roomId,
	  description,
	  errors = []
	 }).

-record(deletedRoom, {
	  roomId,
	  errors = []
	 }).

-record(device, {
	  deviceId,
	  physicalId,
	  deviceClass,
	  roomId,
	  description,
	  errors = []
	 }).

-record(deletedDevice, {
	  deviceId,
	  errors = []
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
