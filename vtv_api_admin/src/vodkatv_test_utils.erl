-module(vodkatv_test_utils).

-compile(export_all).

-include("vodkatv.hrl").

add(X, Y) -> [X | Y].

search_room(RoomId, Rooms) ->
    vodkatv_test_utils:search(RoomId, Rooms,
                fun(Id, Room) -> Room#roomType.roomId == Id end).

search(_Id, [], _Eq)->
    false;
search(Id, [X | Xs], Eq) ->
    case Eq(Id, X) of
	true ->
	    {value, X};
	false ->
	    search(Id, Xs, Eq)
    end.


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
