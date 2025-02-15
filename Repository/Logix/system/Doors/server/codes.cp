/*  $Header: /baz/users/cvs-root/Doors/server/codes.cp,v 1.9 1995/01/26 10:50:37 marilyn Exp $ */

-language(dfcp).
-include([api_includes, server_includes]).

category_strings_to_codes(Strings, Codes) :-
  Strings ? String |
	category_string_to_code(String, Code),
	Codes ! Code?,
	self;

  Strings = [] |
	Codes = [].


category_string_to_code(String, Code) :-
  String = PRESENCE_EVENT_CATEGORY_STRING |
	Code = PRESENCE_EVENT_CATEGORY;

  String = PLACE_EVENT_CATEGORY_STRING  |
	Code = PLACE_EVENT_CATEGORY;

  String = TRANSIENT_EVENT_CATEGORY_STRING |
	Code = TRANSIENT_EVENT_CATEGORY;

  String = ALERT_EVENT_CATEGORY_STRING |
	Code = ALERT_EVENT_CATEGORY;

  String = MESSAGE_EVENT_CATEGORY_STRING |
	Code = MESSAGE_EVENT_CATEGORY;

  String = OTHER_EVENT_CATEGORY_STRING |
	Code = OTHER_EVENT_CATEGORY;

  otherwise |
	String = _,
	Code = UNKNOWN_EVENT_CATEGORY.


category_codes_to_strings(Codes, Strings) :-
  Codes ? Code |
	category_code_to_string(Code, String),
	Strings ! String?,
	self;

  Codes = [] |
	Strings = [].

category_code_to_string(Code, String) :-
  Code = PRESENCE_EVENT_CATEGORY |
	String = PRESENCE_EVENT_CATEGORY_STRING;

  Code = PLACE_EVENT_CATEGORY  |
	String = PLACE_EVENT_CATEGORY_STRING;

  Code = TRANSIENT_EVENT_CATEGORY |
	String = TRANSIENT_EVENT_CATEGORY_STRING;

  Code = ALERT_EVENT_CATEGORY |
	String = ALERT_EVENT_CATEGORY;

  Code = MESSAGE_EVENT_CATEGORY |
	String = MESSAGE_EVENT_CATEGORY_STRING;

  Code = OTHER_EVENT_CATEGORY |
	String = OTHER_EVENT_CATEGORY_STRING;

  otherwise |
	Code = _,
	String = UNKNOWN_EVENT_CATEGORY_STRING.


event_code_to_string(Code, EventCode, String) :-
  Code = PRESENCE_EVENT_CATEGORY |
	person_code_to_string(EventCode, String);

  Code = PLACE_EVENT_CATEGORY  |
	place_code_to_string(EventCode, String);

  Code = TRANSIENT_EVENT_CATEGORY |
	transient_code_to_string(EventCode, String);

  Code = ALERT_EVENT_CATEGORY |
	alert_code_to_string(EventCode, String);

  Code = MESSAGE_EVENT_CATEGORY |
	message_code_to_string(EventCode, String);

  Code = OTHER_EVENT_CATEGORY |
	other_code_to_string(EventCode, String);

  otherwise |
	Code = _, EventCode = _,
	String = UNKNOWN_EVENT_CATEGORY_STRING.

person_code_to_string(EventCode, String):-
  EventCode = PRESENCE_ENTERED_EVENT |
	String = PRESENCE_ENTERED_EVENT_STRING;

  EventCode = PRESENCE_LEFT_EVENT |
	String = PRESENCE_LEFT_EVENT_STRING;

  otherwise |
	EventCode = _,
	String = UNKNOWN_EVENT_TYPE_STRING.

place_code_to_string(EventCode, String) :-

  EventCode = PLACE_INVITE_EVENT |
	String = PLACE_INVITE_EVENT_STRING;

  EventCode = PLACE_UPDATE_EVENT |
	String = PLACE_UPDATE_EVENT_STRING;

  EventCode = PLACE_EDIT_STARTED_EVENT |
	String = PLACE_EDIT_STARTED_EVENT_STRING;

  EventCode = PLACE_EDIT_FAILED_EVENT |
	String = PLACE_EDIT_FAILED_EVENT_STRING;

  EventCode = PLACE_EDIT_FINISHED_EVENT |
	String = PLACE_EDIT_FINISHED_EVENT_STRING;

  EventCode = PLACE_POST_EVENT |
	String = PLACE_POST_EVENT_STRING;

  EventCode = PLACE_MAIL_EVENT |
	String = PLACE_MAIL_EVENT_STRING;

  EventCode = PLACE_CREATED_OBJECT_EVENT |
	String = PLACE_CREATED_OBJECT_EVENT_STRING;

  EventCode = PLACE_DELETED_OBJECT_EVENT |
	String = PLACE_DELETED_OBJECT_EVENT_STRING;
  
  EventCode = PLACE_CONNECTED_OBJECT_EVENT |
	String = PLACE_CONNECTED_OBJECT_EVENT_STRING;

  EventCode = PLACE_DISCONNECTED_OBJECT_EVENT |
	String = PLACE_DISCONNECTED_OBJECT_EVENT_STRING;

  EventCode = PLACE_CONNECTED_PRESENCE_EVENT |
	String = PLACE_CONNECTED_PRESENCE_EVENT_STRING;

  EventCode = PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT |
	String = PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT_STRING;

  EventCode = PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT |
	String = PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT_STRING;

  EventCode = PLACE_DISCONNECTED_PRESENCE_EVENT |
	String = PLACE_DISCONNECTED_PRESENCE_EVENT_STRING;

  otherwise |
	EventCode = _,
	String = UNKNOWN_EVENT_TYPE_STRING.

transient_code_to_string(EventCode, String) :-
  EventCode = TRANSIENT_CLICKED_EVENT |
	String = TRANSIENT_CLICKED_EVENT_STRING;

  EventCode = TRANSIENT_MOVED_EVENT |
	String = TRANSIENT_MOVED_EVENT_STRING;

  EventCode = TRANSIENT_FACE_STATE_EVENT |
	String = TRANSIENT_FACE_STATE_EVENT_STRING;

  EventCode = TRANSIENT_AUDIO_FOCUS_EVENT |
	String = TRANSIENT_AUDIO_FOCUS_EVENT_STRING;

  EventCode = TRANSIENT_CONVERSATION_TYPE_EVENT |
	String = TRANSIENT_CONVERSATION_TYPE_EVENT_STRING;

  otherwise |
	EventCode = _,
	String = UNKNOWN_EVENT_TYPE_STRING.

alert_code_to_string(EventCode, String) :-
  EventCode = ALERT_ALERT_EVENT |
	String = ALERT_ALERT_EVENT_STRING;

  otherwise |
	EventCode = _,
	String = UNKNOWN_EVENT_TYPE_STRING.

message_code_to_string(EventCode, String) :-

  EventCode = MESSAGE_TEXT_EVENT |
	String = MESSAGE_TEXT_EVENT_STRING;

  otherwise |
	EventCode = _,
	String = UNKNOWN_EVENT_TYPE_STRING.

other_code_to_string(EventCode, String) :-
  EventCode = OTHER_TEXT_EVENT |
	String = OTHER_TEXT_EVENT_STRING;
  
   otherwise |
	EventCode = _,
	String = UNKNOWN_EVENT_TYPE_STRING.

api_error_code_to_string(Code, String, Fatal) :-
  Code = DOORS_OK_ERROR_VAL  |
	Fatal = false,
	String = DOORS_OK_ERROR_VAL_STRING;

  Code = DOORS_BAD_ARGUMENT_ERROR_VAL |
	Fatal = false,
	String = DOORS_BAD_ARGUMENT_ERROR_VAL_STRING;

  Code = DOORS_MALLOC_ERROR_VAL |
	Fatal = abort,
	String = DOORS_MALLOC_ERROR_VAL_STRING;

  Code = DOORS_INTERNAL_ERROR |
	Fatal = false,
	String = DOORS_INTERNAL_ERROR_STRING;

  Code = DOORS_INVALID_CALL |
	Fatal = false,
	String = DOORS_INVALID_CALL_STRING;

  Code = DOORS_NO_LOCAL_IP_ADDRESS |
	Fatal = exit,
	String = DOORS_NO_LOCAL_IP_ADDRESS_STRING;

  Code = DOORS_SHM_ERROR |
	Fatal = exit,
	String = DOORS_SHM_ERROR_STRING;

  Code = DOORS_FORK_ERROR |
	Fatal = exit,
	String = DOORS_FORK_ERROR_STRING;
  
  Code = DOORS_EXEC_ERROR |
	Fatal = exit,
	String = DOORS_EXEC_ERROR_STRING;

  Code = DOORS_NO_RESOURCES |
	Fatal = exit,
	String = DOORS_NO_RESOURCES_STRING;

  Code = DOORS_TOO_MANY_CLIENTS |
	Fatal = exit,
	String = DOORS_TOO_MANY_CLIENTS_STRING;

  Code = DOORS_WK_PORT_IN_USE |
	Fatal = exit,
	String = DOORS_WK_PORT_IN_USE_STRING;

  Code = DOORS_ALREADY_INIT |
	Fatal = exit,
	String = DOORS_ALREADY_INIT_STRING;

  Code = DOORS_NOT_INIT |
	Fatal = exit,
	String = DOORS_NOT_INIT_STRING;

  Code = DOORS_COMMUNICATION_CRASHED_ERROR_VAL |
	Fatal = abort,
	String = DOORS_COMMUNICATION_CRASHED_ERROR_VAL_STRING;

  Code = DOORS_TOKEN_BAD_FILE |
	Fatal = exit,
	String = DOORS_TOKEN_BAD_FILE_STRING;

  Code = DOORS_TOKEN_NOT_INITIALIZED |
	Fatal = exit,
	String = DOORS_TOKEN_NOT_INITIALIZED_STRING;

  Code = DOORS_TOKEN_INVALID |
	Fatal = exit,
	String = DOORS_TOKEN_INVALID_STRING;

  Code = DOORS_TOKEN_EXPIRED |
	Fatal = exit,
	String = DOORS_TOKEN_EXPIRED_STRING;

  Code = DOORS_TOKEN_IP_MISMATCH |
	Fatal = exit,
	String = DOORS_TOKEN_IP_MISMATCH_STRING;

  Code = DOORS_AUDIO_NOT_ENABLED |
	Fatal = false,
	String = DOORS_AUDIO_NOT_ENABLED_STRING;

  Code = DOORS_MULTICAST_NOT_SUPPORTED |
	Fatal = false,
	String = DOORS_MULTICAST_NOT_SUPPORTED_STRING;

  Code = false(Reason) |
	Fatal = exit,
	terms_to_string#acyclic_grounded_terms_to_string(("INTERNAL " : Reason),
			INDENT, LENGTH, String);

  otherwise |
	Fatal = false,
	utils#append_strings(["INTERNAL - Unknown Error Code : " , Code],
			String).


api_warning_code_to_string(Code, String) :-
  Code = DOORS_NO_WARNING |
	String = DOORS_NO_WARNING_STRING;

  Code = DOORS_TOKEN_EXPIRES_SOON |
	String = DOORS_TOKEN_EXPIRES_SOON_STRING;

  otherwise |
	utils#append_strings(["INTERNAL - Unknown Warning Code : ", Code],
						String).

request_category_code_to_string(Code, String) :-
  Code = USER_REQUEST_CATEGORY |
	String = USER_REQUEST_CATEGORY_STRING;

  Code = PLACE_REQUEST_CATEGORY |
	String = PLACE_REQUEST_CATEGORY_STRING;

  Code = SERVER_REQUEST_CATEGORY |
	String = SERVER_REQUEST_CATEGORY_STRING.

request_code_to_string(Category, Type, String) :-
  Category = USER_REQUEST_CATEGORY |
	user_request_code_to_string(Type, String);

  Category = PLACE_REQUEST_CATEGORY |
	place_request_code_to_string(Type, String);

  Category = SERVER_REQUEST_CATEGORY |
	server_request_code_to_string(Type, String).

response_category_code_to_string(Code, String) :-
  Code = USER_RESPONSE_CATEGORY |
	String = USER_RESPONSE_CATEGORY_STRING;

  Code = PLACE_RESPONSE_CATEGORY |
	String = PLACE_RESPONSE_CATEGORY_STRING;

  Code = SERVER_RESPONSE_CATEGORY |
	String = SERVER_RESPONSE_CATEGORY_STRING.

response_code_to_string(Category, Type, String) :-
  Category = USER_RESPONSE_CATEGORY |
	user_response_code_to_string(Type, String);

  Category = PLACE_RESPONSE_CATEGORY |
	place_response_code_to_string(Type, String);

  Category = SERVER_RESPONSE_CATEGORY |
	server_response_code_to_string(Type, String).

user_request_code_to_string(Type, String) :-
  Type = USER_DATA_REQUEST |
	String = USER_DATA_REQUEST_STRING;

  Type = USER_FACE_REQUEST |
	String = USER_FACE_REQUEST_STRING;

  Type = USER_ALL_REQUEST |
	String = USER_ALL_REQUEST_STRING.

place_request_code_to_string(Type, String) :-
  Type = PLACE_CONNECT_REQUEST |
	String = PLACE_CONNECT_REQUEST_STRING;

  Type = PLACE_VIEW_EVENTS_REQUEST |
	String = PLACE_VIEW_EVENTS_REQUEST_STRING;

  Type = PLACE_CACHE_REQUEST |
	String = PLACE_CACHE_REQUEST_STRING;

  Type = PLACE_PRESENCES_REQUEST |
	String = PLACE_PRESENCES_REQUEST_STRING;

  Type = PLACE_CONNECT_TO_PRESENCE_REQUEST |
	String = PLACE_CONNECT_TO_PRESENCE_REQUEST_STRING;

  Type = PLACE_CONNECT_TO_OBJECT_REQUEST |
	String = PLACE_CONNECT_TO_OBJECT_REQUEST_STRING;

  Type = PLACE_OBJECT_CONNECTIONS_REQUEST |
	String = PLACE_OBJECT_CONNECTIONS_REQUEST_STRING;

  Type = PLACE_CREATE_OBJECT_REQUEST |
	String = PLACE_CREATE_OBJECT_REQUEST_STRING.

server_request_code_to_string(Type, String) :-
  Type = SERVER_PRESENCES_COUNT_REQUEST |
	String = SERVER_PRESENCES_COUNT_REQUEST_STRING;

  Type = SERVER_PRESENCES_LIST_REQUEST |
	String = SERVER_PRESENCES_LIST_REQUEST_STRING.

user_response_code_to_string(Type, String) :-
  Type = USER_DATA_RESPONSE |
	String = USER_DATA_RESPONSE_STRING;

  Type = USER_FACE_RESPONSE |
	String = USER_FACE_RESPONSE_STRING;

  Type = USER_ALL_RESPONSE |
	String = USER_ALL_RESPONSE_STRING.

place_response_code_to_string(Type, String) :-
  Type = PLACE_SNAPSHOT_RESPONSE |
	String = PLACE_SNAPSHOT_RESPONSE_STRING;

  Type = PLACE_CACHE_RESPONSE |
	String = PLACE_CACHE_RESPONSE_STRING;

  Type = PLACE_PRESENCES_RESPONSE |
	String = PLACE_PRESENCES_RESPONSE_STRING;

  Type = PLACE_OBJECT_CONNECTIONS_RESPONSE |
	String = PLACE_OBJECT_CONNECTIONS_RESPONSE_STRING.

server_response_code_to_string(Type, String) :-
  Type = SERVER_PRESENCES_COUNT_RESPONSE |
	String = SERVER_PRESENCES_COUNT_RESPONSE_STRING;

  Type = SERVER_PRESENCES_LIST_RESPONSE |
	String = SERVER_PRESENCES_LIST_RESPONSE_STRING.

destination_code_to_string(Code, String) :-
  Code = DOORS_SERVER |
	String = DOORS_SERVER_STRING;

  Code = DOORS_PLACE |
	String = DOORS_PLACE_STRING;

  Code = DOORS_CONVERSATION |
	String = DOORS_CONVERSATION_STRING;

  Code = DOORS_LIST |
	String = DOORS_LIST_STRING.

error_severity_to_string(Code, String) :-
  Code = ERROR_SEVERITY_WARNING |
	String = ERROR_SEVERITY_WARNING_STRING;

  Code = ERROR_SEVERITY_ERROR |
	String = ERROR_SEVERITY_ERROR_STRING;

  Code = ERROR_SEVERITY_FATAL |
	String = ERROR_SEVERITY_FATAL_STRING.

error_category_to_string(Code, String) :-
  Code = DOORS_PLACE_CONNECT_ERROR_CATEGORY |
	String = DOORS_PLACE_CONNECT_ERROR_CATEGORY_STRING;

  Code = DOORS_INTERFACE_ERROR_CATEGORY |
	String = DOORS_INTERFACE_ERROR_CATEGORY_STRING.

error_code_to_string(Category, Code, String) :-
  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_SERVER_CLOSED |
	String = DOORS_PLACE_CONNECT_SERVER_CLOSED_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_VERSION_MISMATCH |
	String = DOORS_PLACE_CONNECT_VERSION_MISMATCH_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_SERVER_INTERNAL |
	String = DOORS_PLACE_CONNECT_SERVER_INTERNAL_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_TOKEN_INVALID |
	String = DOORS_PLACE_CONNECT_TOKEN_INVALID_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_TOKEN_EXPIRED |
	String = DOORS_PLACE_CONNECT_TOKEN_EXPIRED_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_PUBLIC_AREA_IS_FULL |
	String = DOORS_PLACE_CONNECT_PUBLIC_AREA_IS_FULL_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_SERVER_IS_FULL |
	String = DOORS_PLACE_CONNECT_SERVER_IS_FULL_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_CLIENT_IS_FULL |
	String = DOORS_PLACE_CONNECT_CLIENT_IS_FULL_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_PROCESSOR_IS_FULL |
	String = DOORS_PLACE_CONNECT_PROCESSOR_IS_FULL_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_SERVER_NOT_THERE |
	String = DOORS_PLACE_CONNECT_SERVER_NOT_THERE_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_CANNOT_CONNECT |
	String = DOORS_PLACE_CONNECT_CANNOT_CONNECT_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_PROTOCOL_ERROR |
	String = DOORS_PLACE_CONNECT_PROTOCOL_ERROR_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_DUP_PRESENCE |
	String = DOORS_PLACE_CONNECT_DUP_PRESENCE_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_PLACE_IS_FULL |
	String = DOORS_PLACE_CONNECT_PLACE_IS_FULL_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_TIMED_OUT |
	String = DOORS_PLACE_CONNECT_TIMED_OUT_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_THROWN_OUT |
	String = DOORS_PLACE_CONNECT_THROWN_OUT_STRING;

  Category = DOORS_PLACE_CONNECT_ERROR_CATEGORY, 
  Code = DOORS_PLACE_CONNECT_TOO_MANY_PRESENCES |
	String = DOORS_PLACE_CONNECT_TOO_MANY_PRESENCES_STRING;

  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = DISCONNECT_NO_PRESENCEID_ERROR |
	String = DISCONNECT_NO_PRESENCEID_ERROR_STRING;
  
  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = VIEW_EVENTS_UNKNOWN_PRESENCE_ERROR |
	String = VIEW_EVENTS_UNKNOWN_PRESENCE_ERROR_STRING;

  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = GOT_EVENT_UNKNOWN_PRESENCE_ERROR |
	String = GOT_EVENT_UNKNOWN_PRESENCE_ERROR_STRING;

  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = QUERY_CACHE_UNKNOWN_PRESENCE_ERROR |
	String = QUERY_CACHE_UNKNOWN_PRESENCE_ERROR_STRING;

  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = QUERY_PLACE_PRESENCES_UNKNOWN_PRESENCE_ERROR |
	String = QUERY_PLACE_PRESENCES_UNKNOWN_PRESENCE_ERROR_STRING;

  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = CREATE_OBJECT_NO_PERSON_ERROR |
	String = CREATE_OBJECT_NO_PERSON_ERROR_STRING;

  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = CONNECT_TO_PRESENCE_UNKNOWN_PRESENCE_ERROR |
	String = CONNECT_TO_PRESENCE_UNKNOWN_PRESENCE_ERROR_STRING;

  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = CONNECT_TO_OBJECT_UNKNOWN_PRESENCE_ERROR |
	String = CONNECT_TO_OBJECT_UNKNOWN_PRESENCE_ERROR_STRING;

  Category = DOORS_INTERFACE_ERROR_CATEGORY,
  Code = QUERY_OBJECT_CONNECTIONS_UNKNOWN_PRESENCE_ERROR |
	String = QUERY_OBJECT_CONNECTIONS_UNKNOWN_PRESENCE_ERROR_STRING.

