/*  $Header: /baz/users/cvs-root/Doors/server/api_includes.cp,v 1.11 1995/01/26 10:50:11 marilyn Exp $ */
-language([nil]).
-mode(interrupt).

/* Header file for shared constants between the doors API and the server */

/* Is is based on doors.h - if that changes, this must change too! */

/* from doors_types.h */
DOORS_FALSE => 0.
DOORS_TRUE  => 1.

DOORS_FACE_PRESENT     => 1.
DOORS_FACE_CONVERSING  => 2.
DOORS_FACE_ICONIZED    => 3.
DOORS_FACE_DISABLED    => 4.

DOORS_FACE_PRESENT_STRING     => present.
DOORS_FACE_CONVERSING_STRING  => conversing.
DOORS_FACE_ICONIZED_STRING    => iconized.
DOORS_FACE_DISABLED_STRING    => disabled.

DOORS_CONNECTION_NONE           => 0.
DOORS_CONNECTION_TO_OBJECT 	=> 1.
DOORS_CONNECTION_TO_PRESENCE  	=> 2.

DOORS_CONNECTION_NONE_STRING            => "no connection".
DOORS_CONNECTION_TO_OBJECT_STRING 	=> "tour passenger".
DOORS_CONNECTION_TO_PRESENCE_STRING  	=> "connection to presence".

DOORS_CONVERSATION_NONE         => 0.
DOORS_CONVERSATION_CONFERENCE	=> 1.
DOORS_CONVERSATION_LECTURE	=> 2.

DOORS_CONVERSATION_NONE_STRING     	=> "no conversation".
DOORS_CONVERSATION_CONFERENCE_STRING	=> conference.
DOORS_CONVERSATION_LECTURE_STRING	=> lecture.

DOORS_OBJECT_TRANSPARANT_BUS 	=> 1.
DOORS_OBJECT_OPAQUE_BUS  	=> 2.

DOORS_OBJECT_TRANSPARANT_BUS_STRING	=> transparant_bus.
DOORS_OBJECT_OPAQUE_BUS_STRING  	=> opaque_bus.

DOORS_SERVER => 1.
DOORS_PLACE => 2.
DOORS_CONVERSATION => 3.
DOORS_LIST   => 4.

DOORS_SERVER_STRING => server.
DOORS_PLACE_STRING => place.
DOORS_CONVERSATION_STRING => conversation.
DOORS_LIST_STRING => list.

DOORS_ANCHOR_HTML     => 1.
DOORS_ANCHOR_RELATIVE => 2.
DOORS_ANCHOR_PRESENCE => 3.

/* from doors_error_codes.h */

DOORS_OK_ERROR_VAL			=> 0.
DOORS_BAD_ARGUMENT_ERROR_VAL		=> 1.
DOORS_MALLOC_ERROR_VAL			=> 2.
DOORS_INTERNAL_ERROR			=> 3.
DOORS_INVALID_CALL			=> 4.
DOORS_NO_LOCAL_IP_ADDRESS               => 5.
DOORS_SHM_ERROR                         => 6.
DOORS_FORK_ERROR                        => 7.
DOORS_EXEC_ERROR                        => 8.
DOORS_NO_RESOURCES                      => 9.
DOORS_TOO_MANY_CLIENTS                  => 10.
DOORS_WK_PORT_IN_USE                    => 11.
DOORS_ALREADY_INIT                      => 12.
DOORS_NOT_INIT                          => 13.

DOORS_TOKEN_BAD_FILE                  	=> 14.
DOORS_TOKEN_NOT_INITIALIZED          	=> 15.
DOORS_TOKEN_INVALID                   	=> 16.
DOORS_TOKEN_EXPIRED                   	=> 17.
DOORS_TOKEN_IP_MISMATCH			=> 18.

DOORS_COMMUNICATION_CRASHED_ERROR_VAL	=> 19.

DOORS_AUDIO_NOT_ENABLED		     	=> 20.
DOORS_MULTICAST_NOT_SUPPORTED           => 21.

DOORS_OK_ERROR_VAL_STRING			=> ok.
DOORS_BAD_ARGUMENT_ERROR_VAL_STRING		=> "INTERNAL : Bad Argument".
DOORS_MALLOC_ERROR_VAL_STRING               	=> "Out of Memory".
DOORS_INTERNAL_ERROR_STRING		=>
	"Doors Server INTERNAL NonFatal Error : Communication Manager Internal Error
".
DOORS_INVALID_CALL_STRING		=>
	"Doors Server INTERNAL NonFatal Error : Communication Manager Invalid Call Error
".

DOORS_NO_LOCAL_IP_ADDRESS_STRING	=> "No Local IP Address
".
DOORS_SHM_ERROR_STRING                  => "Shared Memory Error
".
DOORS_FORK_ERROR_STRING                 => "Cannot Fork - Kill Some Processes and Rerun doors
".
DOORS_EXEC_ERROR_STRING                 => "Cannot Exec - Verify that your
doors_cm process exists, and/or kill some processes, and rerun doors.
".
DOORS_NO_RESOURCES_STRING               => "Out of Systems Resources - Correct the condition and rerun doors.
".
DOORS_TOO_MANY_CLIENTS_STRING           => "License Error - Too many simultaneous copresences requested. 
Get a valid license and rerun doors.
".
DOORS_WK_PORT_IN_USE_STRING             => "Doors Server: Doors Cannot Execute.
Its well known port is already in use by another invocation of doors,
or by some other process.
".
DOORS_ALREADY_INIT_STRING               => "Internal Error - Server already initialized. 
Please report this error to support@ubique.com
".
DOORS_NOT_INIT_STRING                   => "Internal Error - Server not initialized. 
Please report this error to support@ubique.com
".
DOORS_TOKEN_BAD_FILE_STRING 		 => 
	"Doors Server: Bad License File. 
Fix Your license file and rerun doors. Perhaps your $DoorsRoot is incorrect.
".
DOORS_TOKEN_NOT_INITIALIZED_STRING	 =>
	 "Doors Server INTERNAL Fatal Error: License Not Initialized.
Please report this error to support@ubique.com
".
DOORS_TOKEN_INVALID_STRING		 =>
	 "Doors Server: Invalid License.
Get a valid license and rerun doors.
".

DOORS_TOKEN_EXPIRED_STRING		 =>
	 "Doors Server: Your License Has Expired.
Get an up-to-date license and rerun doors.
".

DOORS_TOKEN_IP_MISMATCH_STRING		 =>
	 "Doors Server: Invalid license.
The IP address in your license does not match your machine's IP address.
Get a valid license for this machine and rerun doors.
".


DOORS_COMMUNICATION_CRASHED_ERROR_VAL_STRING	=> 	
	"Doors Server Communication Manager Failure - Restarting...".

DOORS_AUDIO_NOT_ENABLED_STRING =>
	 "Doors Server INTERNAL NonFatal Error: Audio Not Enabled Error
".
 
DOORS_MULTICAST_NOT_SUPPORTED_STRING =>
	"Doors Server INTERNAL NonFatal Error: MultiCast Not Supported Error
".

DOORS_NO_WARNING         => 0.
DOORS_TOKEN_EXPIRES_SOON => 1.

DOORS_NO_WARNING_STRING        => "No Warning".
DOORS_TOKEN_EXPIRES_SOON_STRING => "License Expires Soon
".

/* From doors_event.h */


PRESENCE_EVENT_CATEGORY_STRING  => people.
PLACE_EVENT_CATEGORY_STRING => place.
TRANSIENT_EVENT_CATEGORY_STRING => transient.
ALERT_EVENT_CATEGORY_STRING => alert.
MESSAGE_EVENT_CATEGORY_STRING => message.
OTHER_EVENT_CATEGORY_STRING => other.
UNKNOWN_EVENT_CATEGORY_STRING => unknown_event_category.
UNKNOWN_EVENT_TYPE_STRING => unknown_event_type.

PRESENCE_EVENT_CATEGORY  => 1. 
PLACE_EVENT_CATEGORY => 2.
TRANSIENT_EVENT_CATEGORY => 3.
ALERT_EVENT_CATEGORY => 4.
MESSAGE_EVENT_CATEGORY => 5.
OTHER_EVENT_CATEGORY => 6.
UNKNOWN_EVENT_CATEGORY => -1.

EVENT_TYPES => [PRESENCE_EVENT_CATEGORY, PLACE_EVENT_CATEGORY, 
		TRANSIENT_EVENT_CATEGORY,
		ALERT_EVENT_CATEGORY, MESSAGE_EVENT_CATEGORY].

PRESENCE_ENTERED_EVENT_STRING => enter.
PRESENCE_LEFT_EVENT_STRING => leave.


PRESENCE_ENTERED_EVENT => 1.
PRESENCE_LEFT_EVENT => 2.

PLACE_INVITE_EVENT_STRING => invite.
PLACE_UPDATE_EVENT_STRING => update.
PLACE_EDIT_STARTED_EVENT_STRING => edit_start.
PLACE_EDIT_FAILED_EVENT_STRING => edit_failed.
PLACE_EDIT_FINISHED_EVENT_STRING => edit_end.
PLACE_POST_EVENT_STRING => news.
PLACE_MAIL_EVENT_STRING => mail.
PLACE_CREATED_OBJECT_EVENT_STRING => created_object.
PLACE_DELETED_OBJECT_EVENT_STRING => deleted_object.
PLACE_CONNECTED_OBJECT_EVENT_STRING => connected_object.
PLACE_DISCONNECTED_OBJECT_EVENT_STRING => disconnected_object.
PLACE_CONNECTED_PRESENCE_EVENT_STRING => connected_presence.
PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT_STRING => 	
				failed_to_connect_to_presence.
PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT_STRING => failed_to_connect_to_object.
PLACE_DISCONNECTED_PRESENCE_EVENT_STRING => disconnected_presence.

PLACE_INVITE_EVENT => 1.
PLACE_UPDATE_EVENT => 2.
PLACE_EDIT_STARTED_EVENT => 3.
PLACE_EDIT_FAILED_EVENT => 4.
PLACE_EDIT_FINISHED_EVENT => 5.
PLACE_POST_EVENT => 6.
PLACE_MAIL_EVENT => 7.
PLACE_CREATED_OBJECT_EVENT => 8.
PLACE_FAILED_TO_CREATE_OBJECT_EVENT => 9.
PLACE_DELETED_OBJECT_EVENT => 10.
PLACE_CONNECTED_PRESENCE_EVENT => 11.
PLACE_CONNECTED_OBJECT_EVENT => 12.
PLACE_DISCONNECTED_PRESENCE_EVENT => 13.
PLACE_DISCONNECTED_OBJECT_EVENT	=> 14.
PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT => 15.
PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT	=> 16.

TRANSIENT_CLICKED_EVENT_STRING => clicked.
TRANSIENT_MOVED_EVENT_STRING => moved.
TRANSIENT_FACE_STATE_EVENT_STRING => face_state.
TRANSIENT_AUDIO_FOCUS_EVENT_STRING => audio_focus.
TRANSIENT_CONVERSATION_TYPE_EVENT_STRING => conversation_type.

TRANSIENT_CLICKED_EVENT => 1.
TRANSIENT_MOVED_EVENT => 2.
TRANSIENT_FACE_STATE_EVENT => 3.
TRANSIENT_AUDIO_FOCUS_EVENT => 4.
TRANSIENT_CONVERSATION_TYPE_EVENT => 5.

ALERT_ALERT_EVENT_STRING => alert.
ALERT_ALERT_EVENT => 1.

MESSAGE_TEXT_EVENT_STRING => message_text.
MESSAGE_TEXT_EVENT => 1.

OTHER_TEXT_EVENT_STRING => other_text.
OTHER_TEXT_EVENT => 1.

DOORS_CREATE_OBJECT_FAILED_ALREADY_DRIVER     => 1.
DOORS_CREATE_OBJECT_FAILED_ALREADY_PASSENGER  => 2.
DOORS_CREATE_OBJECT_FAILED_IN_ONE_TO_ONE      => 3.

DOORS_CREATE_OBJECT_FAILED_ALREADY_DRIVER_STRING    => "already driver".
DOORS_CREATE_OBJECT_FAILED_ALREADY_PASSENGER_STRING  => "already passenger".
DOORS_CREATE_OBJECT_FAILED_IN_ONE_TO_ONE_STRING
					      => "in one to one conversation".

DOORS_CONNECT_FAILED_SERVER_ERROR	=> 1.
DOORS_CONNECT_FAILED_NO_CONNECTEE	=> 2.
DOORS_CONNECT_FAILED_BUSY		=> 3.
DOORS_CONNECT_FAILED_MISMATCH		=> 4.
DOORS_CONNECT_FAILED_NO_ROOM		=> 5.

/* from doors_request.h */

USER_REQUEST_CATEGORY_STRING => user_request_category.
PLACE_REQUEST_CATEGORY_STRING => place_request_category.
SERVER_REQUEST_CATEGORY_STRING => server_request_category.

USER_REQUEST_CATEGORY => 1.
PLACE_REQUEST_CATEGORY => 2.
SERVER_REQUEST_CATEGORY => 3.

USER_DATA_REQUEST_STRING => user_data_request.
USER_FACE_REQUEST_STRING => user_face_request.
USER_ALL_REQUEST_STRING => user_all_request.

USER_DATA_REQUEST => 1.
USER_FACE_REQUEST => 2.
USER_ALL_REQUEST => 3.

PLACE_CONNECT_REQUEST_STRING => place_connect_request.
PLACE_VIEW_EVENTS_REQUEST_STRING => place_view_events_request.
PLACE_CACHE_REQUEST_STRING => place_cache_request.
PLACE_PRESENCES_REQUEST_STRING => place_presences_request.
PLACE_CONNECT_TO_PRESENCE_REQUEST_STRING => place_connect_to_presence_request.
PLACE_CONNECT_TO_OBJECT_REQUEST_STRING => place_connect_to_object_request.	
PLACE_OBJECT_CONNECTIONS_REQUEST_STRING => place_object_connections_request.
PLACE_CREATE_OBJECT_REQUEST_STRING => place_create_object_request.

PLACE_CONNECT_REQUEST => 1.
PLACE_VIEW_EVENTS_REQUEST => 2.
PLACE_CACHE_REQUEST => 3.
PLACE_PRESENCES_REQUEST => 4.
PLACE_CONNECT_TO_PRESENCE_REQUEST => 5.
PLACE_CONNECT_TO_OBJECT_REQUEST	=> 6.
PLACE_OBJECT_CONNECTIONS_REQUEST => 7.
PLACE_CREATE_OBJECT_REQUEST => 8.

SERVER_PRESENCES_COUNT_REQUEST_STRING => server_presences_count_request.
SERVER_PRESENCES_LIST_REQUEST_STRING => server_presences_list_request.

SERVER_PRESENCES_COUNT_REQUEST => 1.
SERVER_PRESENCES_LIST_REQUEST => 2.

/* from doors_response.h */

USER_RESPONSE_CATEGORY => 1.
PLACE_RESPONSE_CATEGORY => 2.
SERVER_RESPONSE_CATEGORY => 3.

USER_RESPONSE_CATEGORY_STRING => user_response_category.
PLACE_RESPONSE_CATEGORY_STRING => place_response_category.
SERVER_RESPONSE_CATEGORY_STRING => server_response_category.

USER_DATA_RESPONSE_STRING => user_data_response.
USER_FACE_RESPONSE_STRING => user_face_response.
USER_ALL_RESPONSE_STRING => user_all_response.

USER_DATA_RESPONSE => 1.
USER_FACE_RESPONSE => 2.
USER_ALL_RESPONSE => 3.

PLACE_SNAPSHOT_RESPONSE_STRING => place_snapshot_response.
PLACE_CACHE_RESPONSE_STRING => place_cache_response.
PLACE_PRESENCES_RESPONSE_STRING => place_presences_response.
PLACE_OBJECT_CONNECTIONS_RESPONSE_STRING => place_object_connections_response.

PLACE_SNAPSHOT_RESPONSE => 1.
PLACE_CACHE_RESPONSE => 2.
PLACE_PRESENCES_RESPONSE => 3.
PLACE_OBJECT_CONNECTIONS_RESPONSE => 4.

SERVER_PRESENCES_COUNT_RESPONSE_STRING => server_presences_count_response.
SERVER_PRESENCES_LIST_RESPONSE_STRING => server_presences_list_reponse.

SERVER_PRESENCES_COUNT_RESPONSE => 1.
SERVER_PRESENCES_LIST_RESPONSE => 2.

/* From doors_error.h */
DOORS_PLACE_CONNECT_ERROR_CATEGORY => 1.
DOORS_INTERFACE_ERROR_CATEGORY => 2.

DOORS_PLACE_CONNECT_ERROR_CATEGORY_STRING => place_connect_error_category.
DOORS_INTERFACE_ERROR_CATEGORY_STRING => interface_error_category.

% place connect error category errors
DOORS_PLACE_CONNECT_SERVER_CLOSED       => 1.
DOORS_PLACE_CONNECT_VERSION_MISMATCH    => 2.
DOORS_PLACE_CONNECT_SERVER_INTERNAL     => 3.
DOORS_PLACE_CONNECT_TOKEN_INVALID       => 4.
DOORS_PLACE_CONNECT_TOKEN_EXPIRED       => 5.
DOORS_PLACE_CONNECT_TOKEN_INTERNAL      => 6.
DOORS_PLACE_CONNECT_PUBLIC_AREA_IS_FULL => 7.
DOORS_PLACE_CONNECT_SERVER_IS_FULL      => 8.
DOORS_PLACE_CONNECT_CLIENT_IS_FULL      => 9.
DOORS_PLACE_CONNECT_PROCESSOR_IS_FULL   => 10.
DOORS_PLACE_CONNECT_SERVER_NOT_THERE    => 11.
DOORS_PLACE_CONNECT_CANNOT_CONNECT      => 12.
DOORS_PLACE_CONNECT_PROTOCOL_ERROR      => 13.
DOORS_PLACE_CONNECT_DUP_PRESENCE        => 14.
DOORS_PLACE_CONNECT_PLACE_IS_FULL       => 15.
DOORS_PLACE_CONNECT_TIMED_OUT           => 16.
DOORS_PLACE_CONNECT_THROWN_OUT          => 17.
DOORS_PLACE_CONNECT_TOO_MANY_PRESENCES  => 18.

DOORS_PLACE_CONNECT_SERVER_CLOSED_STRING      => "server closed".
DOORS_PLACE_CONNECT_VERSION_MISMATCH_STRING    => "version mismatch".
DOORS_PLACE_CONNECT_SERVER_INTERNAL_STRING => "internal server error".
DOORS_PLACE_CONNECT_TOKEN_INVALID_STRING       => "token invalid".
DOORS_PLACE_CONNECT_TOKEN_EXPIRED_STRING       => "token expired".
DOORS_PLACE_CONNECT_TOKEN_INTERNAL_STRING      => "token internal".
DOORS_PLACE_CONNECT_PUBLIC_AREA_IS_FULL_STRING => "public area is full".
DOORS_PLACE_CONNECT_SERVER_IS_FULL_STRING      => "server is full".
DOORS_PLACE_CONNECT_CLIENT_IS_FULL_STRING      => "client is full".
DOORS_PLACE_CONNECT_PROCESSOR_IS_FULL_STRING   => "processor is full".
DOORS_PLACE_CONNECT_SERVER_NOT_THERE_STRING    => "server not there".
DOORS_PLACE_CONNECT_CANNOT_CONNECT_STRING      => "cannot connect".
DOORS_PLACE_CONNECT_PROTOCOL_ERROR_STRING      => "protocol error".
DOORS_PLACE_CONNECT_DUP_PRESENCE_STRING        => "duplicate presence".
DOORS_PLACE_CONNECT_PLACE_IS_FULL_STRING       => "place is full".
DOORS_PLACE_CONNECT_TIMED_OUT_STRING           => "connection timed out".
DOORS_PLACE_CONNECT_THROWN_OUT_STRING          => "presence thrown out".
DOORS_PLACE_CONNECT_TOO_MANY_PRESENCES_STRING  => "too many presences".

%interface error category errors
DISCONNECT_NO_PRESENCEID_ERROR => 1.
VIEW_EVENTS_UNKNOWN_PRESENCE_ERROR => 2.
GOT_EVENT_UNKNOWN_PRESENCE_ERROR => 3.
QUERY_CACHE_UNKNOWN_PRESENCE_ERROR => 4.
QUERY_PLACE_PRESENCES_UNKNOWN_PRESENCE_ERROR => 5.
CREATE_OBJECT_NO_PERSON_ERROR => 6.
CONNECT_TO_PRESENCE_UNKNOWN_PRESENCE_ERROR => 7.
CONNECT_TO_OBJECT_UNKNOWN_PRESENCE_ERROR => 8. QUERY_OBJECT_CONNECTIONS_UNKNOWN_PRESENCE_ERROR => 9.

DISCONNECT_NO_PRESENCEID_ERROR_STRING => disconnect_no_presenceid_error.
VIEW_EVENTS_UNKNOWN_PRESENCE_ERROR_STRING => 
				set_categories_unknown_presence_error.
GOT_EVENT_UNKNOWN_PRESENCE_ERROR_STRING => got_event_unknown_error.
QUERY_CACHE_UNKNOWN_PRESENCE_ERROR_STRING => query_cache_unknown_presence_error.
QUERY_PLACE_PRESENCES_UNKNOWN_PRESENCE_ERROR_STRING => 			
				query_place_presences_unknown_presence_error.
CREATE_OBJECT_NO_PERSON_ERROR_STRING => create_object_no_person_error.
CONNECT_TO_PRESENCE_UNKNOWN_PRESENCE_ERROR_STRING
				 => "connecting to unknown presence".
CONNECT_TO_OBJECT_UNKNOWN_PRESENCE_ERROR_STRING
				=> "connectiong to tour of unknown presence". QUERY_OBJECT_CONNECTIONS_UNKNOWN_PRESENCE_ERROR_STRING
					=> "query about unknown presence".

ERROR_SEVERITY_WARNING => 1.
ERROR_SEVERITY_ERROR => 2.
ERROR_SEVERITY_FATAL => 3.

ERROR_SEVERITY_WARNING_STRING => warning.
ERROR_SEVERITY_ERROR_STRING => error.
ERROR_SEVERITY_FATAL_STRING => fatal.
