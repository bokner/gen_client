% State
-record(client_state, {session, jid, handlers, terminators, args = []}).
% Adhoc command
-record(command, {id, name, handler}).
-record(command_result, {id, result, status, sessionid}).
