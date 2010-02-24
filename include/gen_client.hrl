% State
-record(client_state, {session, jid, modules, handlers, terminators}).
% Adhoc command
-record(command, {id, name, handler}).
-record(command_result, {id, result, status, sessionid}).
