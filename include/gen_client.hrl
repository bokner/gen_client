% State
-record(client_state, {session, jid, module, module_state, handlers}).
% Adhoc command
-record(command, {id, name, handler}).
-record(command_result, {id, result, status, sessionid}).
