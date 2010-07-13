% State
-record(client_state, {session, jid, handlers, terminators, options = [], args = []}).
% Adhoc command
-record(command, {id, name, handler}).
-record(command_result, {id, result, status, sessionid}).
