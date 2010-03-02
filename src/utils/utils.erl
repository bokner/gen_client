%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%% Utilities
%%% @end
%%% Created : 27 Dec 2009 by Boris Okner <boris.okner@gmail.com>

-module(utils).

-export([float_round/2, empty_string/1, get_unix_timestamp/1, get_MAC/0, shell_command/2,
				 has_behaviour/2,
				 generate_random_string/1,
				 get_process_state/1,
				 to_jid/1
				 ]).

float_round(N, P) ->
		M = round(math:pow(10, P)),
		round(N*M)/M.

empty_string(Str) ->
		length(string:strip(Str)) == 0.

get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

get_MAC() ->
		macaddr:address().

has_behaviour(Module, Behaviour) ->
	lists:member([Behaviour], proplists:get_all_values(behaviour, Module:module_info(attributes))).

generate_random_string(N) ->
    lists:map(fun (_) -> random:uniform(90)+$\s+1 end, lists:seq(1,N)).


get_process_state(Pid) ->
		{status, Pid, {module, Mod}, [PDict, SysState, Parent, Dbg, Misc]} = 
				sys:get_status(Pid),
		case Mod of
				gen_fsm ->
						[_Pid, _Status, State, _Module, _Timeout] = Misc,
						State;
				gen_server ->
						[_Pid, State, _Module, _Timeout] = Misc,
						State;
				_Other ->
						not_supported
		end.

to_jid(JidStr) when is_list(JidStr) ->
	JidRec = exmpp_jid:parse(JidStr),
	case exmpp_jid:resource(JidRec) of
		undefined ->
			exmpp_jid:full(JidRec, random);
		_Other ->
			JidRec
	end.


shell_command(Dir, Command) ->
		   Port = open_port({spawn, Command},
		     [{cd, Dir}, stream, use_stdio, stderr_to_stdout]),
		D = receive
				{Port, {data, Data}} ->
								Data
				 after 1000 ->
								 "timeout"
		end,
		try
				port_close(Port),
				D
		catch Exc:Reason ->
							 "Exception on command attempt"
		end.

