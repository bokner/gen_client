%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%% Utilities
%%% @end
%%% Created : 27 Dec 2009 by Boris Okner <boris.okner@gmail.com>

-module(gen_client_utils).

-export([float_round/2, 
				 num_to_str/1,
				 empty_string/1, get_unix_timestamp/1, get_MAC/0, shell_command/2,
				 has_behaviour/2,
				 generate_random_string/1,
				 get_process_state/1,
				 to_jid/1,
     get_xml/1,
     get_xmlel/1,        
				 any_to_binary/1,
				 any_to_str/1,
					eval_script/2,
				 create_timer/0, start_timer/3, reset_timer/1, reset_timer/2, reset_timer/3, cancel_timer/1, stop_timer/1,
				 hex/1
				 ]).

%%
%% Include files
%%
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").



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
					case Misc of
						[_Pid, _Status, State, _Module, _Timeout] ->
						State;
						[_Header, _Status, Data] ->
						{data, [{"StateData", State}]} = Data,
						 State;
						_Other ->
							not_known
					end;
				gen_server ->
						[_Pid, State, _Module, _Timeout] = Misc,
						State;
				_Other ->
						not_supported
		end.

to_jid(JID) when ?IS_JID(JID) ->
  JID;

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
		     [{cd, Dir}, stream, use_stdio, stderr_to_stdout, binary]),
		D = receive
				{Port, {data, Data}} ->
								Data
				 after 1000 ->
								 <<"timeout">>
		end,
		try
				port_close(Port),
				D
		catch _Exc:_Reason ->
							 <<"Exception on command attempt">>
		end.

any_to_binary(Value) ->
	case is_binary(Value) of
		true ->
			Value;
		false ->
			exmpp_utils:any_to_binary(Value)
	end.

num_to_str(Number) ->
		lists:flatten(io_lib:format("~p", [Number])).


any_to_str(Term) ->
			lists:flatten(io_lib:format("~p", [Term])).

%% Evaluates script
eval_script(Code, Args) ->
  {ok, Scanned, _} = erl_scan:string(Code),
  {ok, Parsed} = erl_parse:parse_exprs(Scanned),
  Bindings = lists:foldl(fun ({Key, Val}, BindingsAccumulator) ->
    erl_eval:add_binding(Key, Val, BindingsAccumulator)
  end, erl_eval:new_bindings(), Args),
  {value, Result, _} = erl_eval:exprs(Parsed, Bindings),
  Result.




get_dataform(#received_packet{raw_packet = IQ}) ->
		#iq{payload = Payload} = exmpp_iq:xmlel_to_iq(IQ),
		exmpp_xml:get_element(Payload, 'x').


% Resettable timer - the timer process with reset() function
% The idea is to use a single "renewable timer" that accompanies
% a process as opposed to creating new timer every time the previous
% timer expires.
% For instance, in case of using standard timers the heartbeat logic
% would cancel "timeout" timer and create a new one
% every time the heartbeat would occur (see driverFsm).
% This implies that the current timer has to be stored in some process' state.
% whereas reusable timer could be passed to a heartbeat's
% callback function once and had "reset" itself in order to restart countdown.

create_timer() ->
	%% Creates dummy timer process;
	%% This is for obtaining reference in order to control it later (see start_timer, reset_timer, cancel_timer)
	spawn(fun() -> resettable_timer_func(infinity, fun() -> throw({error, sleeping_timer_active}) end) end).

start_timer(Timer, Timeout, Fun) ->
	io:format("Start timer at:~p with timeout = ~p~n", [calendar:local_time(), Timeout] ),	
	reset_timer(Timer, Timeout, Fun).

resettable_timer_func(Timeout, Fun) ->
	receive
		reset ->
			resettable_timer_func(Timeout, Fun);
		{reset, NewTimeout} ->
			resettable_timer_func(NewTimeout, Fun);
		{reset, NewTimeout, NewFun} ->
			resettable_timer_func(NewTimeout, NewFun);
		cancel ->
			void
		after Timeout ->
			Fun(),
			resettable_timer_func(Timeout, Fun) % start new time interval

	end.

reset_timer(Timer) ->
	Timer!reset, ok.

reset_timer(Timer, NewTimeout) ->
	Timer!{reset, NewTimeout}, ok.

reset_timer(Timer, NewTimeout, NewFun) ->
	Timer!{reset, NewTimeout, NewFun}, ok.

cancel_timer(Timer) ->
	Timer!cancel, ok.

stop_timer(Timer) ->
	Timer!{reset, infinity}.


%% Hex
digit_to_xchar(D) when (D >= 0) and (D < 10) ->
	D + 48;
digit_to_xchar(D) ->
	D + 87.


hex(S) ->
	hex(S, []).



hex([], Res) ->
	lists:reverse(Res);
hex([N | Ns], Res) ->
	hex(Ns, [digit_to_xchar(N rem 16),
					 digit_to_xchar(N div 16) | Res]).

%% Get xmlel (as defined by exmpp) from the stanza
get_xmlel(#received_packet{raw_packet = RawPacket}) ->
	RawPacket.

get_xml(Packet) ->
	exmpp_xml:document_to_list(get_xmlel(Packet)).
