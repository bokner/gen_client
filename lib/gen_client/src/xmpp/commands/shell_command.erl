%%%-------------------------------------------------------------------
%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%% "Shell" adhoc command implementation.
%%% Allows executing shell commands on a client side.
%%% @end
%%% Created : 27 Oct 2009 by Boris Okner <boris.okner@gmail.com>
%%%-------------------------------------------------------------------
-module(shell_command).

-behaviour(gen_fsm).
-behaviour(gen_command).

-export([new_session_process/2, execute/4, cancel/1]).

-compile(export_all).

-include("ad_hoc.hrl").

-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").


%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
				 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(p_state, {dir = "."}).

%%% gen_command
new_session_process(_AdhocModuleParams, _Requester) ->
		start_link().

execute(SessionProcess, _AdhocModuleParams, DataForm, _Requester) ->
		gen_fsm:sync_send_event(SessionProcess, {execute, DataForm}).

cancel(SessionProcess) ->
		gen_fsm:send_all_state_event(SessionProcess, cancel).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
		gen_fsm:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
		{ok, Dir} = file:get_cwd(),
		{ok, entry, #p_state{dir = Dir}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, State) ->
		{next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
entry({execute, _}, _From, State) ->
		% Display entry form
		Reply = entry_form(State#p_state.dir),
		{reply, Reply, wait_for_command, State}.

wait_for_command({execute, DataForm}, _From, State) ->
		% Execute shell command and display result
		{Dir, Command} = from_dataform(DataForm),
		Reply = shell_command(Dir, Command),
		{reply, Reply, entry, State#p_state{dir = Dir}}.

state_name(_Event, _From, State) ->
		Reply = ok,
		{reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(cancel, _StateName, State) ->
		{stop, normal, State};

handle_event(_Event, StateName, State) ->
		{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
		Reply = ok,
		{reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
		{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
		ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
		{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
entry_form(Dir) ->
		EntryForm = #xmlel{name = 'x', ns = 'jabber:x:data', attrs = [#xmlattr{name = type, value = <<"form">>}],
											 children = [
																	 #xmlel{name = 'title', children = [#xmlcdata{cdata = <<"Shell commands">>}]},
																	 #xmlel{name = 'instructions', children = [#xmlcdata{cdata = <<"Run shell command ">>}]},
																	 % Current directory
																	 #xmlel{name = 'field', attrs = [
																																	 #xmlattr{name = var, value = <<"dir">>},
																																	 #xmlattr{name = label, value = <<"Directory">>},
																																	 #xmlattr{name = type, value = <<"text-single">>}
																																	],
																					children = [#xmlel{name = 'value', children = [#xmlcdata{cdata = Dir}]}]
																				 },
																	 % Shell command
																	 #xmlel{name = 'field', attrs = [
																																	 #xmlattr{name = var, value = <<"command">>},
																																	 #xmlattr{name = label, value = <<"Type command here:">>},
																																	 #xmlattr{name = type, value = <<"text-single">>}
																																	],
																					children = [#xmlel{name = 'value', children = [#xmlcdata{cdata = ""}]}]
																				 }


																	]
											},
				#command_result{result = EntryForm, status = executing}.


shell_command(Dir_Binary, Command_Binary) ->
	 Dir = binary_to_list(Dir_Binary),
		Command = binary_to_list(Command_Binary),

   Result = case empty_string(Command) of
			 true ->
					 "Empty command line";
			 false ->
					 case empty_string(Dir) of
							 true ->
									 gen_client_utils:shell_command(".", Command);
							 false ->
									 gen_client_utils:shell_command(Dir, Command)
					 end
	 end,

	 output_form(Result).




from_dataform(DataForm) ->
		Fields = exmpp_xml:get_elements(DataForm, 'field'),
		[Dir, Command] = lists:map(fun(F)
																 -> exmpp_xml:get_cdata(exmpp_xml:get_element(F, 'value'))
																			 end, Fields),
		{Dir, Command}.

output_form(Output) ->
		OutputLines =
				lists:map(fun(L) ->
													#xmlel{name = 'field',
																 attrs = [
																					 #xmlattr{name = type, value = <<"fixed">>}
																					], 
																 children = [#xmlel{name = 'value',
																										children = [
																										#xmlcdata{cdata = list_to_binary(L)}]}
																						]}
									end,
									%% Not very eficient, just a quick fix (better parsing binary)...
									string:tokens(binary_to_list(Output), "\n")),

				OutputForm = #xmlel{name = 'x', ns = 'jabber:x:data', attrs = [#xmlattr{name = type, value = <<"result">>}],
											 children = [
																	 #xmlel{name = 'title', children = [#xmlcdata{cdata = <<"Shell commands">>}]} | OutputLines
																]
											},
				#command_result{result = OutputForm, status = executing}.


empty_string(Str) ->
		length(string:strip(Str)) == 0.
