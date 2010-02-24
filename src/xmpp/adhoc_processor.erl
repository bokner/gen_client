%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%% Adhoc command processor
%%% @end
%%% Created : 25 Oct 2009 by Boris Okner <boris.okner@gmail.com>

-module(adhoc_processor).

-behaviour(gen_server).

%% API
-export([start_link/2, stop/1, execute/5, generate_sessionid/1, cancel/3, command_items/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("gen_client.hrl").

-record(p_state, {adhoc_module, adhoc_module_params, commands = dict:new(), command_sessions = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the adhoc processor
%% 
%% @spec start_link(Session, Commands) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(AdhocModule, AdhocModuleParams) ->
		gen_server:start_link(?MODULE, [AdhocModule, AdhocModuleParams], []).

stop(Processor) ->
	gen_server:cast(Processor, stop).

execute(Processor, Command, CommandSession, DataForm, ClientState) ->
		Sessionid = case is_binary(CommandSession) of
										true ->
												binary_to_list(CommandSession);
										false ->
												CommandSession
								end,
		gen_server:call(Processor, {execute, Command, Sessionid, DataForm, ClientState}).

cancel(Processor, Command, CommandSession) ->
		Sessionid = case is_binary(CommandSession) of
										true ->
												binary_to_list(CommandSession);
										false ->
												CommandSession
								end,
		gen_server:call(Processor, {cancel, Command, Sessionid}).


generate_sessionid(Command) ->
		{{Year,Month,Day},{Hour,Min,Sec}} =
    calendar:now_to_datetime(erlang:now()),
		binary_to_list(Command) ++
		    lists:flatten(
	io_lib:fwrite(":~4B~2B~2..0BT~2B~2.10.0B~2.10.0B",
                      [Year, Month, Day, Hour, Min, Sec])).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([AdhocModule, AdhocModuleParams]) ->
		{ok, #p_state{
									adhoc_module = AdhocModule,
									adhoc_module_params = AdhocModuleParams
								 }
		}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({execute, Command, CommandSession, DataForm, ClientState}, _From, 
							#p_state{
											 	commands = CommandDict,
												command_sessions = CommandSessions} = State) ->

		{ok, #command{handler = Handler}} = dict:find(Command, CommandDict),

		% Create new process or find the one assigned to handle this command session
		SessionProcess = if CommandSession == new ->
														 case Handler:new_session_process(ClientState) of
																 {ok, S} -> S;
																 none -> none
														 end;
												true ->
														 case dict:find(CommandSession, CommandSessions) of
																 {ok, P} ->
																		 P;
																 Error -> throw(Error)
														 end
										 end,
				io:format("P2~n"),
				R = Handler:execute(SessionProcess, ClientState, DataForm),
				io:format("P3~nExec result:~p~n", [R]),
%% Store session if it's new and the command is stateful (i.e. SessionProcess /= none)
		{NewState, Reply} = if CommandSession == new andalso SessionProcess /= none ->
																NewSessionid = adhoc_processor:generate_sessionid(Command),
																{State#p_state{command_sessions = dict:store(NewSessionid, SessionProcess, State#p_state.command_sessions)},
																 R#command_result{sessionid = NewSessionid, id = Command}};
													 true ->
																{State, R#command_result{sessionid = CommandSession, id = Command}}
												end,

		{reply, Reply, NewState};

handle_call({cancel, Command, Sessionid}, _From, #p_state{commands = CommandDict, command_sessions = CommandSessions} = State) ->
				{ok, #command{handler = Handler}} = dict:find(Command, CommandDict),
															P =  case dict:find(Sessionid, CommandSessions) of

																			 {ok, X} ->
																		 X;
																 _Error -> none
														 end,
		Handler:cancel(P),
		{reply, ok, State#p_state{command_sessions = dict:erase(Sessionid, State#p_state.command_sessions)}};

handle_call(get_commands, _From, #p_state{commands = Commands} = State) ->
		Reply = lists:reverse(dict:fold(fun(_K, V, Acc) -> [V | Acc] end, [], Commands)),
		{reply, Reply, State};

handle_call(_Request, _From, State) ->
		Reply = ok,
		{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
%%TODO: cancel all hanging commands
	{stop, normal, State};

handle_cast(_Msg, State) ->
		{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
		{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
		ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
		{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
commands(Processor) ->
		gen_server:call(Processor, get_commands).


command_items(Processor, JID) ->
		#xmlel{name = 'query', attrs = [#xmlattr{name = node, value = ?NS_ADHOC_b}],
					 children = lists:map(fun(#command{id = Id, name = Name}) ->
										#xmlel{name = "item",
													 attrs = [
																		#xmlattr{name = node, value = Id},
																		#xmlattr{name = name, value = Name},
																		#xmlattr{name = jid, value = list_to_binary(JID)}
																	 ]
													 } end, commands(Processor))
					 }.
