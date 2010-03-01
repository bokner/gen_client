%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%% Adhoc command processor
%%% @end
%%% Created : 25 Oct 2009 by Boris Okner <boris.okner@gmail.com>

-module(adhoc_processor).

-behaviour(gen_server).

%% API
-export([start_link/3, stop/1, execute/5, generate_sessionid/1, cancel/4, command_items/2]).
-export([field_from_dataform/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("gen_client.hrl").

-record(p_state, {adhoc_module, adhoc_module_params,  command_sessions = dict:new()}).

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
start_link(Session, AdhocModule, AdhocModuleParams) ->
	gen_server:start_link(?MODULE, [Session, AdhocModule, AdhocModuleParams], []).

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

cancel(Processor, Command, CommandSession, ClientState) ->
	Sessionid = case is_binary(CommandSession) of
								true ->
									binary_to_list(CommandSession);
								false ->
									CommandSession
							end,
	gen_server:call(Processor, {cancel, Command, Sessionid, ClientState}).


generate_sessionid(Command) ->
	exmpp_utils:random_id(exmpp_utils:any_to_list(Command)).



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
init([Session, AdhocModule, AdhocModuleParams]) ->
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
										 adhoc_module = AdhocModule,
										 adhoc_module_params = AdhocModuleParams,										 
										 command_sessions = CommandSessions} = State) ->
	io:format("adhocprocessor:Command:~p~n", [Command]),
	CommandList = AdhocModule:get_adhoc_list(AdhocModuleParams, ClientState),
	{value, #command{handler = Handler}} = lists:keysearch(Command, 2, CommandList),
	
	% Create new process or find the one assigned to handle this command session
	SessionProcess = if CommandSession == new orelse CommandSession == "new" ->
												case Handler:new_session_process(AdhocModuleParams, ClientState) of
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
	R = Handler:execute(SessionProcess, AdhocModuleParams, ClientState, DataForm),
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

handle_call({cancel, Command, Sessionid, AdhocModuleParams, ClientState}, _From, 
						#p_state{
										 adhoc_module = AdhocModule,
										 adhoc_module_params = AdhocModuleParams,
										 command_sessions = CommandSessions} = State) ->
	CommandList = AdhocModule:get_adhoc_list(AdhocModuleParams, ClientState),
	{value, #command{handler = Handler}} = lists:keysearch(Command, 2, CommandList),
	P =  case dict:find(Sessionid, CommandSessions) of
				 
				 {ok, X} ->
					 X;
				 _Error -> none
			 end,
	Handler:cancel(P, AdhocModuleParams, ClientState),
	{reply, ok, State#p_state{command_sessions = dict:erase(Sessionid, State#p_state.command_sessions)}};

handle_call({get_commands, ClientState}, _From, #p_state{adhoc_module = AdhocModule, adhoc_module_params = AdhocModuleParams} = State) ->
	Reply = AdhocModule:get_adhoc_list(AdhocModuleParams, ClientState),
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
commands(Processor, ClientState) ->
	gen_server:call(Processor, {get_commands, ClientState}).


command_items(Processor, #client_state{jid = JID} = ClientState) ->
	Commands = commands(Processor, ClientState), 
	#xmlel{name = 'query', ns = ?NS_DISCO_ITEMS, attrs = [#xmlattr{name = node, value = ?NS_ADHOC_b}],
				 children = lists:map(fun(#command{id = Id, name = Name}) ->
																	 #xmlel{name = "item",
																					attrs = [
																									 #xmlattr{name = node, value = Id},
																									 #xmlattr{name = name, value = Name},
																									 #xmlattr{name = jid, value = exmpp_jid:to_binary(JID)}
																									]
																				 } end, Commands)}.



field_from_dataform(DataForm, FieldName) ->
	Fields = exmpp_xml:get_elements(DataForm, 'field'),
	get_field(Fields, FieldName, undefined).
		

get_field([], _F, Default) ->
		Default;

get_field([F | Rest], FieldName, Default) ->
		Var = exmpp_xml:get_attribute(F, 'var', undefined),
		io:format("field:~p", [Var]),
		case Var =:= exmpp_utils:any_to_binary(FieldName) of
				true ->
						F;
				false ->
						get_field(Rest, FieldName, Default)
		end.
