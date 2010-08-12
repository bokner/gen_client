%%%-------------------------------------------------------------------
%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%%
%%% @end
%%% Created : 22 Oct 2009 by Boris Okner <boris.okner@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_client).

-behaviour(gen_server).

% API
-export([start/4, start/5, start/6, start/7, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).
%% API
-export([login/1, send_packet/2, 
				 set_debug/2,
				 send_sync_packet/3, send_sync_packet/4, 
				 add_handler/2, 
				 add_handler/3,
				 remove_handler/2,
				 add_plugin/3,
				 add_plugin/4,
				 remove_plugin/2
				]).

%% 
-export([get_client_state/1, get_client_jid/1]).

%% Helpers
-export([get_xmlel/1, get_xml/1]).	 

-compile(export_all).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

% State
-record(client_state, {session, jid, handlers, options = [], args = []}).

% Defaults
-define(DEFAULT_PRESENCE_MSG, " online.").
-define(LOGGER_KEY, "logger_handler").
-define(SYSTEM_HANDLER, 0).
-define(TEMPORARY_HANDLER, 1).
-define(DEFAULT_PRIORITY, 2).
-define(PLUGIN_KEY(P), atom_to_list(P) ++ "_plugin").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Account, Domain, Resource, Host, Port, Password, Options) ->
	application:start(exmpp),
	start(exmpp_jid:make(Account, Domain, Resource), Host, Port, Password, Options).

start(Account, Domain, Host, Port, Password, Options) ->
	start(Account, Domain, random, Host, Port, Password, Options).


start(Jid, Host, Port, Password, Options) when is_list(Jid) ->
	application:start(exmpp),	
	start(gen_client_utils:to_jid(Jid), Host, Port, Password, Options);

start(Jid, Host, Port, Password, Options) when ?IS_JID(Jid) ->		
	{ok, Client} = gen_server:start_link(?MODULE, [Jid, Password, Host, Port, Options], []),
	{ok, Client}.

start(Jid, Host, Port, Password) when is_list(Jid) ->
	application:start(exmpp),	
	start(gen_client_utils:to_jid(Jid), Host, Port, Password, []);

start(Jid, Host, Port, Password) when ?IS_JID(Jid) ->
	start(Jid, Host, Port, Password, []).

login(Client) ->
	gen_server:call(Client, login).



%% Send packet asynchronously
send_packet(Client, Packet) ->
	gen_server:cast(Client, {send_packet, Packet}).

%%
%% Sends packet and waits Timeout ms for the packet matching the 
%% Trigger to happen. If packet arrives, returns {ok, Packet};
%% otherwise, returns timeout
%% 
send_sync_packet(Client, Packet, Trigger, Timeout) ->
	Pid = self(),
	ResponseId = exmpp_utils:random_id("token"),
	Callback = fun(Response, State) -> 
									case Trigger(Response, State) of 
										true ->
											Pid!{ok, Response, ResponseId},
											stop; %% Signals handler loop to dispose of this handler
										false ->
											ok
									end
						 end,
	HandlerId = generateHandlerId(Callback),
	HandlerKey = add_handler(Client, Callback, HandlerId, ?TEMPORARY_HANDLER),
	%% ...send the packet...
	send_packet(Client, Packet),
	%% ...and wait for response (it should come from controlling process)
	R = receive 
				{ok, Response, ResponseId} -> %% we only interested in a response to our Id, 
					%% ignoring all others;
					{ok, Response} %% return response and let calling module to deal with it.
				after Timeout ->			
					timeout
			end,
	remove_handler(Client, HandlerKey),	
	R.		

%%
%% The default sync Trigger  is "match response and request by id" function.
%% We need to know or create original id first.
%%
send_sync_packet(Client, Packet, Timeout) ->
	Id = exmpp_xml:get_attribute(Packet, id, none),
	{P, PacketId} = case Id of
										none ->
											NewId = exmpp_utils:random_id("session."++ lists:flatten(io_lib:format("~p", [self()]))),
											NewPacket = exmpp_xml:set_attribute(Packet, id, NewId),
											{NewPacket, NewId};
										_Id ->
											{Packet, Id}	
									end,
	%% "Matching id" criteria
	Trigger = fun(#received_packet{id = P_Id}, _State) -> exmpp_utils:any_to_list(P_Id) =:=
																													exmpp_utils:any_to_list(PacketId)
						end,
	%% We have all parts, call generic send_sync now...
	send_sync_packet(Client, P, Trigger, Timeout).

stop(Client) ->
	gen_server:cast(Client, stop).


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
init([JID, Password, Host, Port, Options]) ->
	Domain = exmpp_jid:domain_as_list(JID),
	init([JID, Password, Host, Port, Domain, Options]);

init([JID, Password, Host, Port, Domain, Options] = _Args) ->
	Session = create_session(JID, Password, Host, Port, Domain),
	startup_script(Options),
	{ok, #client_state{jid = JID,
										 session = Session,
										 args = [JID, Password, Host, Port, Domain],
										 options = Options,
										 handlers = orddict:new()	
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
handle_call(login, _From, #client_state{session = Session} = State) ->
	Reply = login_internal(Session),
	{reply, Reply, State};

handle_call({add_handler, Handler, HandlerId, Priority},  _From, #client_state{handlers = Handlers} = State) ->
	HandlerKey = {Priority, HandlerId},
	NewHandlers = orddict:store(HandlerKey, Handler, Handlers),
	{reply, HandlerKey, State#client_state{handlers = NewHandlers}};

handle_call({add_plugin, Plugin, Args, Priority}, _From, #client_state{handlers = Handlers} = State) ->
	{ok, PluginRef} = Plugin:init(Args),
	Handler = fun(Response, Client) -> Plugin:handle(Response, Client, PluginRef) end,
	HandlerKey = {Priority, ?PLUGIN_KEY(Plugin)},
	Terminator = fun() -> Plugin:terminate(PluginRef) end,
	NewHandlers = orddict:store(HandlerKey, {Handler, Terminator}, Handlers),
	{reply, HandlerKey, State#client_state{handlers = NewHandlers}};

handle_call({remove_handler, HandlerKey}, _From, #client_state{handlers = Handlers} = State) ->
	io:format("Removing handler ~p~n", [HandlerKey]),
	NewHandlers = orddict:erase(HandlerKey, Handlers),
	{reply, ok, State#client_state{handlers = NewHandlers}};

handle_call(get_client_state, _From, State) ->
	{reply, State, State};

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
handle_cast({send_packet, Packet}, #client_state{session = Session} = State) ->
	io:format("Outgoing:~p~n", [exmpp_xml:document_to_list(Packet)]),
	spawn(fun() -> exmpp_session:send_packet(Session, Packet) end),
	{noreply, State};	

handle_cast(stop, #client_state{session = Session, handlers = Handlers} = State) ->
	{stop, normal, State};

handle_cast({set_debug, true}, #client_state{handlers = Handlers} = State) ->
	HandlerKey = {?SYSTEM_HANDLER, ?LOGGER_KEY},
	Handler = fun(Received, _State) -> log_packet(Received) end,
	NewHandlers = orddict:store(HandlerKey, {Handler, fun() -> void end}, Handlers),
	{noreply, State#client_state{handlers = NewHandlers}};


handle_cast({set_debug, false}, #client_state{handlers = Handlers} = State) ->
	NewHandlers = orddict:erase({?SYSTEM_HANDLER, ?LOGGER_KEY}, Handlers),
	{noreply, State#client_state{handlers = NewHandlers}};

handle_cast(_Request, State) ->
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

%% This is called immediately after init() is finished.
handle_info(timeout, State) ->
	process_flag(trap_exit, true),
	{noreply, State};

%% Handle EXIT from Session
handle_info({'EXIT', Session, _Reason}, #client_state{session = Session, options = Options} = State) ->
	io:format("Session closed:~p~n", [_Reason]),
	%% Is "reconnect" option specified?
	case lists:keysearch(reconnect, 1, Options) of
		{value, {reconnect, Timeout}} ->
			catch(exmpp_session:stop(Session)),
			timer:send_after(Timeout, reconnect),
			{noreply, State};
		false ->
			{stop, normal, State}
	end;

handle_info({'EXIT', Process, Reason}, State) ->
	io:format("Process ~p ended with ~p.~n", [Process, Reason]),
	{noreply, State};


handle_info(reconnect, #client_state{options = Options, args = Args} = State) ->
	io:format("Reconnecting...~n"),
	NewSession = erlang:apply(fun create_session/5, Args),
	case lists:keysearch(module, 1, Options) of
		{value, {module, Mod, ModArgs}} ->
			Client = self(),
			spawn(fun() -> Mod:init(Client, ModArgs) end);
		false ->
			ok
	end,
	{noreply, State#client_state{session = NewSession}};


%% All received packets will trigger handle_info calls (because we have assigned the process to be a controlling process of session).
handle_info(Received, 
						#client_state{handlers = Handlers} = State) ->
	
	Client = self(),
	spawn(fun() ->
						 try
							 orddict:filter(
								 fun(_Key, {Handler, _Terminator}) ->
											case apply(Handler, [Received, Client]) of
												stop -> throw(stop);
												_ -> false
											end
								 end, Handlers)
						 catch
							 throw:stop -> ok;
							 _Class:Reason ->
								 log_error(Reason)
						 end end),
	{noreply, State};

handle_info(Msg, _State) ->
	io:format("Unexpected message:~p~n", [Msg]).



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
terminate(Reason, #client_state{jid = Jid, session = Session, handlers = Handlers} = _State) ->
	io:format("Terminating ~p with reason ~p~n", [Jid, Reason]),
	exmpp_session:stop(Session),
	orddict:filter(fun(_Key, {_Handler, Terminator}) -> erlang:apply(Terminator, []), false end, Handlers),	
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
%%% API functions
%%%===================================================================
add_handler(Client, Handler) ->
	HandlerId = generateHandlerId(Handler),
	Priority = ?DEFAULT_PRIORITY,
	add_handler(Client, Handler, HandlerId, Priority).

add_handler(Client, Handler, Priority) ->
	HandlerId = generateHandlerId(Handler),
	add_handler(Client, Handler, HandlerId, Priority).
	
add_handler(Client, Handler, HandlerId, Priority) when is_tuple(Handler) ->
	gen_server:call(Client, {add_handler, Handler, HandlerId, Priority});

add_handler(Client, HandlerFunc, HandlerId, Priority) when is_function(HandlerFunc)->
	add_handler(Client, {HandlerFunc, fun() -> void end}, HandlerId, Priority).

remove_handler(Client, HandlerKey) ->
	gen_server:call(Client, {remove_handler, HandlerKey}).

add_plugin(Client, Plugin, Args) when is_atom(Plugin) ->
	add_plugin(Client, Plugin, Args, ?DEFAULT_PRIORITY).

add_plugin(Client, Plugin, Args, Priority) when is_atom(Plugin) ->
	gen_server:call(Client, {add_plugin, Plugin, Args, Priority}).

remove_plugin(Client, PluginKey) ->
	%% Remove plugin
	remove_handler(Client, PluginKey).

get_client_state(Client) ->
	gen_server:call(Client, get_client_state).

get_client_jid(Client) ->
	ClientState = get_client_state(Client),
	ClientState#client_state.jid.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_session(JID, Password, Host, Port, Domain) ->
	%% Create a new session with basic (digest) authentication:
	Session = exmpp_session:start(),
	exmpp_session:auth_basic_digest(Session, JID, Password),
	%% Connect in standard TCP:
	_StreamId = exmpp_session:connect_TCP(Session, Host, Port, Domain),
	exmpp_session:set_controlling_process(Session, self()),
	%% Link with exmpp session so we trap session going down...
	erlang:link(Session),
	Session.

login_internal(Session) ->
	try exmpp_session:login(Session)
	catch
		throw:T ->
			T
	end.

startup_script(Options) ->
	Client = self(),
	VoidFun = fun() -> void end,
	IsDebug = proplists:get_value(debug, Options, false),
	DebugFun = fun() -> gen_client:set_debug(Client, IsDebug) end,
	IsLogin = proplists:get_value(log_in, Options, true),
	%% The login gets called by default
	LoginFun = case IsLogin of
							 true ->
								 fun() -> gen_client:login(Client) end;
							 false ->
								 VoidFun
						 end,
	IsPresence = proplists:get_value(presence, Options, true),
	PFun = 
		fun(Msg) -> gen_client:send_packet(Client, stanza:available(Msg)) end,
	PresenceFun = case IsPresence of
									false ->
										VoidFun;
									{true, Msg} ->
										fun() -> PFun(Msg) end;
									true ->
										fun() -> PFun(?DEFAULT_PRESENCE_MSG) end
								end,
	
	Script = proplists:get_value(script, Options, undefined),
	ScriptFun = case Script of
								undefined ->
									VoidFun;
								_ ->
									%% Script is a function that takes a single Client parameter
									fun() -> Script(Client) end
							end,	
	spawn_link(fun() -> DebugFun(), LoginFun(), PresenceFun(), ScriptFun() end).


%% Add/remove logging
set_debug(Client, TrueFalse) ->
	gen_server:cast(Client, {set_debug, TrueFalse}).

log_packet(#received_packet{raw_packet = Packet} = _Received) -> 
	io:format("Incoming:~p~n", [exmpp_xml:document_to_list(Packet)]).

generateHandlerId(Handler) ->
	exmpp_utils:random_id("handler."  ++  lists:flatten(io_lib:format("~p::~p", [Handler, self()]))).

%% Helpers
%%
%% Get xmlel (as defined by exmpp) from the stanza
get_xmlel(#received_packet{raw_packet = RawPacket}) ->
	RawPacket.

get_xml(Packet) ->
	exmpp_xml:document_to_list(get_xmlel(Packet)).

log_error(Reason) ->
	io:format("Error: ~p~n", [Reason]).