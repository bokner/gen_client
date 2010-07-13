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
				 send_sync_packet/3, send_sync_packet/4, 
				 add_handler/2, add_handler/3,
				 remove_handler/2]).

%% 
-export([get_client_state/1, get_client_jid/1]).

-export([behaviour_info/1]).

-compile(export_all).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("gen_client.hrl").

% gen_client behaviour
% Return a list of required functions and their arity
behaviour_info(callbacks) ->
	[
	 {init, 2}, %% Init gen_client implementation; session and argument list
	 {terminate, 1}, %% Terminate gen_client implementation; client state
	 {terminate, 2}, %% Terminate gen_client implementation; module ref and client state	 
	 {handle, 2},		%% Handle stanza; received packet and client state	
	 {handle, 3}  	%% Handle stanza; module ref, received packet and client state
	
	];
behaviour_info(_Other) -> undefined.

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
	HandlerKey = add_handler(Client, Callback),
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
%% The default sync Trigger  is "match response and request by id".
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
	{ok, #client_state{jid = JID,
										 session = Session,
										 args = [JID, Password, Host, Port, Domain],
										 options = Options,
										 handlers = [], %%Keyed list {Key, Handler},
										 terminators = [] %% Keyed list {Key, Terminator}
										}, 0
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
	Reply = try exmpp_session:login(Session)
					catch
						throw:T ->
							T
					end,
	{reply, Reply, State};

handle_call({add_handler, Handler},  _From, State) ->
	{HandlerKey, NewState} = add_handler_internal(Handler, State),
	{reply,  HandlerKey, NewState};

	
handle_call({remove_handler, HandlerKey},  _From, #client_state{handlers = Handlers, terminators = Terminators} = State) ->
	io:format("Removing handler ~p~n", [HandlerKey]),
	%% if a module handler, call terminator function
	case lists:keysearch(HandlerKey, 1, Terminators) of
		{value, {HandlerKey, TerminatorFunc}} ->
			TerminatorFunc(State);
		false ->
			void
	end,	
	{reply,  ok, State#client_state{handlers = lists:keydelete(HandlerKey, 1, Handlers), terminators = lists:keydelete(HandlerKey, 1, Terminators)}};

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
handle_cast({add_handler, Module, Args}, State) ->
	{_HandlerKey, NewState} = add_module(Module, Args, State),
	{noreply, NewState};	

handle_cast({send_packet, Packet}, #client_state{session = Session} = State) ->
	io:format("Outgoing:~p~n", [exmpp_xml:document_to_list(Packet)]),
	spawn(fun() -> exmpp_session:send_packet(Session, Packet) end),
	{noreply, State};	

handle_cast(stop, #client_state{session = Session, terminators = Terminators} = State) ->
	%% Call all module terminators
	lists:foreach(fun({_Key, TerminatorFunc}) -> 
										 spawn(TerminatorFunc(State))
								end, Terminators
							 ), 
	exmpp_session:stop(Session),
	{stop, normal, State};


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
handle_info(timeout, #client_state{options = Options} = State) ->
	NewState = case lists:keysearch(module, 1, Options) of
		{value, {module, Mod, ModArgs}} ->
			{_, NS} = add_module(Mod, ModArgs, State),
			NS;
		false -> State
	end,
	process_flag(trap_exit, true),
	{noreply, NewState};

%% Handle EXIT from Session
handle_info({'EXIT', Session, _Reason}, #client_state{session = Session, options = Options} = State) ->
	io:format("Session closed:~p~n", [_Reason]),
	%% Is reconnect option specified?
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

handle_info({'EXIT', _, _}, State) ->
	{noreply, State};

%% All received packets will trigger handle_info calls (because we have assigned the process to be a controlling process of session).
handle_info(#received_packet{raw_packet = Packet} = Received, 
						#client_state{handlers = Handlers, session = Session} = State) ->
	io:format("Incoming: ~p~n", [exmpp_xml:document_to_list(Packet)]),
	%% Dynamic handlers	
	lists:foreach(
		fun({Key, Handler}) ->
				 spawn(fun() -> R = apply(Handler, [Received, State]), 
												case R of
													stop -> %% Handler has to be disposed
														remove_handler(Session, Key);
													_Other ->
														void
												end
							 end
							) %% spawn and return result of handling along with handler key
		end,				
		Handlers),
	
	{noreply, State};

handle_info(Msg, State) ->
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


%% Module handler
%% We store handler function and terminator function for every handler
add_module(Module, Args, ClientState) ->
								 case gen_client_utils:has_behaviour(Module, gen_client) of 
									 true ->
										 		 Client = self(),	
										 		 spawn_link(fun() -> Module:init(Client , Args) end),	
												 add_handler_internal({fun(Received, State) -> Module:handle(Received, State) end,
																							 fun(State) -> Module:terminate(State) end}, ClientState);		
									 false ->
										 {error, {"module has to have 'gen_client' behaviour", Module}}
								 end.

add_handler(Client, Handler) ->
	gen_server:call(Client, {add_handler, Handler}).

add_handler(Client, Module, Args) ->
	spawn(fun() -> gen_server:cast(Client, {add_handler, Module, Args}) end).	

remove_handler(Client, HandlerKey) ->
	gen_server:call(Client, {remove_handler, HandlerKey}).

generateHandlerKey(Handler, ExistingHandlers) ->
	K = exmpp_utils:random_id("handler."  ++  lists:flatten(io_lib:format("~p::~p", [Handler, self()]))),
	case lists:keymember(K, 1, ExistingHandlers) of
		true ->
			io:format("Key collision: ~p", [K]),
			{A1,A2,A3} = now(),
			random:seed(A1+1, A2+1, A3+1),
			generateHandlerKey(Handler, ExistingHandlers);
		false ->
			K
	end.


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

get_client_state(Client) ->
	gen_server:call(Client, get_client_state).

get_client_jid(Client) ->
	ClientState = get_client_state(Client),
	ClientState#client_state.jid.

add_handler_internal(Handler, #client_state{handlers = Handlers, terminators = Terminators} = State) ->
	HandlerKey = generateHandlerKey(Handler, Handlers),
	io:format("Adding handler ~p~n", [HandlerKey]),
	NewState = case Handler of
							 {HandlerFunc, TerminatorFunc} ->
								 State#client_state{handlers = [{HandlerKey, HandlerFunc}| Handlers], terminators = [{HandlerKey, TerminatorFunc} | Terminators]};
							 _H ->
								 State#client_state{handlers = [{HandlerKey, Handler}| Handlers]}
						 end,
	{HandlerKey, NewState}.