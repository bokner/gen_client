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
-export([start/6, start/7, start/8, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).
%% API
-export([login/1, send_packet/2, 
				 send_sync_packet/3, send_sync_packet/4, 
				 add_handler/3, add_handler/4, 
					remove_handler/2]).

-export([behaviour_info/1]).


-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("gen_client.hrl").

-define(PUBSUB(NS, Children), (
  #xmlel{ns = NS, name = 'pubsub', children = Children}
)).


% gen_client behaviour
% Return a list of required functions and their arity
behaviour_info(callbacks) ->
		[
		 {run, 2},
		 {terminate, 1},
		 {handle_iq, 5},
		 {handle_message, 5},
		 {handle_presence, 5},
		 {handle_feed, 2}
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

start(Account, Domain, Resource, Host, Port, Password, Module, Args) ->
		application:start(exmpp),
		start(exmpp_jid:make(Account, Domain, Resource), Host, Port, Password, Module, Args).

start(Account, Domain, Host, Port, Password, Module, Args) ->
		start(Account, Domain, random, Host, Port, Password, Module, Args).


start(Jid, Host, Port, Password, Module, Args) when is_list(Jid) ->
	application:start(exmpp),	
	start(exmpp_jid:make(Jid), Host, Port, Password, Module, Args);

start(Jid, Host, Port, Password, Module, Args) when ?IS_JID(Jid) ->		
		Session = exmpp_session:start(),
		{ok, Client} = gen_server:start_link(?MODULE, [Jid, Password, Host, Port, Module, Session], []),
		exmpp_session:set_controlling_process(Session, Client),
		 % Run initial script
		 gen_server:cast(Client, {run, Args}),
		{ok, Session, Client}.

login(Session) ->
		try exmpp_session:login(Session)
		catch
				throw:T ->
						T
		end.


%% Send packet asynchronously
send_packet(Session, Packet) ->
		io:format("Outgoing:~p~n", [exmpp_xml:document_to_list(Packet)]),
		exmpp_session:send_packet(Session, Packet).

%%
%% Sends packet and waits Timeout ms for the packet matching the 
%% Criteria to happen. If packet arrives, returns {ok, Packet};
%% otherwise, returns timeout
%% 
send_sync_packet(Session, Packet, Criteria, Timeout) ->
		Pid = self(),
		ResponseId = utils:generate_random_string(12),
		Callback = fun(Response) -> Pid!{ok, Response, ResponseId} end,
		Handler = add_handler(Session, Criteria, Callback, true),
		%% ...send the packet...
		exmpp_session:send_packet(Session, Packet),
		%% ...and wait for response (it should come from controlling process)
		R = receive 
			{ok, Response, ResponseId} -> %% we only interested in a response to our Id, 
																					%% ignoring all others;
				{ok, Response} %% return response and let calling module to deal with it.
		after Timeout ->
				remove_handler(Session, Handler),				
				timeout
		end,
		R.		


%%
%% The default sync Criteria  is "match response and request by id".
%% We need to know or create original id first.
%%
send_sync_packet(Session, Packet, Timeout) ->
		Id = exmpp_xml:get_attribute(Packet, id, none),
    {P, PacketId} = case Id of
			none ->
				NewId = exmpp_utils:random_id("session"),
	    	NewPacket = exmpp_xml:set_attribute(Packet, id, NewId),
				{NewPacket, NewId};
			_Id ->
				{Packet, Id}	
		end,
		%% "Matching id" criteria
		Criteria = fun(#received_packet{id = P_Id}) -> exmpp_utils:any_to_list(P_Id) =:=
																											 exmpp_utils:any_to_list(PacketId)
							 end,
		%% We have all parts, call generic send_sync now...
		send_sync_packet(Session, P, Criteria, Timeout).
		




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
init([JID, Password, Host, Port, Module, Session]) ->
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(Session, JID, Password),
    %% Connect in standard TCP:
    _StreamId = exmpp_session:connect_TCP(Session, Host, Port),
    {ok, #client_state{jid = JID,
								session = Session,
								module = Module,
								handlers = sets:new()
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
handle_call({add_handler, Handler},  _From, #client_state{handlers = Handlers} = State) ->
		{reply,  ok, State#client_state{handlers = sets:add_element(Handler, Handlers)}};

handle_call({remove_handler, HandlerKey},  _From, #client_state{handlers = Handlers} = State) ->
		{reply,  ok, State#client_state{handlers = sets:del_element(HandlerKey, Handlers)}};



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
handle_cast({run, Args}, #client_state{module = Module} = State) ->

		{ok, ModuleState} = Module:run(State, Args),
		{noreply, State#client_state{module_state = ModuleState}};

handle_cast(stop, #client_state{session = Session, module = Module} = State) ->
		exmpp_session:stop(Session),
		spawn(Module, terminate, [State]),
		{stop, normal, State};


handle_cast({feed, Feed}, #client_state{module = Module} = State) ->

		spawn(Module, handle_feed, [Feed, State]),
		{noreply, State};



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

%% All received packets will trigger handle_info calls (because we have assigned the process to be a controlling process of session).
handle_info(Received, 
						#client_state{handlers = Handlers} = State) ->
	%% Dynamic handlers	
	PermanentHandlers = sets:filter(
		fun({_Key, Criteria, Fun, TearOff}) ->
			case apply(Criteria, [Received]) of
					true ->
						spawn(fun() -> apply(Fun, [Received]) end),
						not TearOff;  %% Filtering out tear-off handlers after they were called
					false ->
						true   %% leave handlers that didn't get called
			end
		end,				
		Handlers),		
					%% Module handlers
					handle_info2(Received, State),
	{noreply, State#client_state{handlers = PermanentHandlers}}.

handle_info2(#received_packet{packet_type = message,
                                 type_attr = Type,
                                 from = From,
                                 id = Id,
                                 raw_packet = Packet}, State) ->

				M = State#client_state.module,
				spawn(M, handle_message, [Type, From, Id, Packet, State]),
	{noreply, State};

handle_info2(#received_packet{packet_type = presence,
                                 type_attr = Type,
                                 from = From,
                                 id = Id,
                                 raw_packet = Packet}, State) ->
				io:format("Incoming presence: ~p~n", [Packet]),

		M = State#client_state.module,
		spawn(M, handle_presence, [Type, From, Id, Packet, State]),
	{noreply, State};

handle_info2(#received_packet{packet_type = iq,
                                 type_attr = Type,
                                 from = From,
                                 id = Id,
                                 raw_packet = Packet}, State) ->
		io:format("Incoming: ~p~n", [exmpp_xml:document_to_list(Packet)]),

		M = State#client_state.module,
				lists:foreach(fun(Handler) ->
														case utils:has_behaviour(M, Handler) of
																true ->
																	spawn(Handler, handle_iq, [Type, From, Id, exmpp_iq:xmlel_to_iq(Packet), State]);
																false ->
																		void
														end
										end, iq_handlers()), 
	{noreply, State};

handle_info2(_Info, State) ->
		%%io:format("Incoming: ~p~n", [Info]),
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

%%% Defines list of known IQ handlers

iq_handlers() ->
	[disco_handler, adhoc_handler].


%% This is a version-dependent hack in absence of
%% exmpp_session:get_controlling_process/1 function.
%% There is also possibility to keep receiver Pid in client state but
%% this will require additional parameter for this and other functions...
%% Will review later...

get_receiver_process(Session) ->
 {state, 
	_, _, _, _, 
	Receiver, 
  _, _, _, _, _, _, _} = 
	utils:get_process_state(Session),
  Receiver.

add_handler(Session, Criteria, Callback) ->
	add_handler(Session, Criteria, Callback, false).

add_handler(Session, Criteria, Callback, TearOff) ->
		Receiver = get_receiver_process(Session),
		HandlerKey = utils:generate_random_string(12),
		gen_server:call(Receiver, {add_handler, {HandlerKey, Criteria, Callback, TearOff}}),
		HandlerKey.
		

remove_handler(Session, HandlerKey) ->
		Receiver = get_receiver_process(Session),
		gen_server:call(Receiver, {remove_handler, HandlerKey}),
		HandlerKey.
		
