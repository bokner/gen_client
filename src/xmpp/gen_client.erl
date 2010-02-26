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
				 add_handler/2, add_handler/3,
				 remove_handler/2]).

%% 
-export([get_client_state/1]).

-export([behaviour_info/1]).


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

start(Account, Domain, Resource, Host, Port, Password, Module, Args) ->
	application:start(exmpp),
	start(exmpp_jid:make(Account, Domain, Resource), Host, Port, Password, Module, Args).

start(Account, Domain, Host, Port, Password, Module, Args) ->
	start(Account, Domain, random, Host, Port, Password, Module, Args).


start(Jid, Host, Port, Password, Module, Args) when is_list(Jid) ->
	application:start(exmpp),	

	start(utils:to_jid(Jid), Host, Port, Password, Module, Args);

start(Jid, Host, Port, Password, Module, Args) when ?IS_JID(Jid) ->
	{ok, Session} = start(Jid, Host, Port, Password), 	
	add_handler(Session, Module, Args),
	{ok, Session}.

start(Jid, Host, Port, Password) when ?IS_JID(Jid) ->		
	Session = exmpp_session:start(),
	{ok, Client} = gen_server:start_link(?MODULE, [Jid, Password, Host, Port, Session], []),
	exmpp_session:set_controlling_process(Session, Client),
	{ok, Session}.



login(Session) ->
	try exmpp_session:login(Session)
	catch
		throw:T ->
			T
	end.


%% Send packet asynchronously
send_packet(Session, Packet) ->
	io:format("Outgoing:~p~n", [exmpp_xml:document_to_list(Packet)]),
	spawn(fun() -> exmpp_session:send_packet(Session, Packet) end).

%%
%% Sends packet and waits Timeout ms for the packet matching the 
%% Trigger to happen. If packet arrives, returns {ok, Packet};
%% otherwise, returns timeout
%% 
send_sync_packet(Session, Packet, Trigger, Timeout) ->
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
	HandlerKey = add_handler(Session, Callback),
	%% ...send the packet...
  send_packet(Session, Packet),
	%% ...and wait for response (it should come from controlling process)
	R = receive 
				{ok, Response, ResponseId} -> %% we only interested in a response to our Id, 
					%% ignoring all others;
					{ok, Response} %% return response and let calling module to deal with it.
				after Timeout ->			
					timeout
			end,
	remove_handler(Session, HandlerKey),	
	R.		


%%
%% The default sync Trigger  is "match response and request by id".
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
	Trigger = fun(#received_packet{id = P_Id}, _State) -> exmpp_utils:any_to_list(P_Id) =:=
																													exmpp_utils:any_to_list(PacketId)
						end,
	%% We have all parts, call generic send_sync now...
	send_sync_packet(Session, P, Trigger, Timeout).





stop(Session) ->
	Receiver = get_receiver_process(Session),
	gen_server:cast(Receiver, stop).


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
init([JID, Password, Host, Port, Session]) ->
	%% Create a new session with basic (digest) authentication:
	exmpp_session:auth_basic_digest(Session, JID, Password),
	%% Connect in standard TCP:
	_StreamId = exmpp_session:connect_TCP(Session, Host, Port),
	{ok, #client_state{jid = JID,
										 session = Session,
										 handlers = [], %%Keyed list {Key, Handler},
										 terminators = [] %% Keyed list {Key, Terminator}
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
handle_call({add_handler, Handler},  _From, #client_state{handlers = Handlers, terminators = Terminators} = State) ->

	HandlerKey = generateHandlerKey(Handler),
		io:format("Adding handler ~p~n", [HandlerKey]),
		NewState = case Handler of
			{HandlerFunc, TerminatorFunc} ->
				State#client_state{handlers = [{HandlerKey, HandlerFunc}| Handlers], terminators = [{HandlerKey, TerminatorFunc} | Terminators]};
			_H ->
				State#client_state{handlers = [{HandlerKey, Handler}| Handlers]}
		end,
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

%% All received packets will trigger handle_info calls (because we have assigned the process to be a controlling process of session).
handle_info(Received, 
						#client_state{handlers = Handlers, session = Session} = State) ->
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

%% Module handler
%% We store handler function and terminator function for every module
add_handler(Session, Module, Args) when is_atom(Module) ->
	case utils:has_behaviour(Module, gen_client) of 
		true ->
			case Module:init(Session, Args) of
				{ok, ModuleRef} ->
					add_handler(Session, {fun(Received, State) -> Module:handle(ModuleRef, Received, State) end,
																fun(State) -> Module:terminate(ModuleRef, State) end});
				ok ->
					add_handler(Session, {fun(Received, State) -> Module:handle(Received, State) end,
																fun(State) -> Module:terminate(State) end});
				Other ->
					throw({moduleInitFailed, {Module, Other}})
			end;		
		false ->
			throw({"module has to have 'gen_client' behaviour", Module})
	end.

add_handler(Session, Handler) ->
	Receiver = get_receiver_process(Session),
	gen_server:call(Receiver, {add_handler, Handler}).



remove_handler(Session, HandlerKey) ->
	Receiver = get_receiver_process(Session),
	gen_server:call(Receiver, {remove_handler, HandlerKey}).

generateHandlerKey(Handler) ->
	exmpp_utils:random_id("handler." ++ lists:flatten(io_lib:format("~p", [Handler]))).

get_client_state(Session) ->
	Receiver = get_receiver_process(Session),
	gen_server:call(Receiver, get_client_state).


