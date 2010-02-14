%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%%
%%% Dummy client, ignores all incoming messages.
%%% Still possible to send messages out and add handlers later for incoming messages using session process handle.
%%% @end
%%% Created : 23 Oct 2009 by Boris Okner <boris.okner@gmail.com>

-module(dummy_client).

-behaviour(gen_client).


-export([run/2, terminate/1, handle_iq/6, handle_presence/5, handle_message/5]).



-include("gen_client.hrl").


%% gen_client API callbacks

run(State, [OnlineStatus]) ->
		Session = State#client_state.session,
		gen_client:login(Session),
		gen_client:send_packet(Session, stanza:available(OnlineStatus)),

	io:format("Dummy client has started."),
		{ok, dummy_state}.

terminate(State) ->
				Session = State#client_state.session,
				gen_client:send_packet(Session, stanza:unavailable()),
				io:format("Dummy client has finished."),
		ok.

handle_iq(_Type, _From, _Id, _NS, _IQ, State) ->
		{ok, State#client_state.module_state}.


handle_presence(_Type, _From, _Id, _Packet, State) ->
		{ok, State#client_state.module_state}.



handle_message(_Type, _From, _Id, _Packet, State) ->
		{ok, State#client_state.module_state}.

