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

-export([init/2, terminate/1, terminate/2, handle/2, handle/3]).

-include("gen_client.hrl").


%% gen_client API callbacks

init(Session, [OnlineStatus]) ->
		gen_client:login(Session),
		gen_client:send_packet(Session, stanza:available(OnlineStatus)),
	ok.

%% Doesn't handle anything
handle(_Received, _State) ->
	ok.

terminate(State) ->
				Session = State#client_state.session,
				gen_client:send_packet(Session, stanza:unavailable()),
				io:format("Dummy client has finished."),
		ok.

%% Following functions should never be called
terminate(_ModuleRef, _State) ->
	ok.

handle(_ModuleRef, _Received, _State) ->
	ok.

