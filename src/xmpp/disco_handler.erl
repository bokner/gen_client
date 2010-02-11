%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with disco capabilities
-module(disco_handler).

-behaviour(gen_client).

%%
%% Exported Functions
%%

%% gen_client functions
-export([run/2, terminate/1, handle_iq/5, handle_presence/5, handle_message/5, handle_feed/2]).

-export([behaviour_info/1]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("include/gen_client.hrl").
% disco_client behaviour
% Return a list of required functions and their arity.
%
behaviour_info(callbacks) ->
		[
			{disco_items, 1}, %% state
			{disco_info, 1}   %% state
		];
behaviour_info(_Other) -> undefined.


%%
%% API Functions
%%

%% Pass-through functions; we delegate to the module everything except disco handling
run(#client_state{module = Module} = State, Args) ->
		Module:run(State, Args).

terminate(#client_state{module = Module} = State) ->
	Module:terminate(State).

handle_message(Type, From, Id, Packet, #client_state{module = Module} = State) ->
	Module:handle_message(Type, From, Id, Packet, #client_state{module = Module} = State).


handle_presence(Type, From, Id, Packet, #client_state{module = Module} = State) ->
	Module:handle_presence(Type, From, Id, Packet, #client_state{module = Module} = State).


handle_feed(Feed, #client_state{module = Module} = State) ->
	Module:handle_feed(Feed, #client_state{module = Module} = State).


%% Here we handle disco
% Disco#info
handle_iq(_Type, {Acc, Domain, Resource} = _From, _Id, #iq{kind = request, type = get,  ns = ?NS_DISCO_INFO} = IQ, #client_state{jid = JID, module = Module, session = Session} = State) ->
				Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Module:disco_info(State))
							),
				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		{ok, State#client_state.module_state};



% Disco#items
handle_iq(_Type, {Acc, Domain, Resource} = _From, _Id, #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, payload = undefined} = IQ, #client_state{jid = JID, module = Module, session = Session} = State) ->
				Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Module:disco_items(State))
							),
				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		{ok, State#client_state.module_state};

%% Other IQ types - do nothing
handle_iq(Type, From, Id, Packet, State) ->
		{ok, State#client_state.module_state}.	


%%
%% Local Functions
%%

