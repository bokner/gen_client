%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with disco capabilities
-module(disco_handler).

-behaviour(gen_handler).

%%
%% Exported Functions
%%


-export([behaviour_info/1]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("gen_client.hrl").

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

%% Here we handle disco
% Disco#info
handle(#received_packet{from = From, raw_packet = IQ}, State, DiscoModule) ->
	handle2({Acc, Domain, Resource} = From, IQ, State, DiscoModule).




handle2({Acc, Domain, Resource} = _From, #iq{kind = request, type = get,  ns = ?NS_DISCO_INFO} = IQ, #client_state{session = Session} = State, DiscoModule) ->
				Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, DiscoModule:disco_info(State))
							),
				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;



% Disco#items
handle2({Acc, Domain, Resource} = From,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, payload = undefined} = IQ, #client_state{session = Session} = State, DiscoModule) ->
				Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, DiscoModule:disco_items(State))
							),
				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;

%% Other IQ types - do nothing
handle2(_From,  _IQ, _State, _DiscoModule) ->
	ok.
