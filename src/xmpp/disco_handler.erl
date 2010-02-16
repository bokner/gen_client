%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with disco capabilities
-module(disco_handler).

-behaviour(gen_handler).

%%
%% Exported Functions
%%

-define(QUERY_INFO,
  #xmlel{ns = ?NS_DISCO_INFO, name = 'query'}
).

-define(QUERY_ITEMS,
  #xmlel{ns = ?NS_DISCO_ITEMS, name ='query'}
).

-export([behaviour_info/1]).
-export( [handle/3]).

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
			{disco_items, 2}, %% state & node
			{disco_info, 1},   %% state
			{disco_info, 2}  %% state & node
		];
behaviour_info(_Other) -> undefined.


%%
%% API Functions
%%

%% Here we handle disco
%% All IQ stanzas
handle(#received_packet{from = From, packet_type = iq, raw_packet = IQ}, State, DiscoModule) ->
	handle2(From, exmpp_iq:xmlel_to_iq(IQ), State, DiscoModule);

handle(_, _, _) ->
	ok.

%% disco#info 
handle2({Acc, Domain, Resource} = _From, #iq{kind = request, type = get,  ns = ?NS_DISCO_INFO, payload = #xmlel{attrs = []}} = IQ, #client_state{session = Session} = State, DiscoModule) ->
	Query = exmpp_xml:set_children(?QUERY_INFO, DiscoModule:disco_info(State)),
				Result = 
					exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Query
															 
							)
															),

				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;

%% disco#info with node
handle2({Acc, Domain, Resource} = _From, #iq{kind = request, type = get,  ns = ?NS_DISCO_INFO, 
																						 payload = #xmlel{attrs = [#xmlattr{name = 'node', value = Node}]}} = IQ, #client_state{session = Session} = State, DiscoModule) ->
	Query = exmpp_xml:set_children(?QUERY_INFO, DiscoModule:disco_info(State, Node)),
				Result = 
					exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Query
															 
							)
															),

				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;


% Disco#items
handle2({Acc, Domain, Resource} = _From,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, payload = #xmlel{name = 'query', attrs = []}} = IQ, #client_state{session = Session} = State, DiscoModule) ->
	Query = exmpp_xml:set_children(?QUERY_ITEMS, DiscoModule:disco_items(State)),
				Result = 
					exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Query
															 
							)
															),
					gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;

% Disco#items and node
handle2({Acc, Domain, Resource} = _From,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, payload = #xmlel{name = 'query', attrs = [#xmlattr{name = 'node', value = Node}]}} = IQ, #client_state{session = Session} = State, DiscoModule) ->
	Query = exmpp_xml:set_children(?QUERY_ITEMS, DiscoModule:disco_items(State, Node)),
				Result = 
					exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Query
															 
							)
															),
					gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;

%% Other IQ types - do nothing
handle2(_From,  #iq{kind = Kind, type = Type, ns = NS, payload = Payload} = IQ, _State, _DiscoModule) ->
	ok.
