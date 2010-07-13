%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with disco capabilities
-module(disco_handler).

-behaviour(gen_client).

%%
%% Exported Functions
%%
-export([init/2, terminate/1, terminate/2, handle/2, handle/3]).


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
			{disco_items, 2}, %% module state, client state
			{disco_info, 2},  %%
						
			{disco_items, 3}, %% states (module, client) & node
			{disco_info, 3}   %% 
		];
behaviour_info(_Other) -> undefined.

-define(QUERY_INFO,
  #xmlel{ns = ?NS_DISCO_INFO, name = 'query'}
).

-define(QUERY_ITEMS,
  #xmlel{ns = ?NS_DISCO_ITEMS, name ='query'}
).
%%
%% API Functions
%%
init(Session, [DiscoModule, DiscoModuleParams]) ->
	{ok, {DiscoModule, DiscoModuleParams}}.

terminate(_ClientState) ->
	throw(unexpected_call).

terminate(_DiscoRef, _ClientState) ->
	ok.

handle(_Received, _ClientState) ->
	throw(unexpected_call).
%% Here we handle disco
%% All IQ stanzas
handle([DiscoModule, DiscoModuleParams], #received_packet{from = From, packet_type = iq, raw_packet = IQ}, ClientState) ->
	handle2(From, exmpp_iq:xmlel_to_iq(IQ), ClientState, DiscoModule, DiscoModuleParams);

handle(_, _, _) ->
	ok.

%% disco#info 
handle2({Acc, Domain, Resource} = _From, #iq{kind = request, type = get,  ns = ?NS_DISCO_INFO, payload = #xmlel{attrs = []}} = IQ, #client_state{session = Session} = ClientState, DiscoModule, DiscoModuleParams) ->
	Query = exmpp_xml:set_children(?QUERY_INFO, DiscoModule:disco_info(DiscoModuleParams, ClientState)),
				Result = 
					exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Query
															 
							)
															),

				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;

%% disco#info with node
handle2({Acc, Domain, Resource} = _From, #iq{kind = request, type = get,  ns = ?NS_DISCO_INFO, 
																						 payload = #xmlel{attrs = [#xmlattr{name = 'node', value = Node}]}} = IQ, #client_state{session = Session} = ClientState, DiscoModule, DiscoModuleParams) ->
	Query = exmpp_xml:set_children(?QUERY_INFO, DiscoModule:disco_info(DiscoModuleParams, ClientState, Node)),
				Result = 
					exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Query
															 
							)
															),

				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;


% Disco#items
handle2({Acc, Domain, Resource} = _From,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, payload = #xmlel{name = 'query', attrs = []}} = IQ, #client_state{session = Session} = ClientState, DiscoModule, DiscoModuleParams) ->
	Query = exmpp_xml:set_children(?QUERY_ITEMS, DiscoModule:disco_items(DiscoModuleParams, ClientState)),
				Result = 
					exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Query
															 
							)
															),
					gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),

		ok;

% Disco#items and node
handle2({Acc, Domain, Resource} = _From,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, payload = #xmlel{name = 'query', attrs = [#xmlattr{name = 'node', value = Node}]}} = IQ, #client_state{session = Session} = ClientState, DiscoModule, DiscoModuleParams) ->
	Items = DiscoModule:disco_items(DiscoModuleParams, ClientState, Node),
	case Items of
	ignore ->
		ok;
	_Other -> 
		Query = exmpp_xml:set_children(?QUERY_ITEMS, Items),
				Result = 
					exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, Query
							)
															),
					gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
		ok
	end;

%% Other IQ types - do nothing
handle2(_From,  _IQ, _State, _DiscoModule, _DiscoModuleParams) ->
	ok.
