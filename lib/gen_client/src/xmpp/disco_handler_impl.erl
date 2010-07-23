%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with disco capabilities
-module(disco_handler_impl, [DiscoImpl, DiscoParams]).

%%
%% Exported Functions
%%



%% Exported for Module:fun calls
-export([handle/3]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("gen_client.hrl").

-define(QUERY_INFO,
				#xmlel{ns = ?NS_DISCO_INFO, name = 'query'}
			 ).

-define(QUERY_ITEMS,
				#xmlel{ns = ?NS_DISCO_ITEMS, name ='query'}
			 ).
%%
%% API Functions
%%

%% disco#info 
handle(Client, {Acc, Domain, Resource} = From, #iq{kind = request, type = get,  ns = ?NS_DISCO_INFO, payload = #xmlel{attrs = []}} = IQ) ->
	Info = DiscoImpl:disco_info(From, DiscoParams),
	case Info of
		ignore -> ok;
		_Other ->
			Query = exmpp_xml:set_children(?QUERY_INFO, Info),
			Result = 
				exmpp_iq:iq_to_xmlel(
					exmpp_iq:result(IQ, Query
												 )
														),			
			gen_client:send_packet(Client, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
			
			ok
	end;

%% disco#info with node
handle(Client, {Acc, Domain, Resource} = From, #iq{kind = request, type = get,  ns = ?NS_DISCO_INFO, 
																						payload = #xmlel{attrs = [#xmlattr{name = 'node', value = Node}]}} = IQ) ->
	Info = DiscoImpl:disco_info(From, DiscoParams, Node),
	case Info of
		ignore -> ok;
		_Other ->
			Query = exmpp_xml:set_children(?QUERY_INFO, Info),
			Result = 
				exmpp_iq:iq_to_xmlel(
					exmpp_iq:result(IQ, Query
												 
												 )
														),
			
			gen_client:send_packet(Client, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
			
			ok
	end;


% Disco#items
handle(Client, {Acc, Domain, Resource} = From,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, payload = #xmlel{name = 'query', attrs = []}} = IQ) ->
	Items = DiscoImpl:disco_items(From, DiscoParams),
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
			gen_client:send_packet(Client, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
			
			ok
	end;

% Disco#items and node
handle(Client, {Acc, Domain, Resource} = From,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS, payload = #xmlel{name = 'query', attrs = [#xmlattr{name = 'node', value = Node}]}} = IQ) ->
	Items = DiscoImpl:disco_items(From, DiscoParams, Node),
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
			gen_client:send_packet(Client, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
			ok
	end;

%% Other IQ types - do nothing
handle(_Client, _From,  _IQ) ->
	ok.
