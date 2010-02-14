%% Author: bokner
%% Created: Feb 3, 2010
%% Description: Monitors and clears temporary pubsub subscriptions.
-module(tidy_bot).


%%
%% Include files
%%
-include_lib("exmpp/include/exmpp_client.hrl").
-include("gen_client.hrl").
%%
%% Exported Functions
%%
-export([tidy_subscriptions/5]).
%%
%% API Functions
%%
tidy_subscriptions(Jid, Password, Host, Port, PubSub) ->
	{ok, Session, _C} = gen_client:start(Jid, Host, Port, Password, dummy_client, ["On tidy duty"]),
	JidOfflineHandler = 
		fun(#received_packet{packet_type = presence, type_attr = "unavailable", from = PeerJid}, #client_state{jid = BotJid} = _State) when BotJid /= PeerJid ->
				 {Node, Domain, _Resource} = PeerJid,	
				 case exmpp_jid:bare_compare(BotJid, exmpp_jid:make(Node, Domain)) of
					 false ->
						 io:format("~p gone offline~n", [PeerJid]),
						 unsubscribe_from_all_nodes(Session, PeerJid, PubSub);														
					 _Other ->
						 void
				 end,
				 ok;
			(_Other, _Session) ->
				ok
		end,
	gen_client:add_handler(Session, JidOfflineHandler),	
	
	%% Get subscriptions
	
	process_subscriptions(
		Session, PubSub, 
		fun(SubscriptionList) ->
				 lists:foreach(fun(S) -> 
														spawn(
															fun() -> 
																	 unsubscribe_temporary(Session, PubSub,
																												 exmpp_xml:get_attribute(S, "jid", undefined),
																												 exmpp_xml:get_attribute(S, "node", undefined),
																												 exmpp_xml:get_attribute(S, "subid", undefined)
																												)
															end
																 )
											 
											 end, 
											 SubscriptionList
											
											) end),
	ok.

unsubscribe_temporary(Session, PubSub, Jid, Node, _Subid) ->
	%% Prepare trigger for probe response
	ProbeSuccessful = fun(#received_packet{from = FullJid, packet_type = presence, type_attr = "available"}, _State) ->
													{Acc, Domain, Resource} = FullJid,										
													case exmpp_jid:parse(Jid) of
														{jid, Jid, Acc, Domain, Resource} ->
															io:format("probe matches for ~p~n", [FullJid]),
															true;
														_NoMatch ->
															io:format("probe doesn't match for ~p, ~p~n", [Jid, FullJid]),
															false
													end;
											 (_NonPresence, _State) ->
												 false
										 end, 
	%% Send presence probe
	io:format("Sending probe to ~p:~n", [Jid]),
	
	ProbeResult = gen_client:send_sync_packet(Session, exmpp_stanza:set_recipient(
																							exmpp_presence:probe(), Jid), ProbeSuccessful, 4000),
	io:format("result of probe for ~p:~n~p~n", [Jid, ProbeResult]),
	case ProbeResult of 
		timeout ->
			unsubscribe_from_node(Session, Jid, Node, PubSub),
			timeout;
		{ok, #received_packet{type_attr = Type}} ->
			io:format("Probe:~p:~p~n", [Jid, Type]);
		Other ->
			io:format("Unexpected:~p~n", [Other])
	end.

unsubscribe_from_node(Session, Jid, Node, PubSub) ->
	io:format("Unsubscribing ~p from ~p...", [Jid, Node]),
	gen_client:send_packet(Session, exmpp_client_pubsub:unsubscribe(Jid, PubSub, Node)),
	io:format("Done.~n").

unsubscribe_from_all_nodes(Session, {Acc, Domain, Resource} = Jid, PubSub) ->
	io:format("Unsubscribing ~p~n", [Jid]),
	process_subscriptions(
		Session, PubSub, 
		fun(SList) ->
				 lists:foreach(fun(S) -> 
														spawn(
															fun() -> 
																	 JidAttr = exmpp_xml:get_attribute(S, "jid", undefined),
																	 case JidAttr == exmpp_jid:to_binary(Acc, Domain, Resource) of
																		 true ->
																			 unsubscribe_from_node(Session, JidAttr, exmpp_xml:get_attribute(S, "node", undefined), PubSub);
																		 false ->
																			 void
																	 end
															
															end
																 )
											 
											 end, 
											 SList
											
											) end								
											 ),
	ok.

process_subscriptions(Session, PubSub, Fun) ->
	{ok, SubscriptionPacket} = gen_client:send_sync_packet(Session, exmpp_client_pubsub:get_subscriptions(PubSub), 5000),
	%%io:format("Subscriptions:~p~n", [SubscriptionPacket]),
	Payload = exmpp_iq:get_payload(exmpp_iq:xmlel_to_iq(SubscriptionPacket#received_packet.raw_packet)),	
	Fun(
		exmpp_xml:get_elements(
			exmpp_xml:get_element(Payload, "subscriptions"),
			"subscription")
		 ).
