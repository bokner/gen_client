%% Author: bokner
%% Created: Feb 3, 2010
%% Description: Monitors and clears temporary pubsub subscriptions.
-module(tidy_bot).



%%
%% Include files
%%
-include_lib("exmpp/include/exmpp_client.hrl").
%%
%% Exported Functions
%%
-export([tidy_subscriptions/5, unsubscribe_all_jids/2, unsubscribe_with_subid/4, process_subscriptions/3]).
%%
%% API Functions
%%
tidy_subscriptions(Jid, Password, Host, Port, PubSub) ->
	{ok, Session} = gen_client:start(Jid, Host, Port, Password, [{presence, {true, "On tidy duty"}}, {reconnect, 15000}]),
	BotJid = gen_client:get_client_jid(Session),
	JidOfflineHandler = 
		fun(#received_packet{packet_type = presence, type_attr = "unavailable", from = PeerJid}, _Session) when BotJid /= PeerJid ->
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
	F = 		fun(SubscriptionList) ->
							 io:format("Tidying up...~n"),
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
											
											) end,

	process_subscriptions(Session, PubSub, F),

		timer:apply_interval(120000, ?MODULE, process_subscriptions,  [Session, PubSub, F]),
	
	ok.

unsubscribe_temporary(Session, PubSub, Jid, Node, Subid) ->
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
			unsubscribe_from_node(Session, Jid, Node, Subid, PubSub),
			timeout;
		{ok, #received_packet{type_attr = Type}} ->
			io:format("Probe:~p:~p~n", [Jid, Type]);
		Other ->
			io:format("Unexpected:~p~n", [Other])
	end.

unsubscribe_from_node(Session, Jid, Node, SubId, PubSub) ->
	io:format("Unsubscribing ~p from ~p...", [Jid, Node]),
	gen_client:send_packet(Session, unsubscribe_with_subid(Jid, PubSub, Node, SubId)),
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
																			 unsubscribe_from_node(Session, JidAttr, exmpp_xml:get_attribute(S, "node", undefined), exmpp_xml:get_attribute(S, "subid", undefined), PubSub);
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


unsubscribe_all_jids(Session, PubSub) ->

	process_subscriptions(
		Session, PubSub, 
		fun(SList) ->
				 io:format("Subscriptions:~p~n", [length(SList)]),
				 lists:foreach(fun(S) -> 
														spawn(
															fun() -> 
																	 JidAttr = exmpp_xml:get_attribute(S, "jid", undefined),
																	 	io:format("Unsubscribing ~p~n", [JidAttr]),
																			 unsubscribe_from_node(Session, JidAttr, exmpp_xml:get_attribute(S, "node", undefined), exmpp_xml:get_attribute(S, "subid", undefined),PubSub)
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

unsubscribe_with_subid(Jid, PubSub, Node, Subid) ->
	S = exmpp_client_pubsub:unsubscribe(Jid, PubSub, Node),
	Pubsub = exmpp_xml:get_element(S, "pubsub"),
	Unsubscribe = exmpp_xml:set_attribute(
		exmpp_xml:get_element(
			Pubsub,  "unsubscribe"), subid, Subid),
	NewPubsub = exmpp_xml:append_child(exmpp_xml:remove_element(Pubsub, "unsubscribe"), Unsubscribe),
	exmpp_xml:append_child(exmpp_xml:remove_element(S, "pubsub"), NewPubsub).


																				
		