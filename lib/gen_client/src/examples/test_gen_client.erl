%% Author: bokner
%% Created: May 1, 2010
%% Description: TODO: Add description to test_gen_client
-module(test_gen_client).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0, test_adhoc/0]).

%%
%% API Functions
%%
test() ->
 exmpp:start(), 
	{ok, Client1} = gen_client:start("gen_client@jabber.ru", "jabber.ru", 5222, "test", 
																	 [{debug, true}, {presence, {true, "I'm online."}}, {reconnect, 15000}]),	
	gen_client:add_plugin(Client1, disco_plugin, [test_disco, []]),
  %% Log in with 2nd client and send discovery request to 1st client
	{ok, Client2} = gen_client:start("gen_client2@jabber.ru", "jabber.ru", 5222, "test", 
																	 [{debug, true}, {presence, {true, "I'm online."}}, {reconnect, 15000}]),

	%% We want to know what resource the first client was assigned, as disco requests should be sent to a particular resource
	Jid1 = gen_client:get_client_jid(Client1),
	%% We need to convert it to string to comply with exmpp_client_disco calls 
	Jid1Str = exmpp_jid:to_list(Jid1),
	{ok, Info} = gen_client:send_sync_packet(Client2, exmpp_client_disco:info(Jid1Str), 10000),
	io:format("Disco info from gen_client:~p~n", [gen_client_utils:get_xml(Info)]),
	{ok, Items} = gen_client:send_sync_packet(Client2, exmpp_client_disco:items(Jid1Str), 10000),
	io:format("Disco items from gen_client:~p~n", [gen_client_utils:get_xml(Items)]),	
	ok.

test_adhoc() ->
	{ok, Client1} = gen_client:start("test@localhost", "vroc.oksphere.com", 5222, "test", 
																	 [{debug, true}, {presence, {true, "test account is online."}}, {reconnect, 15000}]),	
	gen_client:add_plugin(Client1, disco_plugin, [test_disco, []]),
	gen_client:add_plugin(Client1, adhoc_plugin, [test_adhoc, []]).
	

 
%%
%% Local Functions
%%

