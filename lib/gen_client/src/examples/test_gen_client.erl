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
-export([test/0]).

%%
%% API Functions
%%
test() ->

	{ok, Client1} = gen_client:start("gen_client@jabber.ru/1", "jabber.ru", 5222, "test", [{debug, true}, {presence, {true, "I'm online."}}, {reconnect, 15000}]),	
	gen_client:add_plugin(Client1, disco_plugin, [test_disco, []]),
  %% Log in with 2nd client and send discovery request to 1st client
	{ok, Client2} = gen_client:start("gen_client2@jabber.ru/2", "jabber.ru", 5222, "test", [{debug, true}, {presence, {true, "I'm online."}}, {reconnect, 15000}]),

	{ok, Info} = gen_client:send_sync_packet(Client2, exmpp_client_disco:info("gen_client@jabber.ru/1"), 10000),
	io:format("Disco info from gen_client:~p~n", [gen_client:get_xml(Info)]),
	{ok, Items} = gen_client:send_sync_packet(Client2, exmpp_client_disco:items("gen_client@jabber.ru/1"), 10000),
	io:format("Disco items from gen_client:~p~n", [gen_client:get_xml(Items)]),	
	ok.
%%
%% Local Functions
%%

