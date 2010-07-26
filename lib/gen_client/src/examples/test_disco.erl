%% Author: bokner
%% Created: May 1, 2010
%% Description: Example of plugin implementation
%% The code implements disco_plugin behaviour,
%% which enables user-defined handling of XEP-0030 requests
	
-module(test_disco).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([disco_info/2, disco_info/3, disco_items/2, disco_items/3]).

-behaviour(disco_plugin).
%%
%% API Functions
%%
disco_info(_From, _Args) ->
	lists:map(fun(DI) -> [P] = exmpp_xml:parse_document(DI), P end,
		  [io_lib:format("<identity
        category='client'
        type='test_gen_client'
				node='unit'
        name='~s'></identity>", [gen_client_utils:get_MAC()]),
		    "<feature var='jabber:iq:time'/>",
		    "<feature var='jabber:iq:version'/>",
			  "<feature var='http://jabber.org/protocol/commands'/>"
			]).


disco_info(_From, _Args, _Node) ->
	ignore.

%% Example of items (taken from XEP-0030)
disco_items(_From, _Args) ->
	lists:map(fun(DI) -> [P] = exmpp_xml:parse_document(DI), P end,		
    ["<item jid='catalog.localhost'
          node='books'
          name='Books by and about Shakespeare'/>",		 
    "<item jid='catalog.localhost'
          node='clothing'
          name='Wear your literary taste with pride'/>",
		 
    "<item jid='catalog.localhost'
          node='music'
          name='Music from the time of Shakespeare'/>"
		]).

disco_items(_From, _Args, _Node) ->
	ignore.
%%
%% Local Functions
%%

