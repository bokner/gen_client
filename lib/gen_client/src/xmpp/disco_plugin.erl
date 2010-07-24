%%% -------------------------------------------------------------------
%%% Author  : bokner
%%% Description :
%%%
%%% Created : Jul 23, 2010
%%% -------------------------------------------------------------------
-module(disco_plugin).
-behaviour(gen_client_plugin).
-export([init/1, terminate/1, handle/3]).

% disco_handler behaviour
% Return a list of required functions and their arity.
%
-export([behaviour_info/1]).
behaviour_info(callbacks) ->
	[
	 {disco_items, 2}, %% "from", arguments
	 {disco_info, 2},  %% "from", arguments
	 {disco_items, 3}, %% "from", arguments, node
	 {disco_info, 3}   %% "from", arguments, node
	];
behaviour_info(_Other) -> undefined.

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").


init([DiscoImpl, DiscoParams]) ->
	{ok, disco_handler_impl:new(DiscoImpl, DiscoParams)}.

terminate(_DiscoRef) ->
	ok.

%% Here we handle disco
%% All IQ stanzas
handle(#received_packet{from = From, packet_type = iq, raw_packet = IQ}, Client, ModuleRef) ->
	ModuleRef:handle(Client, From, exmpp_iq:xmlel_to_iq(IQ));

%% Ignore non-iq packets
handle(_, _, _) ->
	ok.

