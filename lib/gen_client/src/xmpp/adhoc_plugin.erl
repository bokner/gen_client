%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with ad-hoc (XEP-0050) capabilities
-module(adhoc_plugin).

-behaviour(gen_client_plugin).

%%
%% Exported Functions
%%
-export([behaviour_info/1]).
-export([init/1, terminate/1, handle/3]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("ad_hoc.hrl").

% adhoc_client behaviour
% Return a list of required functions and their arity.
%
behaviour_info(callbacks) ->
	[
	 {commands, 2}   %% requester ({Acc, Domain, Resource}), arguments
	];
behaviour_info(_Other) -> undefined.


%%
%% API Functions
%%
init([AdhocImpl, AdhocImplParams]) ->
	adhoc_processor:start_link(AdhocImpl, AdhocImplParams).

terminate(AdhocProcessor) ->
	adhoc_processor:stop(AdhocProcessor).

%% Handling stanza
handle(#received_packet{from = Requester, packet_type = iq, raw_packet = IQ}, Client, AdhocProcessor) ->
	handle2(Requester, exmpp_iq:xmlel_to_iq(IQ), Client, AdhocProcessor);
%% non-IQ: ignored
handle(_, _, _) ->
	ok.

handle2({Acc, Domain, Resource} = Requester, 
				#iq{kind = request, type = set,  ns = ?NS_ADHOC,
						payload = #xmlel{name = 'command', attrs = Attrs}
					 } = IQ, 
				Client, AdhocProcessor) ->
	RequesterJid = exmpp_jid:make(Acc, Domain, Resource),
	Payload = IQ#iq.payload,
	Action = exmpp_xml:get_attribute_from_list(Attrs, action, <<"execute">>),
	Command = exmpp_xml:get_attribute_from_list(Attrs, node, nil),
	Command_Session = exmpp_xml:get_attribute_from_list(Attrs, sessionid, new),
	DataForm = exmpp_xml:get_element(Payload, 'x'),
	
	case Action of
		X when X == <<"execute">> orelse X == <<"complete">> orelse X == <<"next">> ->
			io:format("Execution of command " ++ binary_to_list(Command) ++ " is required.~n"),
			#command_result{result = ResultDataForm, status = Status, sessionid = Sessionid}
											 = adhoc_processor:execute(AdhocProcessor, Command, Command_Session, DataForm, Requester),
			io:format("Status:~p, session id: ~p, data form: ~p~n", [Status, Sessionid, exmpp_xml:document_to_list(ResultDataForm)]),
			NewPayload = exmpp_xml:set_attributes(
										 case DataForm of
											 undefined ->
												 exmpp_xml:append_child(Payload, ResultDataForm);
											 _ ->
												 exmpp_xml:replace_child(Payload, DataForm, ResultDataForm)
										 end,
										 [{status, Status}, {sessionid, Sessionid}]),
			io:format("New IQ is: ~n~p~n", [exmpp_xml:document_to_list(NewPayload)]),
			%% Construct response
			Result = exmpp_iq:iq_to_xmlel(
								 exmpp_iq:result(IQ, NewPayload)
																	 ),
			io:format("Sending command result...~n~p~n", [exmpp_xml:document_to_list(Result)]),
			gen_client:send_packet(Client, exmpp_stanza:set_recipient(Result, RequesterJid));
		<<"cancel">> ->
			io:format("Command " ++ binary_to_list(Command) ++ " is canceled.~n"),
			adhoc_processor:cancel(AdhocProcessor, Command, Command_Session, Client),
			%% Construct response
			Result = exmpp_iq:iq_to_xmlel(
								 exmpp_iq:result(IQ, exmpp_xml:set_attributes(Payload, [{status, canceled}]))
																	 ),
			io:format("Sending command result...~n~p~n", [exmpp_xml:document_to_list(Result)]),
			gen_client:send_packet(Client, exmpp_stanza:set_recipient(Result, RequesterJid))
	
	end,
	ok;

%% Adhoc Commands list request
handle2({Acc, Domain, Resource} = Requester,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS,
																							payload = #xmlel{name = 'query', attrs = [#xmlattr{name = node, value = ?NS_ADHOC_b}]}} = IQ,
				Client, AdhocProcessor) ->
	io:format("List of ad hoc commands is requested. ~n"),
	%% Construct response
	ClientJid = gen_client:get_client_jid(Client),
	Result = exmpp_iq:iq_to_xmlel(
						 exmpp_iq:result(IQ, 
															 adhoc_processor:command_items(AdhocProcessor, Requester, ClientJid))
															 ),
	gen_client:send_packet(Client, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
	ok;

handle2(_Requester,  _Packet, _State, _AdhocModule) ->
	ok.




%%
%% Local Functions
%%