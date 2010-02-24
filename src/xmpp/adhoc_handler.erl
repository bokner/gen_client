%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with ad-hoc (XEP-0050) capabilities
-module(adhoc_handler).

-behaviour(gen_client).

%% gen_client functions
-export([init/2, terminate/1, terminate/2, handle/2, handle/3]).
%%
%% Exported Functions
%%


-export([behaviour_info/1]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("gen_client.hrl").

% adhoc_client behaviour
% Return a list of required functions and their arity.
%
behaviour_info(callbacks) ->
		[
			{get_adhoc_list, 2},   %% module state, client state
			{execute_command, 5}, %% command, command session, data form, module state, client state
			{cancel_command, 4} %% command, command session, module state, client state
		];
behaviour_info(_Other) -> undefined.


%%
%% API Functions
%%
init(Session, [AdhocImpl, AdhocImplParams]) ->
	adhoc_processor:start_link(Session, AdhocImpl, AdhocImplParams).

%% This should never be called (init returns {ok, ModuleRef}; see gen_client:add_handler/3)
terminate(_ClientState) ->
	throw(unexpected_call).

terminate(AdhocProcessor, _ClientState) ->
	adhoc_processor:stop(AdhocProcessor).

%% This should never be called (init returns {ok, ModuleRef}; see gen_client:add_handler/3)
handle(_Received, _ClientState) ->
	throw(unexpected_call).

%% Handling stanza
handle(AdhocProcessor, #received_packet{from = From, packet_type = iq, raw_packet = IQ}, ClientState) ->
	handle2(From, exmpp_iq:xmlel_to_iq(IQ), ClientState, AdhocProcessor);
%% non-IQ: ignored
handle(_, _, _) ->
	ok.


handle2({Acc, Domain, Resource}, 
					#iq{kind = request, type = set,  ns = ?NS_ADHOC,
							payload = #xmlel{name = 'command', attrs = Attrs}
						 } = IQ, 
					#client_state{session = Session} = ClientState, AdhocProcessor) ->
		Payload = IQ#iq.payload,
		Action = exmpp_xml:get_attribute_from_list(Attrs, action, <<"execute">>),
		Command = exmpp_xml:get_attribute_from_list(Attrs, node, nil),
		Command_Session = exmpp_xml:get_attribute_from_list(Attrs, sessionid, new),
		DataForm = exmpp_xml:get_element(Payload, 'x'),

case Action of
		X when X == <<"execute">> orelse X == <<"complete">> orelse X == <<"next">> ->
				io:format("Execution of command " ++ binary_to_list(Command) ++ " is required.~n"),
				#command_result{result = ResultDataForm, status = Status, sessionid = Sessionid}
						= adhoc_processor:execute(AdhocProcessor, Command, Command_Session, DataForm, ClientState),
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
				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource)))

				;
		<<"cancel">> ->
				io:format("Command " ++ binary_to_list(Command) ++ " is canceled.~n"),
				adhoc_processor:cancel(AdhocProcessor, Command, Command_Session, ClientState),
								%% Construct response
				Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, exmpp_xml:set_attributes(Payload, [{status, canceled}]))
							),
				io:format("Sending command result...~n~p~n", [exmpp_xml:document_to_list(Result)]),
				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource)))

end,
ok;



%% Adhoc Commands list request
handle2({Acc, Domain, Resource} = From,  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS,
																 payload = #xmlel{name = 'query', attrs = [#xmlattr{name = node, value = ?NS_ADHOC_b}]}} = IQ,
					#client_state{jid = JID, session = Session} = ClientState, AdhocProcessor) ->
		io:format("List of ad hoc commands is requested. ~n"),
		%% Construct response
		Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, adhoc_processor:command_items(AdhocProcessor, ClientState), exmpp_jid:to_list(JID))
							),
		gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
		ok;

handle2(_From,  _Packet, _State, _AdhocModule) ->
	ok.




%%
%% Local Functions
%%

