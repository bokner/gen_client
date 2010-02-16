%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with ad-hoc (XEP-0050) capabilities
-module(adhoc_handler).

-behaviour(gen_handler).

%%
%% Exported Functions
%%

%% gen_client functions
-export([handle/3]).

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
			{get_adhoc_list, 1},   %% state
			{execute_command, 4}, %% command, command session, data form, state
			{cancel_command, 3} %% command, command session, state
		];
behaviour_info(_Other) -> undefined.


%%
%% API Functions
%%


%% Here we handle adhoc commands
%%
%% Adhoc Command Execution
handle(#received_packet{from = From, packet_type = iq, raw_packet = IQ}, State, AdhocModule) ->
	handle2({Acc, Domain, Resource} = From, exmpp_iq:xmlel_to_iq(IQ), State, AdhocModule);

handle(_, _, _) ->
	ok.


handle2({Acc, Domain, Resource}, 
					#iq{kind = request, type = set,  ns = ?NS_ADHOC,
							payload = #xmlel{name = 'command', attrs = Attrs}
						 } = IQ, 
					#client_state{session = Session} = State, AdhocModule) ->
		Payload = IQ#iq.payload,
		Action = exmpp_xml:get_attribute_from_list(Attrs, action, <<"execute">>),
		Command = exmpp_xml:get_attribute_from_list(Attrs, node, nil),
		Command_Session = exmpp_xml:get_attribute_from_list(Attrs, sessionid, new),
		DataForm = exmpp_xml:get_element(Payload, 'x'),

case Action of
		X when X == <<"execute">> orelse X == <<"complete">> orelse X == <<"next">> ->
				io:format("Execution of command " ++ binary_to_list(Command) ++ " is required.~n"),
				#command_result{result = ResultDataForm, status = Status, sessionid = Sessionid}
						= AdhocModule:execute_command(Command, Command_Session, DataForm, State),
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
				AdhocModule:cancel_command(Command, Command_Session, State),
								%% Construct response
				Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, exmpp_xml:set_attributes(Payload, [{status, canceled}]))
							),
				io:format("Sending command result...~n~p~n", [exmpp_xml:document_to_list(Result)]),
				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource)))

end,
ok;



%% Adhoc Commands list request
handle2({Acc, Domain, Resource},  #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS,
																 payload = #xmlel{name = 'query', attrs = [#xmlattr{name = node, value = ?NS_ADHOC_b}]}} = IQ,
					#client_state{jid = JID, session = Session} = State, AdhocModule) ->
		io:format("List of ad hoc commands is requested. ~n"),
		%% Construct response
		Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, commands_to_xml(AdhocModule:get_adhoc_list(State), exmpp_jid:to_list(JID)))
							),
		gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
		ok;

handle2(_From,  _Packet, _State, _AdhocModule) ->
	ok.




%%
%% Local Functions
%%
commands_to_xml(Commands, JID) ->
		#xmlel{name = 'query', attrs = [#xmlattr{name = node, value = ?NS_ADHOC_b}],
					 children = lists:map(fun(#command{id = Id, name = Name}) ->
										#xmlel{name = "item",
													 attrs = [
																		#xmlattr{name = node, value = Id},
																		#xmlattr{name = name, value = Name},
																		#xmlattr{name = jid, value = list_to_binary(JID)}
																	 ]
													 } end, Commands)
					 }.

