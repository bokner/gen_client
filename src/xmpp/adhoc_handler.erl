%% Author: bokner
%% Created: Jan 16, 2010
%% Description: gen_client with ad-hoc (XEP-0050) capabilities
-module(adhoc_handler).

-behaviour(gen_client).

%%
%% Exported Functions
%%

%% gen_client functions
-export([run/2, terminate/1, handle_iq/5, handle_presence/5, handle_message/5, handle_feed/2]).

-export([behaviour_info/1]).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("include/gen_client.hrl").
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

%% Pass-through functions; we delegate to the module everything except adhoc command handling
run(#client_state{module = Module} = State, Args) ->
		Module:run(State, Args).

terminate(#client_state{module = Module} = State) ->
	Module:terminate(State).

handle_message(Type, From, Id, Packet, #client_state{module = Module} = State) ->
	Module:handle_message(Type, From, Id, Packet, #client_state{module = Module} = State).


handle_presence(Type, From, Id, Packet, #client_state{module = Module} = State) ->
	Module:handle_presence(Type, From, Id, Packet, #client_state{module = Module} = State).


handle_feed(Feed, #client_state{module = Module} = State) ->
	Module:handle_feed(Feed, #client_state{module = Module} = State).


%% Here we handle adhoc commands
%%
%% Adhoc Command Execution
handle_iq(_Type, {Acc, Domain, Resource} = _From, _Id, 
					#iq{kind = request, type = set,  ns = ?NS_ADHOC,
							payload = #xmlel{name = 'command', attrs = Attrs}
						 } = IQ, 
					#client_state{module_state = ModuleState, module = Module, session = Session} = State) ->
		Payload = IQ#iq.payload,
		Action = exmpp_xml:get_attribute_from_list(Attrs, action, <<"execute">>),
		Command = exmpp_xml:get_attribute_from_list(Attrs, node, nil),
		Command_Session = exmpp_xml:get_attribute_from_list(Attrs, sessionid, new),
		DataForm = exmpp_xml:get_element(Payload, 'x'),

case Action of
		X when X == <<"execute">> orelse X == <<"complete">> orelse X == <<"next">> ->
				io:format("Execution of command " ++ binary_to_list(Command) ++ " is required.~n"),
				#command_result{result = ResultDataForm, status = Status, sessionid = Sessionid}
						= Module:execute_command(Command, Command_Session, DataForm, State),
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
				Module:cancel_command(Command, Command_Session, State),
								%% Construct response
				Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, exmpp_xml:set_attributes(Payload, [{status, canceled}]))
							),
				io:format("Sending command result...~n~p~n", [exmpp_xml:document_to_list(Result)]),
				gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource)))

end,
{ok, ModuleState};



%% Adhoc Commands list request
handle_iq(_Type, {Acc, Domain, Resource} = _From, _Id, #iq{kind = request, type = get,  ns = ?NS_DISCO_ITEMS,
																 payload = #xmlel{name = 'query', attrs = [#xmlattr{name = node, value = ?NS_ADHOC_b}]}} = IQ,
					#client_state{jid = JID, module = Module, module_state = ModuleState, session = Session} = State) ->
		io:format("List of ad hoc commands is requested. ~n"),
		%% Construct response
		Result = exmpp_iq:iq_to_xmlel(
							 exmpp_iq:result(IQ, commands_to_xml(Module:get_adhoc_list(State), exmpp_jid:to_list(JID)))
							),
		gen_client:send_packet(Session, exmpp_stanza:set_recipient(Result, exmpp_jid:make(Acc, Domain, Resource))),
		{ok, ModuleState};

handle_iq(_Type, _From, _Id, _Packet, State) ->
		{ok, State#client_state.module_state}.




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

