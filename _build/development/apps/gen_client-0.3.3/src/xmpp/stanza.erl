%% Author: bokner
%% Created: Jan 23, 2010
%% Description: TODO: Add description to stanza
-module(stanza).

%%
%% Include files
%%
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-define(PUBSUB(NS, Children), (
  #xmlel{ns = NS, name = 'pubsub', children = Children}
)).

%%
%% Exported Functions
%%
-export([register/2, 
				 add_user/2, get_all_users/1, get_user_password/1,
				 create_pubsub_node/4,
				 available/1, unavailable/0, directed_presence/1,
				 subscribe/1, subscribed/1,
				 publish_with_itemid/4,
				 discover_pubsub_nodes/2,
				 retrieve_pubsub_item/3,
				 get_pubsub_subscriptions/1,
				 adhoc_command/3, adhoc_command/4
				]).
%%
%% API Functions
%%



%% Create pubsub node 
%% Type  = collection | leaf
%% Fields is a list of tuples {field_name, field_value}, 
%% field_name and field_value are strings.
create_pubsub_node(PubSub, NodeName, Type, Fields) when is_atom(Type) ->
	[Packet] = 	exmpp_xml:parse_document(
							io_lib:format(
	"<iq type='set' to='~s'>
  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
    <create node='~s'/>
    <configure>
      <x xmlns='jabber:x:data' type='submit'>
        <field var='FORM_TYPE' type='hidden'>
          <value>http://jabber.org/protocol/pubsub#node_config</value>
        </field>
        <field var='pubsub#node_type'><value>~s</value></field>
				~s
      </x>
    </configure>
  </pubsub>
	</iq>", [PubSub, NodeName, atom_to_list(Type), field_list(Fields)])),
	Packet.

%% Discover pubsub child nodes
discover_pubsub_nodes(PubSub, top) ->
		[Packet] = exmpp_xml:parse_document(
								 io_lib:format(
"<iq type='get'
    to='~s'>
  <query xmlns='http://jabber.org/protocol/disco#items'/>
</iq>", [PubSub]																			
		)
	),
	Packet;	

discover_pubsub_nodes(PubSub, Node) ->
		[Packet] = exmpp_xml:parse_document(
								 io_lib:format(
"<iq type='get'
    to='~s'>
  <query xmlns='http://jabber.org/protocol/disco#items' node='~s'/>

</iq>", [PubSub, Node]																			
		)
	),
	Packet.


retrieve_pubsub_item(PubSub, Node, Item) ->
		[Packet] = exmpp_xml:parse_document(
								 io_lib:format(
"<iq type='get'
    to='~s'>
  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
    <items node='~s'>
      <item id='~s'/>
    </items>
  </pubsub>
</iq>
", [PubSub, Node, Item]																			
		)
	),
	Packet.

available(Status) ->

				exmpp_presence:set_status(
				exmpp_presence:available(), Status).

unavailable() ->
		exmpp_presence:unavailable().


subscribe(To) ->
		exmpp_stanza:set_recipient(
														 exmpp_presence:subscribe(), To).

subscribed(To) ->
		exmpp_stanza:set_recipient(
														 exmpp_presence:subscribed(), To).

directed_presence(To) ->
		exmpp_stanza:set_recipient(
								exmpp_presence:available(), To).

publish_with_itemid(Service, Node, Item, ItemID) ->
    %% Prepare item.
    Items = #xmlel{ns = ?NS_PUBSUB, name = 'item',
									 attrs = [
																		#xmlattr{name = id, value = list_to_binary(ItemID)}
														],
		  children = [Item]},
    %% Make the <publish/> element.
    Publish = exmpp_xml:set_attributes(
		#xmlel{ns = ?NS_PUBSUB, name = 'publish',
		       children = [Items]},
		[{'node', Node}]),
    %% Prepare the final <iq/>.
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Publish]),
		Iq = exmpp_xml:set_attributes(
    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'},
      [{'type', "set"}, {'to', Service}, 
			 {'id', "pubsub-" ++ integer_to_list(random:uniform(65536 * 65536))}]),
		exmpp_xml:append_child(Iq, Pubsub).

register(Username, Password) ->
		exmpp_client_register:register_account([{username, Username},
							{password, Password}]).

%% XEP-0133, "Add User" command.
%% This is a shortcut; we skip 1st step asking for data form, assume it's how it comes from ejabberd.
%% TODO: version with field list.
%% NOTE: ACL rule for "configure" is needed, otherwise command won't be authorized.
%% That's how to do it (ejabberd.cfg fragment follows):
%%
%% 		......
%% 		{acl, vroc_admin, {user, "vrocbot", "vroc.local"}}.
%%		......
%%		{access, configure, [{allow, admin}, {allow, vroc_admin}]}.
%%		......
%%
%%
add_user(Jid, Password) when ?IS_JID(Jid) ->
		Domain = exmpp_jid:domain_as_list(Jid),
		JidStr = exmpp_jid:to_list(Jid),
	[Packet] = 	exmpp_xml:parse_document(
							io_lib:format("<iq type='set' to='~s'  >
	<command xmlns='http://jabber.org/protocol/commands' node='http://jabber.org/protocol/admin#add-user'  >
	<x xmlns='jabber:x:data' type='submit' >
	<field type='hidden' var='FORM_TYPE' >
	<value>http://jabber.org/protocol/admin</value>
	</field>
	<field type='jid-single' var='accountjid' >
	<value>~s</value>
	</field>
	<field type='text-private' var='password' >
	<value>~s</value>
	</field>
	<field type='text-private' var='password-verify' >
	<value>~s</value>
	</field>
	</x>
	</command>
	</iq>", [Domain, JidStr, Password, Password])),
		Packet.

%% Note - this is ejabberd specific, may not work with other servers
get_all_users(Domain) ->
	exmpp_client_disco:items(Domain, "all users").

get_pubsub_subscriptions(PubSub) ->
		#xmlel{name ='iq', 									 
										attrs = [
																		#xmlattr{name = 'type', value = <<"get">>},
																		#xmlattr{name = 'to', value = list_to_binary(PubSub)}
														],
		  children = [
									#xmlel{name = 'pubsub',
												 ns = ?NS_PUBSUB,
												 children = [
																		 #xmlel{name = 'subscriptions'}
																		 ]
												 }
									]}.


%% Get User Password
%% XEP-0133, ejabberd impelementation
%% Starts with sending data form with jid filled out.
%%
get_user_password(Jid) when ?IS_JID(Jid) ->
			Domain = exmpp_jid:domain_as_list(Jid),
		JidStr = exmpp_jid:to_list(Jid),
	[Packet] = 	exmpp_xml:parse_document(
							io_lib:format(
	"<iq type='set' to='~s'>
<command xmlns='http://jabber.org/protocol/commands' node='http://jabber.org/protocol/admin#get-user-password' >
<x xmlns='jabber:x:data' type='submit' >
<field type='hidden' var='FORM_TYPE' >
<value>http://jabber.org/protocol/admin</value>
</field>
<field type='jid-single' var='accountjid' >
<value>~s</value>
</field>
</x>
</command>
</iq>", [Domain, JidStr])),
		Packet;

get_user_password(Jid) ->
	get_user_password(exmpp_jid:parse(Jid)).

adhoc_command(CommandNode, Action, FormItems) ->
	adhoc_command(CommandNode, Action, new, FormItems).
	
adhoc_command(CommandNode, Action, SessionId, FormItems) ->
		#xmlel{name ='iq', 									 
										attrs = [
																		#xmlattr{name = 'type', value = <<"set">>}
														],
		  children = [
									#xmlel{name = 'command',
												 ns = ?NS_ADHOC,
												 attrs = [
																		#xmlattr{name = 'node', value = exmpp_utils:any_to_binary(CommandNode)},
																		#xmlattr{name = 'action', value = exmpp_utils:any_to_binary(Action)},
																		#xmlattr{name = 'sessionid', value = exmpp_utils:any_to_binary(SessionId)}																																					
																	],
												 children = [
																		 adhoc_processor:fields_to_dataform(FormItems)
																		 ]
												 }
									]}.
	

%%
%% Local Functions
%%
field_list(Fields) ->
		lists:map(fun({Fieldname, Value}) -> io_lib:format("<field var='~s'><value>~s</value></field>", [Fieldname, Value]) end, Fields).


