%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%% "Environment info" adhoc command implementation
%%% @end
%%% Created : 25 Oct 2009 by Boris Okner <boris.okner@gmail.com>

-module(env_info_command).

-behaviour(gen_command).

-export([new_session_process/2, execute/4, cancel/3]).


-include_lib("exmpp/include/exmpp_nss.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_xmpp.hrl").

-include("ad_hoc.hrl").

% No process is created for handling the command
new_session_process(_AdhocModuleParams, _ClientState) ->
		none.

% This is a one-shot command, so there is no session tracking and no incoming data.
execute(_Session, _AdhocModuleParams, _ClientState, _DataForm) ->
		{ok, Hostname} = inet:gethostname(),
		{ok, {P1, P2, P3, P4}} = inet:getaddr(Hostname, inet),
		{OS_Family, OS_Name} = os:type(),
		{OS_Major, OS_Minor, OS_Release} = os:version(),


		DataResult = [
						{<<"host">>, <<"Host">>, Hostname},
						{<<"ip">>, <<"IP Address">>, integer_to_list(P1) ++ "." ++ integer_to_list(P2) ++ "." ++ integer_to_list(P3) ++ "." ++ integer_to_list(P4)},
						{<<"os_family">>, <<"OS family">>, atom_to_list(OS_Family)},
						{<<"os_name">>, <<"OS name">>, atom_to_list(OS_Name)},
						{<<"os_major">>, <<"Major OS version">>, integer_to_list(OS_Major)},
						{<<"os_minor">>, <<"Minor OS version">>, integer_to_list(OS_Minor)},
						{<<"os_release">>, <<"OS release">>, integer_to_list(OS_Release)}
					],
		#command_result{result = to_dataform(DataResult), status = completed}.


cancel(_Session, _AdhocModuleParams, _ClientState) ->
		ok.

to_dataform(Data) ->
		#xmlel{name = 'x', ns = 'jabber:x:data', attrs = [#xmlattr{name = type, value = <<"result">>}],
					 children = [
											 #xmlel{name = 'title', children = [#xmlcdata{cdata = <<"Environment Info">>}]} |
											 lists:map(fun({Var, Label, Value}) ->
																																					#xmlel{name = 'field', attrs = [
																																																					#xmlattr{name = var, value = Var},
																																																					#xmlattr{name = label, value = Label},
																																																					#xmlattr{name = type, value = <<"text-single">>}
																																																				 ],
																																								 children = [#xmlel{name = 'value', children = [#xmlcdata{cdata = Value}]}]
																																								 } end, Data)
											]
					 }.
