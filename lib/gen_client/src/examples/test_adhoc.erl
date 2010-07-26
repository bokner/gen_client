%% Created: July 23, 2010
%% Description: Example of plugin implementation.
%% The code implements adhoc_plugin behaviour,
%% which enables user-defined adhoc commands (XEP-0050)

-module(test_adhoc).

-behaviour(disco_plugin).
%%
%% Include files
%%
-include("gen_client.hrl").
%%
%% Exported Functions
%%
-export([commands/2]).

%%
%% API Functions
%%
commands(_Requester, _Arguments) ->
		[
		 #command{id = <<"env_info_command">>, name = <<"Environment Info">>, 
							handler = env_info_command},
		 #command{id = <<"shell_command">>, name = <<"Run shell commands">>, 
							handler = shell_command}
		].	


%%
%% Local Functions
%%

