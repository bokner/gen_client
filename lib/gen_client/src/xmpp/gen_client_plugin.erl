%%
-module(gen_client_plugin).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([behaviour_info/1]).


% gen_client_plugin behaviour
% Return a list of required functions and their arity
behaviour_info(callbacks) ->
	[
	 {init, 1}, %% Init plugin; argument list.
	 {terminate, 1}, %% Terminate plugin; plugin reference (module instance or PID).
	 {handle, 3}	%% Handle stanza; received packet, client PID and plugin reference (module instance or PID).	
	
	];
behaviour_info(_Other) -> undefined.
%%
%% API Functions
%%



%%
%% Local Functions
%%

