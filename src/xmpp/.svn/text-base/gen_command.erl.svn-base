%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%% Adhoc command behaviour
%%% @end
%%% Created : 27 Oct 2009 by Boris Okner <boris.okner@gmail.com>

-module(gen_command).

-export([behaviour_info/1]).

% gen_command behaviour
% Return a list of required functions and their arity
behaviour_info(callbacks) ->
		[
		 {new_session_process, 1},
		 {execute, 3},
		 {cancel, 1},
		 {to_dataform, 1}
		];
behaviour_info(_Other) -> undefined.
