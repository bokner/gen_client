%%% @author Boris Okner <boris.okner@gmail.com>
%%% @copyright (C) 2009, Boris Okner
%%% @doc
%%% Generic stanza handler behaviour
%%% @end
%%% Created : 27 Oct 2009 by Boris Okner <boris.okner@gmail.com>

-module(gen_handler).

-export([behaviour_info/1]).

% gen_command behaviour
% Return a list of required functions and their arity
behaviour_info(callbacks) ->
		[
			{handle, 3} %% Received, State, HandlerParams
		];
behaviour_info(_Other) -> undefined.