%%%----------------------------------------------------------------
%%% @author  Boris Okner <boris.okner@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2010 Boris Okner
%%%----------------------------------------------------------------
-module(gen_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

	StartMod = gen_client,
	StartFunc = {gen_server, start_link, [StartMod]},
	ChildSpec = {StartMod, StartFunc, temporary, 4000, worker, [StartMod]},
	{ok, {{simple_one_for_one, 0, 1}, [ChildSpec]}}.  


%%%===================================================================
%%% Internal functions
%%%===================================================================


