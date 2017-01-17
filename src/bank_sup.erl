-module(bank_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Restart = permanent,
    Shutdown = 2000,

    Bank_Server = {bank_server , {bank_server , start_link , []},
		 Restart , Shutdown , worker , [bank_server]},
    {ok, { {one_for_one, 5, 10}, [Bank_Server]} }.

