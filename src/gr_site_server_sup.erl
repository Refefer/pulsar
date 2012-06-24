-module(gr_site_server_sup).
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
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} -> 
            erlang:unlink(Pid),
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {already_started, Pid}
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Server = ?CHILD(gr_site_server, worker),
    {ok, { { simple_one_for_one, 5, 10}, [Server]} }.

