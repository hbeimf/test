%%%-------------------------------------------------------------------
%% @doc go top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(go_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
% init([]) ->
%     {ok, { {one_for_all, 0, 1}, []} }.

init([]) ->
    Go = {go_server, {go_server, start_link, []},
               permanent, 5000, worker, [go_server]},

    Children = [Go],

    {ok, { {one_for_all, 10, 10}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
