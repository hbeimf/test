%%%-------------------------------------------------------------------
%% @doc python top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(python_sup).

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
    Element = {taskboy_server, {taskboy_server, start_link, []},
               temporary, brutal_kill, worker, [taskboy_server]},

    Python = {python_port, {python, start_link, []},
               permanent, 5000, worker, [python]},


    Children = [Element, Python],

    {ok, { {one_for_all, 10, 10}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
