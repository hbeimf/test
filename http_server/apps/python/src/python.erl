-module(python).

-behavior(python_port).

% -export([start_link/0, echo/1]).

% -export([init/1]).

-compile(export_all).

start_link() ->
    python_port:start_link(?MODULE, [], [registered]).

init([]) ->
    DirPy = code:lib_dir(python, priv) ++ "/py/py_port",

    % {ok, [{module, "echo"}]}.
    {ok, [{module, DirPy}]}.


echo() ->
    Msg = <<"hello world!!">>,
    % lager:error("error message hello world"),
    % lager:warning("warning message hello world"),

    echo(Msg).

echo(Msg) ->
    % lager:error("Some message"),
    % lager:warning("Some message with a term: ~p", [Msg]),

    python_port:call_port(?MODULE, {echo, Msg}).

add(X, Y) ->
    python_port:call_port(?MODULE, {add, X, Y}).

curl() ->
    Link = <<"http://www.baidu.com">>,
    Reply = python_port:call_port(?MODULE, {curl, Link}),

    Dir = code:lib_dir(workboy, priv) ++ "/baidu.html",
    lib_fun:file_put_contents(Dir, lib_fun:to_str(Reply)),
    ok.
    % io:format("~p~n~n", [Reply]).



