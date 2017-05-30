%%%-------------------------------------------------------------------
%% @doc go public API
%% @end
%%%-------------------------------------------------------------------

-module(go).
-compile(export_all).

%%====================================================================
%% Call Api functions
%% 同步消息发送
%%====================================================================
% info() ->
%     go_name_server:info().

info() ->
    Call = info,
    call(Call).

parse_list(List) ->
    parse_list(List, 0.1).

parse_list(List, Add) ->
    Call = {list, List, Add},
    call(Call).

% From = 'gb2312',
% To = 'utf-8',
iconv(Str, From, To) ->
    Call = {iconv, Str, From, To},
    {ok, ReplyStr} = call(Call),
    ReplyStr.

str_replace() ->
    str_replace("hello world!!", "e", "XX").
str_replace(StrRes, FindStr, ReplaceTo) ->
    Call = {str, str_replace, StrRes, FindStr, ReplaceTo},
    call(Call).

% str(Str) ->
%     Call = {str, Str},
%     call(Call).

call(Call) ->
    GoMBox = go_name_server:get_gombox(),
    gen_server:call(GoMBox, Call).

%%====================================================================
%% Cast Api functions
%% 异步消息发送　
%%====================================================================
cast() ->
    List = [{name, "xiaomin"}, {email, "123456@qq.com"}],
    Cast = {list, List},
    send_cast(Cast).


send_cast(Cast) ->
    GoMBox = go_name_server:get_gombox(),
    go_name_server:send_cast(GoMBox, Cast),
    ok.


t() ->
    lists:foreach(fun(I) ->
        GoMBox = go_name_server:get_gombox(),
        io:format("~p ~p　~n", [I, GoMBox])
    end, lists:seq(1, 100)).



%%====================================================================
%% Internal functions
%%====================================================================
% default_go_mailbox() ->
%     {ok, GoMBox} = application:get_env(go, go_mailbox),
%     GoMBox.

% start_goroutine() ->
%     go_server:start_goroutine().

% stop({ServerName, _Host} = GoMBox) ->
%     case exists(ServerName) of
%         true ->
%             go_server:stop_goroutine(GoMBox);
%         _ ->
%             ok
%     end.

% stop_by_name(ServerName) ->
%     {_, Host} = go_name_server:get_gombox(),
%     GoMBox = {ServerName, Host},
%     stop(GoMBox).

% stop() ->
%     {_, _, {_, _, Goroutines}, _} = info(),
%     lists:foreach(fun(Goroutine) ->
%         stop_by_name(Goroutine)
%     end, Goroutines).

% exists(ServerName) when is_atom(ServerName) ->
%     {_, _, {_,_, Goroutines}, _} = info(),
%     case lists:member(ServerName, Goroutines) of
%         true ->
%             true;
%         _ ->
%             false
%     end;
% exists(_ServerName) ->
%     false.


%%====================================================================
%% Call Api functions
%% erlang 实现的函数
%%====================================================================

http_get(Url) ->
    case httpc:request(get, {Url, []},
                        [{autoredirect, true},
                         {timeout, 60000},
                         {version, "HTTP/1.1"}],
                        [{body_format, binary}]) of
            {ok, {_,_, Body}}->
                Body;
            {error, _Reason} ->
                <<"">>
    end.

% 用在rebar3发布的配置文件中
apps() ->
    Apps = application:which_applications(),

    AppList = lists:foldl(fun({App, _, _}, ReleaseAppList) ->
        [App|ReleaseAppList]
    end, [], Apps),

    io:format("~n~p~n~n", [AppList]),
    ok.


%%====================================================================
%% call demos
%%====================================================================

% echo_zh() ->
%     Utf8Name = unicode:characters_to_binary("xiaomin小明"),
%     io:format("~ts~n", [Utf8Name]).

% d() ->
%     demo(),
%     wrong(),
%     iconv(),
%     parse_list(),
%     ok.


% ss() ->
%     lists:foreach(fun(X)->
%         GoMBox = start_goroutine(),
%         io:format("~p: ~p~n", [X, GoMBox])
%     end, lists:seq(1, 100)).

% demo() ->
%     % {ok, GoMBox} = application:get_env(go, go_mailbox),
%     GoMBox = start_goroutine(),

%     Utf8Name = unicode:characters_to_binary("xiaomin小明"),
%     % Json = jsx:encode(#{name=>Utf8Name, email=><<"123456@gmail.com">>, age=> 1}),
%     Json = jsx:encode(#{name=>Utf8Name, email=><<"123456@gmail.com">>, age=> 1, list=>[1,2,3,5]}),

%     Call = {demo, Json},
%     Reply = gen_server:call(GoMBox, Call),

%     stop(GoMBox),

%     io:format("~p ~n", [Reply]),

%     Reply.

% wrong() ->
%     GoMBox = start_goroutine(),

%     Utf8Name = unicode:characters_to_binary("xiaomin小明"),
%     Json = jsx:encode(#{name=>Utf8Name, email=><<"123456@gmail.com">>, age=> 1}),

%     Call = {wrong, Json},
%     Reply = gen_server:call(GoMBox, Call),
%     stop(GoMBox),
%     io:format("~p ~n", [Reply]),
%     Reply.


% iconv() ->
%     % gb2312 的网页
%     Url = "http://down.chinaz.com/soft/34159.htm",
%     % Url = "https://www.baidu.com/",

%     DirGoPriv = code:lib_dir(go, priv),
%     Html = http_get(Url),
%     Dir = DirGoPriv ++ "/gb2312.html",
%     file:write_file(Dir, Html),

%     From = 'gb2312',
%     To = 'utf-8',
%     Html2 = iconv(Html, From, To),

%     Dir1 = DirGoPriv ++ "/utf-8.html",
%     file:write_file(Dir1, Html2),

%     ok.
