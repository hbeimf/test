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
parse_html() ->
    Html = "html",
    parse_html(Html).
parse_html(Html) ->
    Call = {str, parse_html, Html},
    call(Call).

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
    Call = {str, str_replace, lib_fun:to_str(StrRes), FindStr, ReplaceTo},
    {ok, NewString} = call(Call),
    NewString.

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

