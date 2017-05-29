-module(lib_fun).
-compile(export_all).
-include("config.hrl").

save_upload_file(FileInfoList) ->
	{_, FileName} = lists:keyfind(filename, 1, FileInfoList),
	{_, Content} = lists:keyfind(filecontent, 1, FileInfoList),
	Md5 = md5(Content),
	Array = explode(to_str(FileName), "."),
	ArrayLast = lists:last(Array),
	NewName = Md5 ++ "." ++ ArrayLast,
	case code:priv_dir(http_serv) of
		{error,bad_name} ->
			Dir = root_dir() ++ "deps/views/priv/upload/";
		_ ->
			Dir = code:priv_dir(views) ++ "/upload/" 
	end,
	{{Year, Month, Day}, {_Hour, _Min, _Sec}} = calendar:local_time(),
	DirYearMonth = Dir ++ to_str(Year) ++ to_str(Month),
	file:make_dir(DirYearMonth),
	DirDay = DirYearMonth ++ "/" ++ to_str(Day),
	file:make_dir(DirDay),
	Target = DirDay ++ "/" ++ NewName,
	file:write_file(Target, Content),
	DirArray = explode(Target, "priv"),
	[{flg, ok}, 
	 {data, [{md5, to_binary(Md5)}, 
	 		 {dir, to_binary(lists:last(DirArray))},
	         {size, byte_size(Content)}]
	}].

page(PageList) ->
	case lists:keyfind(<<"table_name">>, 1, PageList) of
		{_, TableNameStr} ->
			TableName = to_str(TableNameStr);
		_ ->
			TableName = "notable"
	end,
	case lists:keyfind(<<"current_page">>, 1, PageList) of
		{_, undefined} ->
			CurrentPage = 1;
		{_, CurrentPageStr} ->
			CurrentPage = to_integer(CurrentPageStr);
		_ ->
			CurrentPage = 1
	end,
	case lists:keyfind(<<"action">>, 1, PageList) of
		{_, ActionStr} ->
			Action = to_str(ActionStr);
		_ ->
			Action = "noaction"
	end,
	case lists:keyfind(<<"where">>, 1, PageList) of
		{_, WhereStr} ->
			Where = to_str(WhereStr);
		_ ->
			Where = " 1=1 "
	end,
	
	case lists:keyfind(<<"page_size">>, 1, PageList) of
		{_, PageSizeStr} ->
			PageSize = to_integer(PageSizeStr);
		_ ->
			PageSize = 10
	end,
	case lists:keyfind(<<"show_size">>, 1, PageList) of
		{_, ShowSizeStr} ->
			ShowSize = to_integer(ShowSizeStr);
		_ ->
			ShowSize = 10
	end,

	Sql = "select count(*) as a from " ++ TableName ++ " where " ++ Where,
	[Res] = get_assoc(Sql),
	{_, All} = lists:keyfind(<<"a">>, 1, Res),

	AllPage = ceil(All / PageSize),

	case CurrentPage < 1 of
		true ->
			CPage = 1;
		_ ->
			case CurrentPage > AllPage of
				true ->
					CPage = AllPage;
				_->
					CPage = CurrentPage
			end
	end,

	Start = (CPage - 1) * PageSize,

	case lists:keyfind(<<"sql">>, 1, PageList) of
		{_, SelectSql} ->
			SqlData = to_str(SelectSql) ++ " limit " ++ to_str(Start) ++ "," ++ to_str(PageSize);
		_ ->
			SqlData = "select * from " ++ TableName ++ " where " ++ Where ++ " limit " ++ to_str(Start) ++ "," ++ to_str(PageSize)
	end,

	%SqlData = "select * from " ++ TableName ++ " where " ++ Where ++ " limit " ++ to_str(Start) ++ "," ++ to_str(PageSize),
	ResultList = get_assoc(SqlData),

	case ShowSize < AllPage of
		true ->
			Loop = ShowSize,
			case (CPage + ceil(ShowSize / 2)) > AllPage of
				true ->
					StartShow = AllPage - ShowSize + 1;
				_ ->
					case ( CPage - ceil(ShowSize /2)) < 1 of
						true ->
							StartShow = 1;
						_ ->
							StartShow = CPage - ceil( ShowSize / 2 )
					end
			end;
		_ ->
			Loop = AllPage,
			StartShow = 1
	end,

	PageResult = "共" ++ to_str(All) ++ "条记录",
	HeadUrl = "<span><a href=\"" ++ to_str(Action) ++ "&page=1\">首页</a></span>",
	MidList = iolist_to_binary([page_url(N, CPage, StartShow, Action) || N <- lists:seq(1, Loop)]),
	LastUrl = "<span><a href=\"" ++ to_str(Action) ++"&page="++ to_str(AllPage) ++"\">尾页</a></span>",

	Url = to_str(iolist_to_binary([PageResult, HeadUrl, MidList, LastUrl])),

	[{data, ResultList}, {page, Url}].

page_url(N, CurrentPage, StartShow, Action) ->
	Page = N + StartShow - 1,
	case (Page == CurrentPage) of
		true ->
			Url = "<span class=\"current\">"++ to_str(Page) ++"</span>";
		_ ->
			Url = "<span><a href=\""++ Action ++"&page="++ to_str(Page) ++"\">"++ to_str(Page) ++"</a></span>"
	end,
	Url.

ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.


get_body(TemplateName, DataList, Handler) ->
	%TemplateDir = code:priv_dir(http_serv) ++ "/" ++ TemplateName,
	%TemplateDir = http_serv_fun:priv_dir() ++ TemplateName,
	case code:priv_dir(http_serv) of
		{error,bad_name} ->
			TemplateDir = lib_fun:priv_dir() ++ TemplateName;
		Dir ->
			TemplateDir = Dir ++ "/" ++ TemplateName
	end,
	erlydtl:compile( TemplateDir, Handler, [ 
		{out_dir, false},
	 	{custom_filters_modules, [lib_filter]},
	 	{custom_tags_modules, [lib_tags]}
	]),
	{ok, List} = Handler:render(DataList),
	iolist_to_binary(List).

%% -----------------------------------
get_redis_host() ->
	?REDIS_HOST.

get_redis_port() ->
	?REDIS_PORT.

%% lib_redis
connect_redis() ->
	log("redis connect ..."),
	{ok, ClientPid} = erldis:connect(?REDIS_HOST, ?REDIS_PORT),
	ClientPid.

close(ClientPid) ->
	erldis:quit(ClientPid).

flushAll(ClientPid) ->
	erldis:flushdb(ClientPid).

delete(ClientPid, Key) ->
	erldis:del(ClientPid, Key).

set(ClientPid, Key, Val) ->
	erldis:set(ClientPid, Key, Val).

get(ClientPid, Key) ->
	erldis:get(ClientPid, Key).

exists(ClientPid, Key) ->
	erldis:exists(ClientPid, Key).

%% lib_emysql
get_assoc(Sql) ->
	{_, _, Field, Data, _} = query_sql(Sql),
	FieldList = lists:map(fun({_F1, _F2, _F3, _F4, _F5, _F6, FieldName, _F8, _F9, _F10, _F11, _F12, _F13, _F14, _F15}) -> 
						 		FieldName
							end, Field),
	DataList = lists:map(fun(L) -> 
						 	lists:zip(FieldList, L)
						 end, Data), 
	DataList.

update(TableName, List, Where) ->
	query_sql(update_sql(TableName, List, Where)).

update_sql(TableName, List, Where) ->
	SetList = lists:map(fun({Key, Val}) -> 
				lists:concat(["`", to_str(Key), "` = '", replace(to_str(Val), "'", "\\'"), "'"])	
	end, List),
	Set = string:join(SetList, ", "),
	lists:concat(["UPDATE `", TableName, "`", " SET ", Set, " WHERE ", Where]).
	
insert(TableName, List) ->
	query_sql(insert_sql(TableName, List)).

insert_sql(TableName, List) ->
	{FieldList, DataList} = lists:unzip(List),
	%%
	FilterFieldList = lists:map(fun(Key) -> 
		to_str(Key)
	end, FieldList),
	FieldStr = string:join(FilterFieldList, "`, `"),
	%FieldStr = string:join(FieldList, "`, `"),
	DataList1 = lists:map(fun(F) -> 
		FF = replace(to_str(F), "'", "\\'"),
		lists:concat(["'", FF, "'"])
	end, DataList),
	DataStr = string:join(DataList1, ", "),
	lists:concat(["INSERT INTO `", TableName, "` (`", FieldStr, "`) VALUES (", DataStr, ")"]).

query_sql(Sql) ->
	try
		emysql:execute(?DB, to_binary(Sql))
	catch
		_Type:_What ->
			log("mysql connect ..."),
			init_emysql(),
			emysql:execute(?DB, list_to_binary(Sql))	
	end.

init_emysql() ->
	emysql:add_pool(?DB, 1, ?DB_USER_NAME, ?DB_PASSWORD, ?DB_HOST, ?DB_PORT, ?DB_NAME, ?DB_ENCODE).

to_list(S) when is_integer(S) ->
	integer_to_list(S);
to_list(S) when is_float(S) ->
	float_to_list(S);
to_list(S) ->
	S.

%% lib_file
append(Dir, Data) ->
	case file_exists(Dir) of
		true ->
			file:write_file(Dir, "\n" ++ Data, [append]);
		_ ->
			file:write_file(Dir, Data, [append])
	end.

file_get_contents(Dir) ->
	case file:read_file(Dir) of
		{ok, Bin} ->
			{ok, binary_to_list(Bin)};
		{error, Msg} ->
			{error, Msg}
	end.

file_put_contents(Dir, Str) ->
	file:write_file(Dir, list_to_binary(Str)).

file_exists(Dir) ->
	case filelib:is_dir(Dir) of
		true ->
			false;
		false ->
			filelib:is_file(Dir)
	end.

is_dir(Dir) ->
	filelib:is_dir(Dir).

make_dir(Dir) ->
	file:make_dir(Dir).

del_dir(Dir) ->
	file:del_dir(Dir).

file_size(Dir) ->
	filelib:file_size(Dir).

%% lib_fun
is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    case lists:member(node(Pid), nodes()) of
		false ->
	   	 false;
		true ->
	    	case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
				true ->
		    		true;
				false ->
		    		false;
				{badrpc, _Reason} ->
		    		false
	    	end
    end.

to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_float(X) -> list_to_binary(float_to_list(X));
to_binary(X) -> term_to_binary(X).

to_str(X) when is_list(X) -> X;
to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_binary(X) -> binary_to_list(X);
to_str(X) when is_integer(X) -> integer_to_list(X);
to_str(X) when is_float(X) -> float_to_list(X).


to_integer(X) when is_list(X) -> list_to_integer(X);
to_integer(X) when is_binary(X) -> binary_to_integer(X);
to_integer(X) when is_integer(X) -> X;
to_integer(X) -> X.

% 时间转时间戳，格式：{{2013,11,13}, {18,0,0}}  
datetime_to_timestamp(DateTime) ->  
	calendar:datetime_to_gregorian_seconds(DateTime) -  calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) - 8 * 60 * 60.  

% 时间戳转时间  
timestamp_to_datetime(Timestamp) ->  
	calendar:gregorian_seconds_to_datetime(Timestamp +  calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).  


%% 获取时间截
time() ->
	DateTime = calendar:local_time(),
	datetime_to_timestamp(DateTime).

%% 返回随机数
random() ->
	{_, _, P3} = erlang:now(),
	to_str(random:uniform(P3)).

%%　返回日期
date_str("y-m-d") ->
	{{Year, Month, Day}, {_Hour, _Min, _Sec}} = calendar:local_time(),
	to_str(Year) ++ "-" ++ to_str(Month) ++ "-" ++ to_str(Day).

date_str() ->
	{{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
	to_str(Year) ++ "-" ++ to_str(Month) ++ "-" ++ to_str(Day)
	++ " " ++ to_str(Hour) ++ ":" ++ to_str(Min) ++ ":" ++ to_str(Sec).

%% 返回目录
root_dir() ->
	replace(os:cmd("pwd"), "\n", "/").
	
priv_dir() ->
	replace(os:cmd("pwd"), "\n", "/priv/").

%% lib_lists
%% pmap	
pmap(Fun, List) ->
	Pid = self(),
	Ref = erlang:make_ref(),
	Pids = lists:map(fun(I) -> 
				spawn(fun() -> do_pmap2(Pid, Ref, Fun, I) end)
			end, List),
	gathers(Pids, Ref).

%% Fun 处理回调函数
%% L 待处理的列表
%% LimitProcess 最多使用的进程数
pmap(Fun, L, LimitProcess) ->
	NewList = heap_split(LimitProcess, L),
	Pid = self(),
	Ref = erlang:make_ref(),
	Pids = lists:map(fun(HeapList) ->
				spawn(fun() -> do_pmap3(Pid, Ref, Fun, HeapList) end)
			end, NewList),
	lists:flatten(gathers(Pids, Ref)).

do_pmap2(Parent, Ref, Fun, I) ->
	Parent ! {self(), Ref, (catch(Fun(I)))}.	

do_pmap3(Parent, Ref, Fun, HeapList) ->
	ResultList = lists:map(fun(I)->
			catch(Fun(I))
		end, HeapList),
	Parent ! {self(), Ref, ResultList}.	

gathers([Pid|T], Ref) ->
	receive
		{Pid, Ref, Ret}->
			[Ret|gathers(T, Ref)]
	end;
gathers([], _) ->
	[].

%%
%%将 List 分成 N 个子 List
heap_split(N, List) ->
	Length = length(List),
	{ok,{{a,A,AR},{b,_B,BR},{v,_V}}} = heap(Length, N),
	AListLength = A * AR,
	%%BListLength = B * BR,
	{AList, BList} = lists:split(AListLength, List),
	SubAList = split(AR, AList),
	SubBList = split(BR, BList),
	SubAList ++ SubBList.

heap(A, N) ->
	case A >= N of
		true ->
			R = round(A / N),
			B = (1 + R) * N - A,
			C = A - N * R,

			AA = N*R -A,
			BB = (1-R) * N  + A,

			case ((B >=0) and (B =< N) and (C >=0) and (C =< N)) of
				true ->
					List = {ok, {{a, B, R}, {b, C, R+1}, {v, R}}};
				_ ->
					case ((AA>=0) and (AA =< N) and (BB>=0) and (BB=<N)) of
						true ->
							List = {ok, {{a, AA, R-1},{b, BB, R}, {v, R}}};
						_ ->
							%% default 以防万一
							List = {ok, {{a, 0, R-1},{b, N, R}, {v, R}}}
					end
			end;
		_ ->
			List = {ok, {{a, 0, 1},{b, A, 1}, {v, 1}}}
	end,
	List.

%% N 堆大小
%% List 为要分配的列表
split(N, List) ->
	split(N, List, []).

%% N 堆大小
%% List 为要分配的列表
split(_N, [], LRes) -> lists:reverse(LRes);
split(N, List, LRes) ->
	Length = length(List),
	case Length > N of
		true ->
			{List1, List2} = lists:split(N, List),
			split(N, List2, [List1|LRes]);
		_ ->
			split(N, [], [List|LRes])
	end.

%% lib_string
%%-compile(export_all).
%% http://sen228.blog.163.com/blog/static/1648623192012112113246157/

replace(Str, SubStr, NewStr) ->
	case string:str(Str, SubStr) of
		Pos when Pos == 0 ->
			Str;
		Pos when Pos == 1 ->
			Tail = string:substr(Str, string:len(SubStr) + 1),
			string:concat(NewStr, replace(Tail, SubStr, NewStr));
		Pos ->
			Head = string:substr(Str, 1, Pos - 1),
			Tail = string:substr(Str, Pos + string:len(SubStr)),
			string:concat(string:concat(Head, NewStr), replace(Tail, SubStr, NewStr))
	end.

explode(Str, SubStr) ->
	case string:len(Str) of
		Length when Length == 0 ->
			[];
		_Length ->
			explode(Str, SubStr, [])
	end.

explode(Str, SubStr, List) ->
	case string:str(Str, SubStr) of
		Pos when Pos == 0 ->
			List ++ [Str];
		Pos when Pos == 1 ->
			LengthStr = string:len(Str),
			LengthSubStr = string:len(SubStr),
			case LengthStr - LengthSubStr of
				Length when Length =< 0 ->
					List;
				Length ->
					LastStr = string:substr(Str, LengthSubStr + 1, Length),
					explode(LastStr, SubStr, List)
			end;
		Pos ->
			Head = string:substr(Str, 1, Pos -1),
			Tail = string:substr(Str, Pos),
			explode(Tail, SubStr, List ++ [Head])
	end.

implode(List, Str) ->
	string:join(List, Str).

trim(Str) ->
	string:strip(Str).

ltrim(Str) ->
	string:strip(Str, left).

rtrim(Str) ->
	string:strip(Str, right).

trim(Str, SubStr) ->
	LStr = ltrim(Str, SubStr),
	rtrim(LStr, SubStr).

rtrim(Str, SubStr) ->
	NewStr = trim(Str),
	NewSubStr = trim(SubStr),
	LengthNewStr = string:len(NewStr),
	case string:len(NewSubStr) of
		LengthNewSubStr when LengthNewSubStr == 0 ->
			NewStr;
		LengthNewSubStr ->
			case LengthNewStr - LengthNewSubStr of
				Length when Length < 0 ->
					NewStr;
				Length ->
					Head = string:substr(NewStr, Length + 1, LengthNewSubStr),
					case string:equal(Head, NewSubStr) of
						true ->		
							Tail = string:substr(NewStr, 1, Length),
							rtrim(Tail, SubStr);
						false ->
							NewStr
					end
			end
	end.

ltrim(Str, SubStr) ->
	NewStr = trim(Str),
	NewSubStr = trim(SubStr),
	LengthNewStr = string:len(NewStr),
	case string:len(NewSubStr) of
		LengthNewSubStr when LengthNewSubStr == 0 ->
			NewStr;
		LengthNewSubStr ->
			case LengthNewStr - LengthNewSubStr of
				Length when Length < 0 ->
					NewStr;
				Length ->
					Head = string:substr(NewStr, 1, LengthNewSubStr),
					case string:equal(Head, NewSubStr) of
						true ->		
							Tail = string:substr(NewStr, LengthNewSubStr+1, Length),
							ltrim(Tail, SubStr);
						false ->
							NewStr
					end
			end
	end.

md5(S, true) ->
	string:substr(md5(S), 9, 16).

md5(S) ->
    Md5_bin =  erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).

%% lib_sys
log(Str) ->
	Dir = root_dir() ++ "log/" ++ date_str("y-m-d") ++ "-log.txt",
	Log = " \n =====================" ++ date_str() ++ "============================ \n " ++ Str,
	append(Dir, Log).


%% 写系统日志到文件中
write_log(Report) ->
  Dir = root_dir() ++ "log/cache_"++ random() ++".txt",
  {ok, S} = file:open(Dir, write),
  io:format(S, "~p~n", [Report]),
  file:close(S),
  {ok, Str} = file_get_contents(Dir),
  log(Str),
  file:delete(Dir),
  ok.

%% lib_serv

get_redis() ->
	broadcast_room_element:get_redis().

%%进入认证
in_room({Pid, Uid, Mac, Type, Random}) ->
	broadcast_room_element:in_room({Pid, Uid, Mac, Type, Random}).

%% 退出认证
out_room(Mac) ->
	broadcast_room_element:out_room(Mac).

%%% 在线人数统计 
online() ->
	broadcast_room_element:online().

%% 字符串解码
decode(Str) ->
	decode_string_server:parse_document(to_binary(Str)).

% 字符串编码
encode(Str) ->
  	encode_string_server:parse_document(to_binary(Str)).

%% 写认证日志
write_log(TableName, LogList) ->
	insert(TableName, LogList).