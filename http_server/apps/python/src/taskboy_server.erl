%% gen_server代码模板

-module(taskboy_server).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
% --------------------------------------------------------------------
-export([]).

% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

% --------------------------------------------------------------------
% External API
% --------------------------------------------------------------------





start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([]) ->
    {ok, _TRef} = timer:send_interval(1000, update),
    {ok, #state{}}.

% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info(update, State) ->
    %% do something here
    % error_logger:info_msg("update, do something here ..."),
    Time = timestamp_to_datetime(timestamp()),
    % error_logger:info_msg("update, do something here ~p~n ...", [Time]),

    case Time of
        {_, {_, _, 1}} ->
            % error_logger:info_msg("update, do something here ~p~n ...", [Time]),
            % 定时任务
            % Work = {fetch_web, self()},
            % workboy:start(Work),

            ok;
        _ ->
            ok
    end,

    {noreply, State};
handle_info({from_workboy, Message}, State) ->
    % 接收定时任务返回信息
    error_logger:info_msg("from workboy, do something here ~p~n ...", [Message]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



% private functions  -------------------------------------------------
% 时间转时间戳，格式：{{2013,11,13}, {18,0,0}}
datetime_to_timestamp(DateTime) ->
    % calendar:datetime_to_gregorian_seconds(DateTime) -  calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) - 8 * 60 * 60.
    calendar:datetime_to_gregorian_seconds(DateTime) -  calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).


% 时间戳转时间
timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +  calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).

%% 获取时间截
timestamp() ->
    DateTime = calendar:local_time(),
    datetime_to_timestamp(DateTime).
