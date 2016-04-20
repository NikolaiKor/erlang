%%%-------------------------------------------------------------------
%%% @author nikolai
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 6:52 PM
%%%-------------------------------------------------------------------
-module(gen_db).
-author("nikolai").
-behavior(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([stop/0, start_link/0, start/0]).
-export([get/1, set/2, remove/1]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, table, []).
start() -> gen_server:start({local, ?MODULE}, ?MODULE, table, []).
stop() -> gen_server:cast(?MODULE, stop).

get(Key) -> gen_server:call(?MODULE, {get, Key}).
set(Key, Value) -> gen_server:call(?MODULE, {put, Key, Value}).
remove(Key) -> gen_server:call(?MODULE, {remove, Key}).

init(TableName) -> {ok, ets:new(TableName, [])}.

handle_cast(stop, LoopData) -> {stop, normal, LoopData}.

handle_call({get, Key}, _From, TableName) -> {reply, ets:lookup(TableName, Key), TableName};
handle_call({put, Key, Value}, _From, TableName) -> {reply, ets:insert(TableName, {Key, Value}), TableName};
handle_call({remove, Key}, _From, TableName) -> {reply, ets:delete(TableName, Key), TableName}.

terminate(_Reason, _LoopData) -> ok.