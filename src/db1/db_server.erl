%%%-------------------------------------------------------------------
%%% @author nikolai
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2016 8:59 PM
%%%-------------------------------------------------------------------
-module(db_server).
-author("nikolai").

%% API
-export([start/0, stop/0, get/1, init/0, set/2, remove/1]).

start() -> register(db_server, spawn(db_server, init, [])).
stop() -> call(stop).
get(Key) -> call({get, Key}).
set(Key, Value) -> call({put, Key, Value}).
remove(Key) -> call({remove, Key}).

init() -> loop(ets:new(table, [])).

reply(Pid, Reply) -> Pid ! {reply, Reply}.

call(Message) ->
  db_server ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

loop(Table) ->
  receive
    {request, Pid, {get, Key}} ->
      reply(Pid, ets:lookup(Table, Key)),
      loop(Table);
    {request, Pid, {remove, Key}} ->
      reply(Pid, ets:delete(Table, Key)),
      loop(Table);
    {request, Pid, {put, Key, Value}} ->
      reply(Pid, ets:insert(Table, {Key, Value})),
      loop(Table);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.