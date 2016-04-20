%%%-------------------------------------------------------------------
%%% @author nikolai
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2016 8:47 PM
%%%-------------------------------------------------------------------
-module(parser).
-author("nikolai").

%% API
-export([parseTree/1, start/0]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Function for parsing list of elements like {element, Value, Level} to tree structure
%% @spec parseTree(A::List) -> Result
%%       List = [Item]
%%       Item = {{element, term(), integer()}}
parseTree([]) -> [];
parseTree(Arr) when is_list(Arr) -> parseTree(Arr, 1, []).
parseTree([], _, Acc) -> Acc;
parseTree([{element, Value, Level}|T], Level, Acc) ->
  parseTree(T, Level, Acc ++ [{element, Value, parseTree(T, Level + 1, [])}]);
parseTree([{element, _Value, Level}|T], Level2, Acc) when Level > Level2 -> parseTree(T, Level2, Acc);
parseTree([{element, _Value, _Level}|_T], _, Acc) -> Acc.

start() ->
  A = parseTree([{element, 1, 1}, {element, 2, 1},
    {element, 3, 2}, {element, 4, 2}, {element, 11, 3}, {element, 5, 2},
    {element, 6, 1}, {element, 7, 1}, {element, 8, 2}, {element, 9, 3}, {element, 10, 1}]),
  A.

%% Tests
parseTree_normal1_test() ->
  [{element, 1, []},
    {element, 2, [
      {element, 3, []},
      {element, 4, []},
      {element, 5, []}]},
    {element, 6, []}] =
    parseTree([
      {element, 1, 1},
      {element, 2, 1},
        {element, 3, 2},
        {element, 4, 2},
        {element, 5, 2},
      {element, 6, 1}]).

parseTree_normal2_test() ->
  [
    {element, 1, []},
    {element, 2, [
      {element, 3, []},
      {element, 4, [
        {element, 11, []}
      ]},
      {element, 5, []}]},
    {element, 6, []},
    {element, 7, [
      {element, 8, [
        {element, 9, []}]}]},
    {element, 10, []}] =
    parseTree([
      {element, 1, 1},
      {element, 2, 1},
      {element, 3, 2},
      {element, 4, 2},
      {element, 11, 3},
      {element, 5, 2},
      {element, 6, 1},
      {element, 7, 1},
      {element, 8, 2},
      {element, 9, 3},
      {element, 10, 1}]).

parseTree_empty_test() -> [] = parseTree([]).