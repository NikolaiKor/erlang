-module(er1).

-export([max2/1, min_max/1, min/1, min_max_3/1, average/1, start/0, len/1, sum/1, odd/1, member/2, min_2/1, min_max2/1, bubblesort/1, quicksort/1]).
-import(lists, [reverse/1]).

-include_lib("eunit/include/eunit.hrl").

start() ->
  A = quicksort([242, 657, 488, 724, 355, 873, 222]),
  1.

max2([]) -> throw(bagarg);
max2([H|T]) -> max2(T, H).
max2([], Max) -> Max;
max2([H|T], Max) when H > Max -> max2(T, H);
max2([_H|T], Max) -> max2(T, Max).

min_2([H|T]) -> min_2(H, T).
min_2(Min, []) -> Min;
min_2(Min, [H|T]) when Min > H -> min_2(H, T);
min_2(Min, [_|T]) -> min_2(Min, T).

min([]) -> throw(badarg);
min([H|T]) ->
  Min = fun(A, B) when A > B -> B;
    (A, _) -> A end,
  lists:foldl(Min, H, T).

min_max([]) -> throw(badarg);
min_max([H]) ->
  {minmax, H, H};
min_max([H|T]) ->
  {minmax, Min, Max} = min_max(T),
  Min2 = if
           Min > H -> H;
           true -> Min
         end,
  Max2 = if
           Max > H -> Max;
           true -> H
         end,
  {minmax, Min2, Max2}.

min_max2([]) -> throw(badarg);
min_max2([H|T]) -> min_max2({minmax, H, H}, T).
min_max2({minmax, Min, Max}, [H|T]) when Min > H -> min_max2({minmax, H, Max}, T);
min_max2({minmax, Min, Max}, [H|T]) when Max < H -> min_max2({minmax, Min, H}, T);
min_max2({minmax, Min, Max}, [_|T]) -> min_max2({minmax, Min, Max}, T);
min_max2({minmax, Min, Max}, []) -> {minmax, Min, Max}.

min_max_3([]) -> throw(badarg);
min_max_3(List) ->
  {minmax, lists:min(List), lists:max(List)}.


average([]) -> throw(badarg);
average(List) -> average(List, 0, 0).
average([], Count, Sum) -> Sum / Count;
average([H|T], Count, Sum) -> average(T, Count + 1, Sum + H).

len([]) -> 0;
len([_|T]) -> len(T) + 1.

sum([]) -> 0;
sum([H|T]) -> sum(T) + H.

odd([]) -> [];
odd([H|T]) when H rem 2 == 0 -> [H|odd(T)];
odd([_|T]) -> odd(T).

member(_, []) -> false;
member(H, [H|_]) -> true;
member(A, [_|T]) -> member(A, T).

bubblesort([]) -> [];
bubblesort(Arr) when is_list(Arr) -> bubblesort(false, [], Arr).
bubblesort(true, Acc, [H]) -> bubblesort(false, [], Acc ++ [H]);
bubblesort(false, Acc, [H]) -> Acc ++ [H];
bubblesort(_, Acc, [H, H1|T]) when H > H1 -> bubblesort(true, Acc ++ [H1], [H|T]);
bubblesort(Changed, Acc, [H|T]) -> bubblesort(Changed, Acc ++ [H], T).

quicksort([]) -> [];
quicksort([H|T]) -> quicksort([X || X <- T, X < H]) ++ [H] ++ quicksort([X || X <- T, X > H]).

min_normal_test() -> 3 = min([7, 4, 8, 3, 12]).
min_single_test() -> 5 = min([5]).
min_empty_test() -> ?assertThrow(badarg, min([])).

max_normal_test() -> 12 = max2([7, 4, 8, 3, 12]).
max_single_test() -> 5 = max2([5]).
max_empty_test() -> ?assertThrow(bagarg, max2([])).

min_max_normal_test() -> {minmax, 3, 12} = min_max([7, 4, 8, 3, 12]).
min_max_single_test() -> {minmax, 5, 5} = min_max([5]).
min_max_empty_test() -> ?assertThrow(badarg, min_max([])).

min_max2_normal_test() -> {minmax, 3, 12} = min_max2([7, 4, 8, 3, 12]).
min_max2_single_test() -> {minmax, 5, 5} = min_max2([5]).
min_max2_empty_test() -> ?assertThrow(badarg, min_max2([])).

min_max3_normal_test() -> {minmax, 3, 12} = min_max_3([7, 4, 8, 3, 12]).
min_max3_single_test() -> {minmax, 5, 5} = min_max_3([5]).
min_max3_empty_test() -> ?assertThrow(badarg, min_max_3([])).

average_normal_test() -> 3.0 = average([1, 2, 3, 4, 5]).
average_single_test() -> 5.0 = average([5]).
average_empty_test() -> ?assertThrow(badarg, average([])).

len_normal_test() -> 5 = len([1, 2, 3, 4, 5]).
len_empty_test() -> 0 = len([]).

sum_normal_test() -> 15 = sum([1, 2, 3, 4, 5]).
sum_single_test() -> 5 = sum([5]).
sum_empty_test() -> 0 = sum([]).

odd_normal_test() -> [2, 4] = odd([1, 2, 3, 4, 5]).
odd_single_test() -> [] = odd([5]).
odd_empty_test() -> [] = odd([]).

member_normal_true_test() -> true = member(2, [1, 2, 3, 4, 5]).
member_normal_false_test() -> false = member(8, [1, 2, 3, 4, 5]).
member_single_true_test() -> true = member(5, [5]).
member_single_false_test() -> false = member(3, [5]).
member_empty_test() -> false = member(2, []).

bubblesort_normal_test() -> [1, 2, 3, 4, 5] = bubblesort([5, 4, 2, 1, 3]).
bubblesort_normal2_test() -> [1, 2, 3, 4, 5] = bubblesort([5, 4, 3, 2, 1]).
bubblesort_single_test() -> [5] = bubblesort([5]).
bubblesort_empty_test() -> [] = bubblesort([]).