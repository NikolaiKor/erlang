%%%-------------------------------------------------------------------
%%% @author nikolai
%%% @doc Module with solving 99 Scala Problems("[http://aperiodic.net/phil/scala/s-99/]")
%%%-------------------------------------------------------------------
-module(lists).
-author("nikolai").

%% API
-export([last/1, penultimate/1, nth/2, length2/1, reverse/1, duplicate/1, split/2, insertAt/3, range/2, removeAt/2,
  isPalindrome/1, compress/1, pack/1, encode/1, encodeModified/1, decode/1, encodeDirect/1, duplicateN/2, drop/2, slice/3, rotate1/2, rotate2/2, randomSelect/2]).
-export([start/0]).

-include_lib("eunit/include/eunit.hrl").

%%P01 (*) Find the last element of a list.
-spec last(A:: [integer()]) -> integer().
last([]) -> throw(badarg);
last([H]) -> H;
last([_|T]) -> last(T).

%%P02 (*) Find the last but one element of a list.
penultimate([]) -> throw(badarg);
penultimate([_]) -> throw(badarg);
penultimate([L, _]) -> L;
penultimate([_|T]) -> penultimate(T).

%%P03 (*) Find the Kth element of a list.
%%By convention, the first element in the list is element 0.
nth(N, List) when N >= 0 -> nth(N, 0, List);
nth(_N, _List) -> throw(badarg).
nth(_N, _I, []) -> throw(badarg);
nth(N, N, [H|_]) -> H;
nth(N, I, [_|T]) -> nth(N, I + 1, T).

%%P04 (*) Find the number of elements of a list.
length2([]) -> 0;
length2(List) -> length2(List, 0).
length2([], Acc) -> Acc;
length2([_|T], Acc) -> length2(T, Acc + 1).

%%P05 (*) Reverse a list.
reverse([]) -> [];
reverse(List) -> reverse(List, []).
reverse([], Acc) -> Acc;
reverse([H|T], Acc) -> reverse(T, [H|Acc]).

%%P06 (*) Find out whether a list is a palindrome.
%%Example:
%%scala> isPalindrome(List(1, 2, 3, 2, 1))
%%res0: Boolean = true
isPalindrome(List) -> List == reverse(List).

%%P07 (**) Flatten a nested list structure.
%%scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
%%res0: List[Any] = List(1, 1, 2, 3, 5, 8)
flatten([]) -> [];
flatten(List) -> reverse(flatten(List, [])).
flatten([], Acc) -> Acc;
flatten([[H|T1]|T2], Acc) -> flatten(T2, flatten([H|T1], []) ++ Acc);
flatten([H|T], Acc) -> flatten(T, [H|Acc]).

%%P08 (**) Eliminate consecutive duplicates of list elements.
%%If a list contains repeated elements they should be replaced with a single copy of the element.
%%The order of the elements should not be changed.
%%scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
%%res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
compress([]) -> [];
compress(List) -> compress(List, []).
compress([H], Acc) -> reverse([H|Acc]);
compress([H, H|T], Acc) -> compress([H|T], Acc);
compress([H|T], Acc) -> compress(T, [H|Acc]).

%%P09 (**) Pack consecutive duplicates of list elements into sublists.
%%If a list contains repeated elements they should be placed in separate sublists.
%%scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd)
%%res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d))
pack([]) -> [];
pack(List) -> reverse(pack(List, [], [])).
pack([H, H|T], Acc, Acc2) -> pack([H|T], Acc, [H|Acc2]);
pack([], Acc, _Acc2) -> Acc;
pack([H|T], Acc, Acc2) -> pack(T, [[H|Acc2]|Acc], []).

%%P10 (*) Run-length encoding of a list.
%%Use the result of problem P09 to implement the so-called run-length encoding data compression method.
%% Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
%%scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
%%res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
encode([]) -> [];
encode(List) -> encode(List, [], 1).
encode([], Acc, _Count) -> reverse(Acc);
encode([[H]|T], Acc, Count) -> encode(T, [{Count, H}|Acc], 1);
encode([[_|T1]|T2], Acc, Count) -> encode([T1|T2], Acc, Count + 1).

%%P11 (*) Modified run-length encoding.
%%Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into
%% the result list. Only elements with duplicates are transferred as (N, E) terms.
%%Example:
%%scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
%%res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
encodeModified([]) -> [];
encodeModified(List) -> encodeModified(List, [], 1).
encodeModified([], Acc, _Count) -> reverse(Acc);
encodeModified([[H]|T], Acc, 1) -> encodeModified(T, [H|Acc], 1);
encodeModified([[H]|T], Acc, Count) -> encodeModified(T, [{Count, H}|Acc], 1);
encodeModified([[_|T1]|T2], Acc, Count) -> encodeModified([T1|T2], Acc, Count + 1).

%%P12 (**) Decode a run-length encoded list.
%%Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
%%scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
%%res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
decode([]) -> [];
decode(List) -> decode(List, [], 0).
decode([], Acc, _Count) -> reverse(Acc);
decode([{Count, _Symbol}|T], Acc, Count) -> decode(T, Acc, 0);
decode([{Count1, Symbol}|T], Acc, Count) -> decode([{Count1, Symbol}|T], [Symbol|Acc], Count + 1).

%%P13 (**) Run-length encoding of a list (direct solution).
%%Implement the so-called run-length encoding data compression method directly.
%%I.e. don't use other methods you've written (like P09's pack).
%%scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
%%res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
encodeDirect([]) -> [];
encodeDirect(List) -> encodeDirect(List, [], 1).
encodeDirect([], Acc, _Count) -> reverse(Acc);
encodeDirect([H, H|T], Acc, Count) -> encodeDirect([H|T], Acc, Count + 1);
encodeDirect([H|T], Acc, Count) -> encodeDirect(T, [{Count, H}|Acc], 1).

%%P14 (*) Duplicate the elements of a list.
%%scala> duplicate(List('a, 'b, 'c, 'c, 'd))
%%res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
duplicate([]) -> [];
duplicate([H|T]) -> [H, H|duplicate(T)].

%%P15 (**) Duplicate the elements of a list a given number of times.
%%scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
%%res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
duplicateN(_, []) -> throw(badarg);
duplicateN(N, List) when N > 0 -> duplicateN(N, List, [], 0);
duplicateN(_, _List) -> throw(badarg).
duplicateN(_N, [], Acc, _Count) -> reverse(Acc);
duplicateN(N, [_H|T], Acc, N) -> duplicateN(N, T, Acc, 0);
duplicateN(N, [H|T], Acc, Count) -> duplicateN(N, [H|T], [H|Acc], Count + 1).

%%P16 (**) Drop every Nth element from a list.
%%scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
%%res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
drop(_, []) -> throw(badarg);
drop(N, List) when N > 0 -> drop(N, List, [], 1);
drop(_, _List) -> throw(badarg).
drop(_N, [], Acc, _Count) -> reverse(Acc);
drop(N, [_H|T], Acc, N) -> drop(N, T, Acc, 1);
drop(N, [H|T], Acc, Count) -> drop(N, T, [H|Acc], Count + 1).

%%P17 (*) Split a list into two parts.
%%The length of the first part is given. Use a Tuple for your result.
split(N, [H|T]) when N > 0 -> split(N, 1, T, [H]);
split(_N, _List) -> throw(badarg).
split(_N, _N1, [], _ArrStorage) -> throw(badarg);
split(N, N, List, ArrStorage) -> {result, ArrStorage, List};
split(N, N1, [H|T], ArrStorage) -> split(N, N1 + 1, T, ArrStorage ++ [H]).

%%P18 (**) Extract a slice from a list.
%%Given two indices, I and K, the slice is the list containing the elements from and including the Ith element
%% up to but not including the Kth element of the original list. Start counting the elements with 0.
%%scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
%%res0: List[Symbol] = List('d, 'e, 'f, 'g)
slice(_I, _K, []) -> throw(badarg);
slice(I, K, _List) when K < I; I < 0; K < 0 -> throw(badarg); % +1 line or +1 guard? close
slice(I, K, List) -> slice(I, K, List, [], 0).
slice(_I, K, _List, Acc, K) -> reverse(Acc);
slice(_I, _K, [], _Acc, _Count) -> throw(badarg);
slice(I, K, [_H|T], Acc, Count) when Count < I -> slice(I, K, T, Acc, Count + 1); % []/List?
slice(I, K, [H|T], Acc, Count) -> slice(I, K, T, [H|Acc], Count + 1).

%%P19 (**) Rotate a list N places to the left.
%%Examples:
%%scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
%%res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
%%scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
%%res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
rotate1(_N, []) -> throw(badarg);
rotate1(N, List) when N < 0 -> rotate1(length(List) + N, List, [], 0);
rotate1(N, List) when N > 0 -> rotate1(N, List, [], 0).
rotate1(N, List, Acc, N) -> List ++ reverse(Acc);
rotate1(N, [H|T], Acc, Count) -> rotate1(N, T, [H|Acc], Count + 1).

rotate2(_N, []) -> throw(badarg);
rotate2(N, List) when N < 0 ->
  lists:sublist(List, length(List) + N + 1, abs(N)) ++ lists:sublist(List, length(List) + N);
rotate2(N, List) when N > 0 -> lists:sublist(List, N + 1, length(List) - N) ++ lists:sublist(List, N).

%%P20 (*) Remove the Kth element from a list.
%%Return the list and the removed element in a Tuple. Elements are numbered from 0.
%%scala> removeAt(1, List('a, 'b, 'c, 'd))
%%res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
removeAt(K, List) -> removeAt(K, 0, List, []).
removeAt(_K, _N, [], _ArrAcc) -> throw(badarg);
removeAt(K, K, [H|T], ArrAcc) -> {result, ArrAcc ++ T, H};
removeAt(K, N, [H|T], ArrAcc) -> removeAt(K, N + 1, T, ArrAcc ++ [H]).

%%P21 (*) Insert an element at a given position into a list.
%%scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
%%res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
insertAt(New, N, List) when N >= 0 -> insertAt(New, N, 0, List, []);
insertAt(_New, _N, _List) -> throw(badarg).
insertAt(_New, _N, _Count, [], _ArrAcc) -> throw(badarg);
insertAt(New, N, N, List, ArrAcc) -> ArrAcc ++ [New|List];
insertAt(New, N, Count, [H|T], ArrAcc) -> insertAt(New, N, Count + 1, T, ArrAcc ++ [H]).

%%P22 (*) Create a list containing all integers within a given range.
%%scala> range(4, 9)
%%res0: List[Int] = List(4, 5, 6, 7, 8, 9)
range(L, R) when L < R -> range([], L, R);
range(_L, _R) -> throw(badarg).
range(Arr, N, N) -> Arr ++ [N];
range(Arr, N, R) -> range(Arr ++ [N], N + 1, R).

%%P23 (**) Extract a given number of randomly selected elements from a list.
%%scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
%%res0: List[Symbol] = List('e, 'd, 'a)
%%Hint: Use the solution to problem P20
randomSelect(_N, []) -> throw(badarg);
randomSelect(N, List) -> randomSelect(N, List, [], 0).
randomSelect(N, List, Acc, Count) -> randomSelect(N, List, Acc, Count).

%%P24 (*) Lotto: Draw N different random numbers from the set 1..M.
%%scala> lotto(6, 49)
%%res0: List[Int] = List(23, 1, 17, 33, 21, 37)

%%P25 (*) Generate a random permutation of the elements of a list.
%%Hint: Use the solution of problem P23.
%%scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
%%res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

%%P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
%%In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
%%scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
%%res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

%%P27 (**) Group the elements of a set into disjoint subsets.
%%a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
%%scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
%%res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
%%b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
%%scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
%%res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
%%Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
%%
%%You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
%%

%%P28 (**) Sorting a list of lists according to length of sublists.
%%a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
%%scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
%%res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
%%b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
%%scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
%%res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
%%Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length.
%% Finally, the last three lists have length 2. This is the most frequent length.


start() ->
  A = slice(3, 7, [1111, 2222, 3333, 4444, 5555, 6666, 7777, 8888, 9999]),
  A.

last_normal_test() -> 5 = last([1, 2, 3, 4, 5]).
last_single_test() -> 4 = last([4]).
last_empty_test() -> ?assertThrow(badarg, last([])).

penultimate_normal_test() -> 4 = penultimate([1, 2, 3, 4, 5]).
penultimate_single_test() -> ?assertThrow(badarg, penultimate([3])).
penultimate_empty_test() -> ?assertThrow(badarg, penultimate([])).

nth_normal_test() -> 4 = nth(3, [1, 2, 3, 4, 5]).
nth_single_test() -> 8 = nth(0, [8]).
nth_negative_test() -> ?assertThrow(badarg, nth(-5, [1, 2, 3, 4])).
nth_high_test() -> ?assertThrow(badarg, nth(6, [1, 2, 3, 4])).
nth_empty_test() -> ?assertThrow(badarg, nth(2, [])).

length_normal_test() -> 5 = lists:length2([1, 2, 3, 4, 5]).
length_single_test() -> 1 = lists:length2([4]).
length_empty_test() -> 0 = lists:length2([]).

reverse_normal_test() -> [5, 4, 3, 2, 1] = reverse([1, 2, 3, 4, 5]).
reverse_single_test() -> [4] = reverse([4]).
reverse_empty_test() -> [] = reverse([]).

duplicate_normal_test() -> [1, 1, 2, 2, 3, 3, 4, 4, 5, 5] = duplicate([1, 2, 3, 4, 5]).
duplicate_single_test() -> [4, 4] = duplicate([4]).
duplicate_empty_test() -> [] = duplicate([]).

split_normal_test() -> {result, [1, 2], [3, 4, 5]} = split(2, [1, 2, 3, 4, 5]).
split_negative_test() -> ?assertThrow(badarg, split(-2, [1, 2, 3])).
split_high_test() -> ?assertThrow(badarg, split(8, [1, 2, 3, 4, 5])).

removeAt_normal_test() -> {result, [1, 2, 4, 5], 3} = removeAt(2, [1, 2, 3, 4, 5]).
removeAt_negative_test() -> ?assertThrow(badarg, removeAt(-2, [1, 2, 3, 4, 5])).
removeAt_high_test() -> ?assertThrow(badarg, removeAt(8, [1, 2, 3, 4, 5])).

insertAt_normal_test() -> [1, 2, 3, 4, 5] = insertAt(4, 3, [1, 2, 3, 5]).
insertAt_high_test() -> ?assertThrow(badarg, insertAt(8, 12, [1, 2, 3, 4, 5])).
insertAt_negative_test() -> ?assertThrow(badarg, insertAt(8, -4, [1, 2, 3, 4, 5])).

range_normal_test() -> [4, 5, 6, 7, 8, 9] = range(4, 9).
range_high_test() -> ?assertThrow(badarg, range(9, 4)).

isPalindrome_true_test() -> true = isPalindrome([1, 2, 3, 2, 1]).
isPalindrome_false_test() -> false = isPalindrome([1, 2, 3, 4, 1]).

flatten_empty_test() -> [] = flatten([]).
flatten_normal_test() -> [1, 2, 3, 4] = flatten([1, 2, 3, 4]).
flatten_nested_test() -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = flatten([1, [2, 3], 4, [5, [6]], [7, 8, [9, 10]]]).

compress_empty_test() -> [] = compress([]).
compress_normal_test() -> [1, 2, 3, 4, 5, 6, 7] = compress([1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7]).

pack_empty_test() -> [] = pack([]).
pack_normal_test() ->
  [[1], [2, 2], [3, 3, 3], [4], [5, 5], [6, 6, 6, 6, 6], [7, 7, 7, 7]] = pack([1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7]).

encode_empty_test() -> [] = encode(pack([])).
encode_normal_test() ->
  [{1, 1}, {2, 2}, {3, 3}, {1, 4}, {2, 5}, {5, 6}, {4, 7}] = encode(pack([1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7])).

encodeModified_empty_test() -> [] = encodeModified(pack([])).
encodeModified_normal_test() ->
  [1, {2, 2}, {3, 3}, 4, {2, 5}, {5, 6}, {4, 7}] = encodeModified(pack([1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7])).

decode_empty_test() -> [] = decode(pack([])).
decode_normal_test() ->
  [1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7] = decode([{1, 1}, {2, 2}, {3, 3}, {1, 4}, {2, 5}, {5, 6}, {4, 7}]).

encodeDirect_empty_test() -> [] = encodeDirect([]).
encodeDirect_normal_test() ->
  [{1, 1}, {2, 2}, {3, 3}, {1, 4}, {2, 5}, {5, 6}, {4, 7}] = encodeDirect([1, 2, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7]).

duplicateN_normal_test() -> [1, 1, 2, 2, 3, 3, 4, 4, 5, 5] = duplicateN(2, [1, 2, 3, 4, 5]).
duplicateN_single_test() -> [4, 4, 4] = duplicateN(3, [4]).
duplicateN_empty_test() -> ?assertThrow(badarg, duplicateN(3, [])).
duplicateN_negative_test() -> ?assertThrow(badarg, duplicateN(-3, [1, 2, 3])).

drop_normal_test() -> [1, 2, 4, 5, 7, 8] = drop(3, [1, 2, 3, 4, 5, 6, 7, 8, 9]).
drop_single_test() -> [1, 2] = drop(3, [1, 2]).
drop_empty_test() -> ?assertThrow(badarg, drop(3, [])).
drop_negative_test() -> ?assertThrow(badarg, drop(-3, [1, 2, 3])).

slice_normal_test() -> [4, 5, 6, 7] = slice(3, 7, [1, 2, 3, 4, 5, 6, 7, 8, 9]).
slice_no_segment_test() -> ?assertThrow(badarg, slice(5, 3, [1, 2, 3, 4, 5, 6, 7, 8])).
slice_empty_test() -> ?assertThrow(badarg, slice(1, 3, [])).
slice_negative1_test() -> ?assertThrow(badarg, slice(-3, 2, [1, 2, 3])).
slice_negative2_test() -> ?assertThrow(badarg, slice(3, -2, [1, 2, 3])).
slice_negative3_test() -> ?assertThrow(badarg, slice(-3, -2, [1, 2, 3])).

rotate1_empty_test() -> ?assertThrow(badarg, rotate1(3, [])).
rotate1_normal_test() -> [4, 5, 6, 7, 8, 9, 1, 2, 3] = rotate1(3, [1, 2, 3, 4, 5, 6, 7, 8, 9]).
rotate1_negative_test() -> [8, 9, 1, 2, 3, 4, 5, 6, 7] = rotate1(-2, [1, 2, 3, 4, 5, 6, 7, 8, 9]).

rotate2_empty_test() -> ?assertThrow(badarg, rotate2(3, [])).
rotate2_normal_test() -> [4, 5, 6, 7, 8, 9, 1, 2, 3] = rotate2(3, [1, 2, 3, 4, 5, 6, 7, 8, 9]).
rotate2_negative_test() -> [8, 9, 1, 2, 3, 4, 5, 6, 7] = rotate2(-2, [1, 2, 3, 4, 5, 6, 7, 8, 9]).