-module(easy).
-author("nikolai").

%% API
-export([last/1, penultimate/1, nth/2, length/1, reverse/1, duplicate/1, split/2, insertAt/3, range/2, removeAt/2]).
-export([start/0]).

%%P01 (*) Find the last element of a list.
last([H|[]]) -> H;
last([_|T]) -> last(T).

%%P02 (*) Find the last but one element of a list.
penultimate([L, _|[]]) -> L;
penultimate([_|T]) -> penultimate(T).

%%P03 (*) Find the Kth element of a list.
%%By convention, the first element in the list is element 0.
nth(N, T) -> nth(N, 0, T).
nth(N, N, [H|_]) -> H;
nth(N, I, [_|T]) -> nth(N, I, T).

%%P04 (*) Find the number of elements of a list.
length([]) -> 0;
length([_|T]) -> easy:length(T) + 1.

%%P05 (*) Reverse a list.
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

%%P06 (*) Find out whether a list is a palindrome.
%%Example:
%%scala> isPalindrome(List(1, 2, 3, 2, 1))
%%res0: Boolean = true

%%P10 (*) Run-length encoding of a list.
%%Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
%%Example:
%%scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
%%res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

%%P11 (*) Modified run-length encoding.
%%Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
%%Example:
%%scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
%%res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

%%P14 (*) Duplicate the elements of a list.
%%scala> duplicate(List('a, 'b, 'c, 'c, 'd))
%%res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
duplicate([]) -> [];
duplicate([H|T]) -> [H, H] ++ duplicate(T).

%%P17 (*) Split a list into two parts.
%%The length of the first part is given. Use a Tuple for your result.
split(N, [H|T]) -> split(N, 1, T, [H]).
split(N, N, [_|T], ArrStorage) -> {result, ArrStorage, T};
split(N, N1, [H|T], ArrStorage) -> split(N, N1 + 1, T, ArrStorage ++ [H]).

%%P20 (*) Remove the Kth element from a list.
%%Return the list and the removed element in a Tuple. Elements are numbered from 0.
%%scala> removeAt(1, List('a, 'b, 'c, 'd))
%%res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
removeAt(K, List) -> removeAt(K, 0, List, []).
removeAt(K, K, [H|T], ArrAcc) -> {result, ArrAcc ++ T, H};
removeAt(K, N, [H|T], ArrAcc) -> removeAt(K, N + 1, T, [H] ++ ArrAcc).

%%P21 (*) Insert an element at a given position into a list.
%%scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
%%res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
insertAt(New, N, List) -> insertAt(New, N, 0, List, []).
insertAt(New, N, N, List, ArrAcc) -> ArrAcc ++ [New|List];
insertAt(New, N, Count, [H|T], ArrAcc) -> insertAt(New, N, Count + 1, T, ArrAcc ++ [H]).

%%P22 (*) Create a list containing all integers within a given range.
%%scala> range(4, 9)
%%res0: List[Int] = List(4, 5, 6, 7, 8, 9)
range(L, R) -> range([L], L, R).
range(Arr, N, N) -> Arr;
range(Arr, N, R) -> range(Arr, N + 1, R).

%%P24 (*) Lotto: Draw N different random numbers from the set 1..M.
%%scala> lotto(6, 49)
%%res0: List[Int] = List(23, 1, 17, 33, 21, 37)

%%P25 (*) Generate a random permutation of the elements of a list.
%%Hint: Use the solution of problem P23.
%%scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
%%res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)


start() ->
  A = insertAt(234, 4, [324, 456, 432, 888, 123, 998]),
  1.