-module(lists1).
-export([ filter/2
        , reverse/1
        , concatenate/1
        , flatten/1
        , quick_sort/1
        , merge_sort/1
        ]).

% Given a list of integers and an integer, returns all integers
% less than or equal to that integer.
filter([], _) -> [];
filter([H|T], Key) when H =< Key -> [ H | filter(T, Key) ];
filter([_|T], Key) -> filter(T, Key).

% Returns a list with elements in reverse order.
reverse(List) -> reverse_acc(List, []).

% Helper with accumulator that contains the reversed list (so far).
reverse_acc([], Acc) -> Acc;
reverse_acc([H|T], Acc) -> reverse_acc(T, [H|Acc]).

% List concatenation: [[1,2,3], [], [4,five]] => [1,2,3,4,five].
concatenate([]) -> [];
concatenate([H|T]) when is_list(H) -> append(H, concatenate(T)).

append([], Ys) -> Ys;
append([X|Xs], Ys) -> [ X | append(Xs, Ys) ].

% Given a list of nested lists, returns a flat list.
flatten([]) -> [];
flatten([H|T]) -> append(flatten(H), flatten(T));
flatten(List) -> [List].

% Quick sort: partition list with respect to head (pivot) (< and >=),
% sort both parts recursively and put the pivot between the results.
quick_sort([]) -> [];
quick_sort([X]) -> [X];
quick_sort([ Pivot | List ]) ->
  {L, GE} = partition(List, Pivot, {[], []}),
  append(quick_sort(L), append([Pivot], quick_sort(GE))).

% Returns a list partitioned with respect to the Pivot.
% First element is elements which are less than the Pivot,
% second is all the others.
partition([], _, {L, GE}) -> {L, GE};
partition([H|T], Pivot, {L, GE}) ->
  if
    H < Pivot -> partition(T, Pivot, {[H|L], GE});
    true -> partition(T, Pivot, {L, [H|GE]})
  end.

% Merge sort: split list into halves, sort each half recursively
% and merge the results.
merge_sort([]) -> [];
merge_sort([X]) -> [X];
merge_sort(List) ->
  {Left, Right} = split(List),
  merge(merge_sort(Left), merge_sort(Right)).

% Split a given list roughly in halves.
split(List) -> split_helper(List, len(List), [], []).

% Helper function which takes a list and its original length
% and splits the list into halves (roughly) by subtracting two
% from the length and accumulating the first half and finally
% returning a tail (all remaining elements) as the second half.
split_helper([], _, L1, L2) -> {L1, L2};
split_helper(List, N, L1, _) when N =< 0 -> {L1, List};
split_helper([H|T], N, L1, L2) -> split_helper(T, N - 2, [H|L1], L2).

% Returns the length of a given list.
len([]) -> 0;
len([_|T]) -> 1 + len(T).

% Merges two sorted lists into one sorted list.
merge(Left, []) -> Left;
merge([], Right) -> Right;
merge([X|Xs], [Y|Ys]) ->
  if
    Y < X -> [ Y | merge([X|Xs], Ys) ];
    true -> [ X | merge(Xs, [Y|Ys]) ]
  end.

