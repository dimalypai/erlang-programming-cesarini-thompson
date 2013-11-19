-module(sum).
-export([ sum/1
        , sum/2
        ]).

% Returns the sum of all the integers between 1 and N.
% N is positive.
% Not tail-recursive.
sum(1) -> 1;
sum(N) when N > 1 -> N + sum(N - 1).

% Returns the sum of the interval between N and M.
% N <= M.
% Not tail-recursive.
sum(N, N) -> N;
sum(N, M) when N < M -> M + sum(N, M - 1).

