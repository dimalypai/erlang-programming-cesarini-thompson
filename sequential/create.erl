-module(create).
-export([ create/1
        , reverse_create/1
        ]).

% Returns a list of the format [1,2,...,N-1,N].
create(N) when N > 0 -> create_helper(N, 1).

% Helper function that has an additional counter (current number) parameter.
create_helper(N, N) -> [N];
create_helper(N, I) when N > I -> [ I | create_helper(N, I + 1) ].

% Returns a list of the format [N,N-1,...,2,1].
reverse_create(1) -> [1];
reverse_create(N) when N > 1 -> [ N | reverse_create(N - 1) ].

