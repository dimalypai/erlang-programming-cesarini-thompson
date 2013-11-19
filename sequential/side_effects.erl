-module(side_effects).
-export([ print_ints/1
        , print_even_ints/1
        ]).

% Prints out the integers between 1 and N.
print_ints(N) when N > 0 -> print_ints_helper(N, 1).

% Helper function with additional counter (current number) parameter.
print_ints_helper(N, N) -> print_number(N);
print_ints_helper(N, I) when N > I ->
  print_number(I),
  print_ints_helper(N, I + 1).

% Prints out the even integers between 1 and N.
print_even_ints(N) when N > 0 -> print_even_ints_helper(N, 1).

% Helper function with additional counter (current number) parameter.
print_even_ints_helper(N, N) when N rem 2 == 0 -> print_number(N);
print_even_ints_helper(N, N) -> ok;
print_even_ints_helper(N, I) when N > I, I rem 2 == 0 ->
  print_number(I),
  print_even_ints_helper(N, I + 1);
print_even_ints_helper(N, I) when N > I -> print_even_ints_helper(N, I + 1).

print_number(N) -> io:format("Number:~p~n", [N]).

