-module(text).
-export([wrap/2]).

% Simple text wrapper. Tries to wrap a given string with respect to a given width.
wrap(Text, Width) ->
  Words = string:tokens(Text, " "),
  print_wrapped(Words, Width, 0).

% Wrapper helper. Takes a list of words, a width and a current width.
print_wrapped([], _, _) -> io:format("~n");
print_wrapped([ Word | Words ], Width, CurrentWidth) ->
  if
    CurrentWidth + length(Word) =< Width ->
      % fits on the current line
      io:format("~s ", [Word]),
      % continue with updated current width
      print_wrapped(Words, Width, CurrentWidth + length(Word) + 1);
    length(Word) >= Width ->
      % word is longer than the given width
      if
        % current line is not empty, insert line feed
        CurrentWidth /= 0 -> io:format("~n~s~n", [Word]);
        % current line is empty
        true -> io:format("~s~n", [Word])
      end,
      % continue with reset current width
      print_wrapped(Words, Width, 0);
    true ->
      % next word doesn't fit, create a new line
      io:format("~n", []),
      % continue with reset current width and the same word
      print_wrapped([ Word | Words ], Width, 0)
  end.

