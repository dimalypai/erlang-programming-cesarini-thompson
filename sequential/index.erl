-module(index).
-export([create_index/1]).

% Example file: doc.txt

% Takes a file name and prints an index for this file.
create_index(FileName) ->
  {ok, Text} = file:read_file(FileName),
  Words = string:tokens(binary:bin_to_list(Text), " \n\r\t"),
  print_index(Words).

% Main index printing function.
% Builds an index from a given list of words and outputs the result.
print_index(Words) ->
  Index = get_occurences(Words),
  Index1 = group_occurences(Index),
  output_index(Index1).

% Takes a list of words and returns an index (a list of pairs:
% word and the word's occurences (word numbers) in the file).
get_occurences(Words) -> get_occurences_helper(Words, [], 1).

% Helper function with the counter (current word number).
get_occurences_helper([], Index, _) -> sort_occurences(Index);
get_occurences_helper([ Word | Words ], Index, WordNumber) ->
  Index1 = 
    case lists:keyfind(Word, 1, Index) of
      {Word, Occurences} ->
        lists:keystore(Word, 1, Index, {Word, [WordNumber | Occurences]});
      false ->
        lists:keystore(Word, 1, Index, {Word, [WordNumber]})
    end,
  get_occurences_helper(Words, Index1, WordNumber + 1).

% Given an index, returns an index with occurences sorted.
sort_occurences([]) -> [];
sort_occurences([ Entry | Index ]) ->
  {Word, Occurences} = Entry,
  Occurences1 = lists:sort(Occurences),
  [ {Word, Occurences1} | sort_occurences(Index) ].

% Given an index (list of pairs with words and occurences),
% returns a grouped index (duplicates are removed and adjacent numbers are grouped as a range).
group_occurences([]) -> [];
group_occurences([ Entry | Index ]) ->
  [ group_entry_occurences(Entry) | group_occurences(Index) ].

% Grouping on index entry level.
group_entry_occurences({Word, Occurences}) ->
  {Word, group_entry_occurences_helper(tl(Occurences), hd(Occurences), hd(Occurences))}.

% Helper function with current start and end occurence.
group_entry_occurences_helper([], Start, End) -> [{Start, End}];
group_entry_occurences_helper([ WordNumber | Occurences ], Start, End) ->
  if
    WordNumber == End ->
      % skip duplicate
      group_entry_occurences_helper(Occurences, Start, End);
    WordNumber == End + 1 ->
      % group adjacent
      group_entry_occurences_helper(Occurences, Start, End + 1);
    true ->
      % create a range
      [ {Start, End} | group_entry_occurences_helper(Occurences, WordNumber, WordNumber) ]
  end.

% Outputs the whole index.
output_index([]) -> ok;
output_index([ Entry | Index ]) ->
  output_index_entry(Entry),
  output_index(Index).

% Outputs a word and its occurences: Erlang 1-2,4-6,8.
output_index_entry({ Word, Occurences }) ->
  FormattedOccurences = format_occurences(Occurences),
  io:format("~s ~s~n", [Word, lists:sublist(FormattedOccurences, length(FormattedOccurences) - 1)]).

% Returns a string of formatted occurences of the form: 1-2,4-6,8.
% Leaves "," at the end.
format_occurences([]) -> "";
format_occurences([ {Start, End} | Occurences ]) when Start == End ->
  integer_to_list(Start) ++ "," ++ format_occurences(Occurences);
format_occurences([ {Start, End} | Occurences ]) ->
  integer_to_list(Start) ++ "-" ++ integer_to_list(End) ++ "," ++ format_occurences(Occurences).

