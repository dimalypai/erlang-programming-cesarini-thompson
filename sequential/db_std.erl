-module(db_std).
-export([ new/0
        , destroy/1
        , write/3
        , delete/2
        , read/2
        , match/2
        ]).

% Returns a new database.
new() -> [].

% "Destroys" the database and returns ok.
destroy(_Db) -> ok.

% Writes Key-Element pair to the database and returns the new one.
write(Key, Element, Db) -> [ {Key, Element} | Db ].

% Deletes all the entries with the given Key from the database and returns the new one.
delete(Key, Db) -> lists:keydelete(Key, 1, Db).

% Returns {ok, Element} if an Element with the given Key is in the database
% or {error, instance} otherwise.
read(Key, Db) ->
  case lists:keyfind(Key, 1, Db) of
    {Key, Element} -> {ok, Element};
    false -> {error, instance}
  end.

% Returns a list with all the keys that are associated with the given Element in the database.
match(_, []) -> [];
match(Element, [ {Key, Element} | Db ]) -> [ Key | match(Element, Db) ];
match(Element, [ _ | Db]) -> match(Element, Db).

