-module(my_db).
-export([ start/0
        , stop/0
        , write/2
        , delete/1
        , read/1
        , match/1
        ]).
-export([init/0]).

%% Client interface.

% Starts and registers a database server.
start() ->
  register(db_server, spawn(my_db, init, [])),
  ok.

% Stops the database server.
stop() -> call(stop).

write(Key, Element) -> call({write, Key, Element}).

delete(Key) -> call({delete, Key}).

read(Key) -> call({read, Key}).

match(Element) -> call({match, Element}).

%% Server utilities.

% Initialises an empty databases and starts the main loop.
init() -> loop(new()).

% Main loop.
loop(Db) ->
  receive
    {request, Pid, {write, Key, Element}} ->
      NewDb = write(Key, Element, Db),
      reply(Pid, ok),
      loop(NewDb);
    {request, Pid, {delete, Key}} ->
      NewDb = delete(Key, Db),
      reply(Pid, ok),
      loop(NewDb);
    {request, Pid, {read, Key}} ->
      Reply = read(Key, Db),
      reply(Pid, Reply),
      loop(Db);
    {request, Pid, {match, Element}} ->
      Reply = match(Element, Db),
      reply(Pid, Reply),
      loop(Db);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.

% Turns user calls to communication with the database server.
call(Message) ->
  db_server ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

% Sends a reply message to process with Pid.
reply(Pid, Reply) -> Pid ! {reply, Reply}.

%% Database operations.

% Returns a new database.
new() -> [].

% Writes Key-Element pair to the database and returns the new one.
write(Key, Element, Db) -> [ {Key, Element} | Db ].

% Deletes all the entries with the given Key from the database and returns the new one.
delete(_, []) -> [];
delete(Key, [ {Key, _} | Db ]) -> delete(Key, Db);
delete(Key, [ Entry | Db ]) -> [ Entry | delete(Key, Db) ].

% Returns {ok, Element} if an Element with the given Key is in the database
% or {error, instance} otherwise.
read(_, []) -> {error, instance};
read(Key, [ {Key, Element} | _ ]) -> {ok, Element};
read(Key, [ _ | Db]) -> read(Key, Db).

% Returns a list with all the keys that are associated with the given Element in the database.
match(_, []) -> [];
match(Element, [ {Key, Element} | Db ]) -> [ Key | match(Element, Db) ];
match(Element, [ _ | Db]) -> match(Element, Db).

