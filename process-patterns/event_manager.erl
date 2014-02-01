-module(event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, swap_handlers/3, get_data/2, send_event/2]).
-export([init/1]).

%% Event manager. Allows to add and delete handlers and send events to them in a generic way.

% Starts an event manager which will be registered as Name with initial list of handlers.
% List of handlers is a list of tuples with handler module and data.
start(Name, HandlerList) ->
  register(Name, spawn(event_manager, init, [HandlerList])),
  ok.

% Initialisation function.
init(HandlerList) -> loop(initialise(HandlerList)).

% Goes through the handlers list and calls their init function passing the initial data.
initialise([]) -> [];
initialise([{Handler, InitData} | Rest]) ->
  [{Handler, Handler:init(InitData)} | initialise(Rest)].

% Stops an event manager registered as Name.
stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

% Goes through the handlers list and calls their terminate function, passing the current data
% and collecting the return values in a list.
terminate([]) -> [];
terminate([{Handler, Data} | Rest]) ->
  [{Handler, Handler:terminate(Data)} | terminate(Rest)].

add_handler(Name, Handler, InitData) -> call(Name, {add_handler, Handler, InitData}).

delete_handler(Name, Handler) -> call(Name, {delete_handler, Handler}).

% Terminates OldHandler and adds NewHandler passing the return value of OldHandler terminate
% as the initial data to the NewHandler.
swap_handlers(Name, OldHandler, NewHandler) -> call(Name, {swap_handlers, OldHandler, NewHandler}).

get_data(Name, Handler) -> call(Name, {get_data, Handler}).

send_event(Name, Event) -> call(Name, {send_event, Event}).

% Main worker function. Handles all the messages originated as user calls.

handle_msg({add_handler, Handler, InitData}, LoopData) ->
  {ok, [{Handler, Handler:init(InitData)} | LoopData]};

handle_msg({delete_handler, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false -> {{error, instance}, LoopData};
    {value, {Handler, Data}} ->
      Reply = {data, Handler:terminate(Data)},
      NewLoopData = lists:keydelete(Handler, 1, LoopData),
      {Reply, NewLoopData}
  end;

handle_msg({swap_handlers, OldHandler, NewHandler}, LoopData) ->
  case handle_msg({delete_handler, OldHandler}, LoopData) of
    {{data, TermData}, NewLoopData} -> handle_msg({add_handler, NewHandler, TermData}, NewLoopData);
    Other -> Other
  end;

handle_msg({get_data, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false -> {{error, instance}, LoopData};
    {value, {Handler, Data}} -> {{data, Data}, LoopData}
  end;

handle_msg({send_event, Event}, LoopData) -> {ok, event(Event, LoopData)}.

% Goes through the handlers list and calls their handle_event function.
event(_Event, []) -> [];
event(Event, [{Handler, Data} | Rest]) ->
  [{Handler, Handler:handle_event(Event, Data)} | event(Event, Rest) ].

% Utility function. Turns user calls to communication with the event manager.
call(Name, Msg) ->
  Name ! {request, self(), Msg},
  receive {reply, Reply} -> Reply end.

% Sends a reply message to process denoted as To.
reply(To, Msg) -> To ! {reply, Msg}.

% Main loop. Delegates the work to handle_msg.
loop(State) ->
  receive
    {request, From, Msg} ->
      {Reply, NewState} = handle_msg(Msg, State),
      reply(From, Reply),
      loop(NewState);
    {stop, From} ->
      reply(From, terminate(State))
  end.

