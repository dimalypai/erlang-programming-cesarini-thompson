-module(io_handler).
-export([init/1, terminate/1, handle_event/2]).

%% IO handler. Filters events of the form {raise_alarm, Id, Type} and {clear_alarm, Id, Type}.
%% Keeps the counter of the occurences of these events.
%% Handles events by printing information about them and incrementing the counter.

% Initialise with initial counter value.
init(Count) -> Count.

% Returns a tagged counter value on termination.
terminate(Count) -> {count, Count}.

% Event handler function itself. Prints the information about specific events and returns
% the incremented counter.
handle_event({raise_alarm, Id, Alarm}, Count) ->
  print(alarm, Id, Alarm, Count),
  Count + 1;
handle_event({clear_alarm, Id, Alarm}, Count) ->
  print(clear, Id, Alarm, Count),
  Count + 1;
handle_event(_Event, Count) -> Count.

% Printing function.
print(Type, Id, Alarm, Count) ->
  Date = fmt(date()),
  Time = fmt(time()),
  io:format("#~w,~s,~s,~w,~w,~p~n", [Count, Date, Time, Type, Id, Alarm]).

% Formatting utilities.

fmt({AInt, BInt, CInt}) ->
  AStr = pad(integer_to_list(AInt)),
  BStr = pad(integer_to_list(BInt)),
  CStr = pad(integer_to_list(CInt)),
  [AStr,$:,BStr,$:,CStr].

pad([M1]) -> [$0, M1];
pad(Other) -> Other.

