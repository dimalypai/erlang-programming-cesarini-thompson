-module(log_handler).
-export([init/1, terminate/1, handle_event/2]).

%% Logging event handler. Writes an information about events to a file.

% Initialise with a file name.
% Returns a reference to the file.
init(File) ->
  {ok, Fd} = file:open(File, write),
  Fd.

% Closes the file on termination.
terminate(Fd) -> file:close(Fd).

% Event handler function itself.
% Formats data and writes it to a file.
% Returns a reference to the file.
handle_event({Action, Id, Event}, Fd) ->
  {MegaSec, Sec, MicroSec} = now(),
  io:format(Fd, "~w,~w,~w,~w,~w,~p~n",
                [MegaSec, Sec, MicroSec, Action, Id, Event]),
  Fd;
handle_event(_Event, Fd) -> Fd.

