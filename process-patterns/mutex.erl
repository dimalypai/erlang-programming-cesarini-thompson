-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

% Starts a mutex process with init function and registers it as mutex.
start() ->
  register(mutex, spawn(?MODULE, init, [])),
  ok.

% Stops a mutex process.
stop() -> mutex ! stop.

% Try to grab a lock, blocks if it is already grabed.
wait() ->
  mutex ! {wait, self()},
  receive ok -> ok end.

% Releases a lock.
signal() -> mutex ! {signal, self()}, ok.

init() -> free().

% Function representing Free state.
free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    stop -> terminate()
  end.

% Function representing Busy state.
busy(Pid) ->
  receive
    {signal, Pid} ->
      free()
  end.

% Termination function. Kills all waiting processes.
terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after
    0 -> ok
  end.

