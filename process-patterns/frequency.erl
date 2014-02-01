-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

% Starts a frequency server. Registers it as frequency.
start() -> register(frequency, spawn(frequency, init, [])).

% Initialisation with frequency range.
init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Frequency range.
get_frequencies() -> lists:seq(10, 15).

% Stops the frequency server.
stop() -> call(stop).

% Makes a request to get frequency.
allocate() -> call(allocate).

% Makes a request to release frequency.
deallocate(Freq) -> call({deallocate, Freq}).

% Utility function. Turns user calls to communication with the frequency server.
call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

% Main loop.
loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Pid, Freq),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.

% Sends a reply message to process with Pid.
reply(Pid, Reply) -> Pid ! {reply, Reply}.

% Internal function for frequency allocation.
% Maintains a pair of list of available frequencies and list of frequency-pid pairs of allocated frequencies.
% Returns a pair of updated list of available frequencies and reply ({error, no_frequency} or {ok, allocated frequency}).
allocate({[], Allocated}, _Pid) -> {{[], Allocated}, {error, no_frequency}};
allocate({[Freq | Free], Allocated}, Pid) -> {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}.

% Internal function for frequency deallocation.
% Takes a pair of list of available frequencies and list of frequency-pid pairs of allocated frequencies.
% Returns an updated pair (with the frequency given as a second argument added to the available frequencies list
% and deleted from the allocated list) and a reply.
% If there is no such frequency-pid pair in Allocated -> returns error as a reply.
deallocate({Free, Allocated}, Pid, Freq) ->
  case lists:member({Freq, Pid}, Allocated) of
    false -> {{Free, Allocated}, error};
    true  ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {{[Freq | Free], NewAllocated}, ok}
  end.

