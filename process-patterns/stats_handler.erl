-module(stats_handler).
-export([init/1, terminate/1, handle_event/2]).

%% Statistics event handler. Keeps the count of how many times an event occurs.
%% Event is represented as {Type, Description}.

% Initialise with initial statistics as a list of tuples {{Type, Description}, Count}.
init(Stats) -> Stats.

% Returns the collected data on termination.
terminate(Stats) -> Stats.

% Event handler function itself. Updates the statistics by incrementing the counter or
% creating a new entry with the value of counter equal to 1.
handle_event({Type, _, Desc}, Stats) ->
  case lists:keyfind({Type, Desc}, 1, Stats) of
    {_, Count} -> lists:keyreplace({Type, Desc}, 1, Stats, {{Type, Desc}, Count + 1});
    false -> [ {{Type, Desc}, 1} | Stats ]
  end.

