-module(fel).
-export([handle_request/1, start/0, add_event/3, update_cel/1]).

%% @private
%% Handles incoming requests for event management.
%% complexity O(n) where n is the number of events in the list.
%%
%% @param State The current state of the game, which includes the list of events and the next time.


-spec handle_request({list(), integer()}) -> no_return().
handle_request({Events, Next_time}) ->
    {State, Continue} = receive 
        {add, {Time, Fun}} -> 
            % If time on the new event is smaller than current smallest time then update it
            Updated_time = case Time < Next_time of
                true -> Time;
                _ -> Next_time
                end,

            % Add the sent event to the list
            {{[{Time, Fun}] ++ Events,Updated_time}, true};
        {add, Additional_events} when is_list(Additional_events) ->

            [{First_time, _}|_] = Additional_events,
            Smallest_new_time = lists:foldl(fun({Time, _}, Accum) -> case Time < Accum of 
                                                                    true -> Time;
                                                                    _ -> Accum
                                                                end
                                                            end,
                                                            First_time, Additional_events),
                Updated_time = case First_time < Smallest_new_time of
                true -> First_time;
                _ -> Smallest_new_time
                end,

            io:format("fel ~p ~n",[{Additional_events ++ Events,Updated_time}]),
            {{Additional_events ++ Events,Updated_time}, true};
        {get_next, Pid} -> 
            % find the next events to send to the given Pid
            Next_events = lists:filter(fun({Time, _}) -> Time == Next_time end, Events),

            % Extract the first time in the list of events
            [{First_time, _}|_] = Next_events,

            % Sends the add message to the Pid giving it the next events
            Pid ! {add, Next_events},

            % Find the time of the next soonest event
            Updated_time = lists:foldl(fun({Time, _}, Accum) -> case Time < Accum of 
                                                                    true -> Time;
                                                                    _ -> Accum
                                                                end
                                                            end,
                                                            First_time, Next_events),

            % Uodate the list of events by removing the next events it just sent away
            Updated_events = lists:subtract(Events, Next_events),

            % The new event list and time
            {{Updated_events, Updated_time}, true};
        stop -> {{Events,Next_time},false}
        end,
    case Continue of
        true -> handle_request(State);
        _ -> ok
    end.


% API FUNCTIONS

%% @doc
%% Starts the future event list event handleing process
%% complexity O(1)
%%
%% @return Pid of the spawned process.

-spec start() -> pid().
start() ->
    spawn(fun() -> handle_request({[], 0}) end).


%% @doc
%% Adds an event to the event handling process.
%% complexity O(1)
%%
%% @param Pid The process identifier of the event handling process.
%% @param Fun The function to be added as an event.
%% @param Time The time at which the event should be triggered.
%% @return ok

-spec add_event(pid(), fun(), integer()) -> ok.
add_event(Pid,Fun, Time) ->
    Pid ! {add, {Time, Fun}}.


%% @doc
%% Updates the current event list in the event handling process.
%% complexity O(1)
%% @param Pid The process identifier of the event handling process.
%% @return ok

-spec update_cel(pid()) -> ok.
update_cel(Pid) ->
    Pid ! {get_next, self()}.

                                                            

    
