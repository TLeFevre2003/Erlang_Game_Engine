-module(fel).
-export([handle_request/1, start/0, add_event/3, update_cel/1]).

%% @private
%% Handles incoming requests for event management.
%% complexity O(n) where n is the number of events in the list.
%%
%% @param State The current state of the game, which includes the list of events and the next time.


-spec handle_request({list(), integer()}) -> no_return().
handle_request({Events, Next_time}) ->
    io:format("Current simulation time ~p ~n",[Next_time]),
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

            io:format("fel add ~p~p~n", [Additional_events ++ Events, Updated_time]),
            {{Additional_events ++ Events,Updated_time}, true};
        {get_next, Pid} ->
            io:format("fel get_next recieved ~p ~p~n", [Events, Next_time]),
            
            % Find the next events to send to the given Pid
            Next_events = lists:filter(fun({Time, _}) -> Time == Next_time end, Events),


                % Send the add message to Pid with the next events
                Pid ! {add, Next_events},
                io:format("fel sent events ~p ~n", [Next_events]),

                % Find the time of the next soonest event
                Updated_events = lists:subtract(Events, Next_events),

                io:format("Finding next state with updated events ~p ~n",[Updated_events]),

                find_updated_state(Updated_events,Next_time);


        stop -> {{Events,Next_time},false}
        end,
    case Continue of
        true -> 
            io:format(" FEL Recursively calling with updated state ~p ~n",[State]),
            handle_request(State);
        _ -> ok
    end.
find_updated_state([],Time) -> {{[], Time},true};
find_updated_state(Updated_events, _) ->
    [{First_updated_time, _} | _] = Updated_events,
        Updated_time = lists:foldl(fun({Time, _}, Accum) ->
                                                if
                                                    Time < Accum -> Time;
                                                    true -> Accum
                                                end
                                            end,
                                            First_updated_time, Updated_events),

                io:format("Next simulated time ~p ~n",[Updated_time]),

                % Update the list of events by removing the sent events
                

                % Return the updated events and time
                
                {{Updated_events, Updated_time}, true}.




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

                                                            

    
