-module(cel).
-export([handle_request/1, add_event/2, run_events/1, start/1, get_game_state/1]).


%% @private
%% Handles incoming requests for game state management.
%% complexity O(n) where n is the number of events in the list to be processed or added. 
%%
%% @param State A tuple representing the current state, which includes the game state, the list of events, and the process identifier of the FEL process.
%% @return No return value as this function calls itself recursively to maintain the state machine loop.
-spec handle_request({any(), list(fun()), pid()}) -> no_return().
handle_request({Game_state, Events, Fel_pid}) ->
    {Updated_game_state, Continue} = receive
        {add, Event} ->
            {{Game_state, [Event | Events], Fel_pid},true};
        {get_game_state, Pid} ->
            Pid ! Game_state,
            {{Game_state, Events, Fel_pid},true};
        run ->
            % Run all of Cel events
            io:format("cel run ~p ~p~n", [Game_state, Events]),
            Current_state = lists:foldl(fun({_, Event}, Current_game_state) -> Event(Current_game_state) end, Game_state, Events),
            io:format("Folded value ~p~n",[Current_state]),

            % Get the next events from the Fel
            Fel_pid ! {get_next, self()},
            receive
                {add, New_events} ->
                    io:format("cel add command recieved ~p ~p~n", [Game_state, New_events]),
                    case New_events of
                        [] -> ok;
                        _ -> self() ! run
                    end,
                    {{Current_state, New_events, Fel_pid}, true};
                _ -> 
                    io:format("cel else ~p ~p~n", [Game_state, Events]),
                    {{Current_state, [], Fel_pid}, true}
            end;
        stop -> {Game_state, false}
    end,
    case Continue of
        true -> handle_request(Updated_game_state);
        _ -> ok
    end.


%% API FUNCTIONS


%% @doc
%% Adds an event to the game state handling process.
%% complexity O(1)
%%
%% @param Pid The process identifier of the game state handling process.
%% @param Fun The function to be added as an event.
%% @return ok if the event is successfully added to the process queue.
-spec add_event(pid(), fun()) -> ok.
add_event(Pid,Fun) ->
    Pid ! {add, Fun}.

%% @doc
%% Retrieves the current game state from the game state handling process.
%% complexity O(1)
%%
%% @param Pid The process identifier of the game state handling process.
%% @return The current game state held by the process handling game state updates. The type of the game state is dynamic and can be any() depending on the implementation details of the game state updates. 
-spec get_game_state(pid()) -> any().
get_game_state(Pid) ->
    Pid ! {get_game_state, self()},
    receive
        Game_state ->
            Game_state 
    end.

%% @doc
%% Runs all scheduled events in the game state handling process.
%% complexity O(n) where n is the number of events to be executed in the game state.
%%
%% @param Pid The process identifier of the game state handling process.
%% @return ok if the events are successfully run by the process handling game state updates. 
-spec run_events(pid()) -> ok.
run_events(Pid) ->
    Pid ! {run, self()}.

%% @doc
%% Starts the game state handling process with the given FEL process identifier.
%% complexity O(1)
%%
%% @param Fel_pid The process identifier of the FEL (Future Event List) process.
%% @return Pid of the spawned process handling game state.

-spec start(pid()) -> pid().
start(Fel_pid) ->
    spawn(fun() -> handle_request({0, [], Fel_pid}) end).