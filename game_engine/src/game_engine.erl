-module(game_engine).
-export([pid_tracker/2, start_engine/1, stop_engine/0]).

% 1> game_engine:start_engine([{0, fun(Current_state) -> io:format("Running event ~p ~p ~n ",[Current_state,1]),Current_state+1 end},{2, fun(Current_state) -> io:format("Running event~p ~p ~n ",[Current_state,2]),Current_state+1 end}]).


start_engine(Initial_events) ->
    Fel_pid = fel:start(),
    Cel_pid = cel:start(Fel_pid),
    Fel_pid ! {add, Initial_events},
    io:format("Fel_pid ~p~n", [Fel_pid]),
    io:format("Cel_pid ~p~n", [Cel_pid]),
    Cel_pid ! run,
    Pid_tracker = spawn(fun() -> pid_tracker(Cel_pid, Fel_pid) end),
    register(pid_tracker, Pid_tracker).

stop_engine() ->
    pid_tracker ! stop.




pid_tracker(Cel_pid, Fel_pid) ->
    Keep_going = receive 
        {get_cel, Pid} -> Pid ! Cel_pid, true;
        {get_fel, Pid} -> Pid ! Fel_pid, true;
        stop -> Cel_pid ! stop, Fel_pid ! stop, false
    end,
    case Keep_going of 
        true -> pid_tracker(Cel_pid, Fel_pid);
        _ -> ok
    end.