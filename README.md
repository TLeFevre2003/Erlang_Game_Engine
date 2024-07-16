# ErlangGameEngine
Game engine made in Erlang

How to try:

cd game_engine
rebar3 shell
game_engine:start_engine([{0, fun(Current_state) -> io:format("Running event ~p ~p ~n ",[Current_state,1]),Current_state+1 end},{2, fun(Current_state) -> io:format("Running event~p ~p ~n ",[Current_state,2]),Current_state+1 end}]).
