-module(game_engine).

-export([]).




cel({Game_state, Events})->
    Updated_game_state = receive
            {add, Event}->
                {Game_state, [Event] ++ Events};
            run->
                {lists:foldl(fun(Event,Current_game_state)->Event(Current_game_state) end, Game_state, Events), []}
        end,
    cel(Updated_game_state).




