# Erlang Game Engine

This project implements a basic game engine in Erlang.

## Overview

The Erlang Game Engine provides a framework for creating and managing game events. It allows developers to define custom events that manipulate the game state and perform actions.

## Features

- **Event-Driven:** Define events with custom actions to control game flow.
- **State Management:** Manage and update game state dynamically.
- **Interactive Shell:** Utilize Erlang's shell for real-time interaction and testing.

## Getting Started

To use the game engine, follow these steps:

1. **Navigate to the Project Directory:**
cd game_engine
2. **Start the Erlang Shell:**
rebar3 shell

3. **Start the Game Engine:**
```erlang
game_engine:start_engine([
    {0, fun(Current_state) ->
            io:format("Running event ~p ~p ~n ", [Current_state, 1]),
            Current_state + 1
        end},
    {2, fun(Current_state) ->
            io:format("Running event ~p ~p ~n ", [Current_state, 2]),
            Current_state + 1
        end}
]).
