# Mastermind

Erlang implementation of [Matermind](http://en.wikipedia.org/wiki/Mastermind_(board_game)) game.

## Build

    $ rebar3 compile

## Play

    $ rebar3 shell

    1> codemaker:start().
    true
    2> codemaker:guess([1,1,2,2]).
    {0,0}
