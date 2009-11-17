-module(my_lib).
-export([even/1]).

even(Int) when Int rem 2 == 0 -> true; 
even(Int) when Int rem 2 == 1 -> false.

%END:area
