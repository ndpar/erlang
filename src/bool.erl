%%
%% Erlang Programming, p.44.
%% Exercise 2-3: Simple Pattern Matching
%%
-module(bool).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

b_not(true) -> false;
b_not(false) -> true.

b_and(true,true) -> true;
b_and(_,_) -> false.

b_or(false,false) -> false;
b_or(_,_) -> true.

b_nand(X,Y) -> b_not(b_and(X,Y)).

%% Solution from: http://groups.google.com/group/erlang-programming-book/browse_thread/thread/e95f323f72c72327#
% b_and(true,  true)  -> true; 
% b_and(false, true)  -> false; 
% b_and(true,  false) -> false; 
% b_and(false, false) -> false. 
% b_or(true,  true)  -> true; 
% b_or(false, true)  -> true; 
% b_or(true,  false) -> true; 
% b_or(false, false) -> false. 
