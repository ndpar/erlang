%
% F.Cesarini & S.Thomson, Erlang Programming, p.169.
% Exercise 7-4: Records and Shapes
%
-module(shapes_test).
-export([test/0]).
-include("Shapes.hrl").

test() ->
    area_circle(),
    area_rectangle(),
    area_triangle(),
    perimeter_circle(),
    perimeter_rectangle(),
    perimeter_triangle(),
    ok.

area_circle() ->
    Circle = #circle{radius = 10},
    Area = math:pi() * 100,
    Area = shapes:area(Circle).

area_rectangle() ->
    Rectangle = #rectangle{length = 10, width = 5},
    50 = shapes:area(Rectangle).

area_triangle() ->
    Triangle = #triangle{a = 3, b = 4, c = 5},
    6.0 = shapes:area(Triangle).

perimeter_circle() ->
    Circle = #circle{radius = 10},
    Perimeter = math:pi() * 20,
    Perimeter = shapes:perimeter(Circle).

perimeter_rectangle() ->
    Rectangle = #rectangle{length = 10, width = 5},
    30 = shapes:perimeter(Rectangle).

perimeter_triangle() ->
    Triangle = #triangle{a = 5, b = 6, c = 7},
    18 = shapes:perimeter(Triangle).
