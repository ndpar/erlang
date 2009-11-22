%
% F.Cesarini & S.Thomson, Erlang Programming, p.169.
% Exercise 7-4: Records and Shapes
%
-module(shapes).
-export([area/1, perimeter/1]).
-include("Shapes.hrl").

area(Shape) when is_record(Shape, circle) ->
    #circle{radius = Radius} = Shape,
    math:pi() * Radius * Radius;

area(Shape) when is_record(Shape, rectangle) ->
    #rectangle{length = Length, width = Width} = Shape,
    Length * Width;

area(Shape) when is_record(Shape, triangle) ->
    #triangle{a = A, b = B, c = C} = Shape,
    S = perimeter(Shape) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

perimeter(Shape) when is_record(Shape, circle) ->
    #circle{radius = Radius} = Shape,
    2 * math:pi() * Radius;

perimeter(Shape) when is_record(Shape, rectangle) ->
    #rectangle{length = Length, width = Width} = Shape,
    2 * (Length + Width);

perimeter(Shape) when is_record(Shape, triangle) ->
    #triangle{a = A, b = B, c = C} = Shape,
    A + B + C.
