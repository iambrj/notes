-module(ch1).
% only exported functions are available outside module
-export([factorial/1]).
-export([area/1]).

% Use pattern matching for choice
factorial(0) -> 1; % note ; use for peicewise functions
factorial(N) -> N * factorial(N - 1).

% Use pattern matching for choice
area({circle, Radius}) -> % Variables should start with uppercase letter
    3.14 * Radius * Radius;
area({square, Side}) -> % Tokens starting with lowercase letters are atoms
    Side * Side;
area({rectangle, Length, Breadth}) ->
    Length * Breadth;
area({triangle, A, B ,C}) ->
    S = (A + B + C) / 2, % use comma to perform sequencing
    math:sqrt(S * (S - A) * (S - B) * (S - C)).
