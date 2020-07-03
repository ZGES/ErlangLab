-module(power).
-export([power/2]).

power(Base,0) ->
  1;
power(Base,Index) ->
  Base * power(Base,Index-1).

