-module(myLists).
-export([contains/2,duplicateElements/1,sumFloats/1,sumFloats_tail/2, contains2/2]).


contains([], _) ->
  false;
contains([H|T],Value) ->
  if
    H == Value -> true;
    true -> contains(T,Value)
  end.

contains2([], _) ->
  false;
contains2([Value|_], Value) ->
  true;
contains2([_|T], Value) ->
  contains2(T, Value).

duplicateElements([]) ->
  [];
duplicateElements([H|T]) ->
  [H,H] ++ duplicateElements(T).

sumFloats([]) ->
  0.0;
sumFloats([H|T]) ->
  if
    is_float(H) -> H + sumFloats(T);
    true -> sumFloats(T)
  end.


sumFloats_tail([], Sum) ->
  Sum;
sumFloats_tail([H|T], Sum) ->
  if
    is_float(H) -> sumFloats_tail(T, Sum+H);
    true -> sumFloats_tail(T, Sum)
  end.