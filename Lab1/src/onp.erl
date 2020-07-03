-module(onp).
-export([onp/1]).

onp_helper([], []) ->
  "Podano puste wyrażenie";
onp_helper([], Stack) when length(Stack) == 1 ->
  hd(Stack);
onp_helper([], _) ->
  "Podano za duzo operatorow/za malo argumentow";
onp_helper(["+"|T], [F,S|Stack]) ->
  onp_helper(T,[S+F|Stack]);
onp_helper(["-"|T], [F,S|Stack]) ->
  onp_helper(T,[S-F|Stack]);
onp_helper(["*"|T], [F,S|Stack]) ->
  onp_helper(T,[S*F|Stack]);
onp_helper(["/"|_], [0,_|_]) ->
  "Nie mozna dzielic przez zero";
onp_helper(["/"|T], [F,S|Stack]) ->
  onp_helper(T,[S/F|Stack]);
onp_helper(["pow"|T], [F,S|Stack]) ->
  onp_helper(T,[math:pow(S,F)|Stack]);
onp_helper(["^"|T], [F,S|Stack]) ->
  onp_helper(T,[math:pow(S,F)|Stack]);
onp_helper(["sqrt"|T], [F|Stack]) ->
  onp_helper(T,[math:sqrt(F)|Stack]);
onp_helper(["sin"|T], [F|Stack]) ->
  onp_helper(T,[math:sin(F)|Stack]);
onp_helper(["cos"|T], [F|Stack]) ->
  onp_helper(T,[math:cos(F)|Stack]);
onp_helper(["tan"|T], [F|Stack]) ->
  onp_helper(T,[math:tan(F)|Stack]);
onp_helper(["epx"|T], [F|Stack]) ->
  onp_helper(T,[math:exp(F)|Stack]);
onp_helper(["pi"|T], Stack) ->
  onp_helper(T,[math:pi()|Stack]);
onp_helper(["e"|T], Stack) ->
  onp_helper(T,[math:exp(1)|Stack]);
onp_helper([H|T], Stack) ->
  onp_helper(T, [H|Stack]).

get_numbers([]) -> [];
get_numbers([H|T]) when is_number(H) ->
  [H|get_numbers(T)];
get_numbers(["+"|T]) ->
  ["+"|get_numbers(T)];
get_numbers(["-"|T]) ->
  ["-"|get_numbers(T)];
get_numbers(["*"|T]) ->
  ["*"|get_numbers(T)];
get_numbers(["/"|T]) ->
  ["/"|get_numbers(T)];
get_numbers(["pow"|T]) ->
  ["pow"|get_numbers(T)];
get_numbers(["^"|T]) ->
  ["^"|get_numbers(T)];
get_numbers(["sqrt"|T]) ->
  ["sqrt"|get_numbers(T)];
get_numbers(["sin"|T]) ->
  ["sin"|get_numbers(T)];
get_numbers(["cos"|T]) ->
  ["cos"|get_numbers(T)];
get_numbers(["tan"|T]) ->
  ["tan"|get_numbers(T)];
get_numbers(["exp"|T]) ->
  ["exp"|get_numbers(T)];
get_numbers(["pi"|T]) ->
  ["pi"|T];
get_numbers(["e"|T]) ->
  ["e"|T];
get_numbers([H|T]) ->
  %[list_to_integer(H)|get_numbers(T)].
  case string:to_float(H) of
    {error, no_float} ->
      case string:to_integer(H) of
        {error, no_integer} -> exit("Argument not float and not int!");
        {Int, _} -> [Int | get_numbers(T)]
      end;
    {Float, _} -> [Float | get_numbers(T)]
end.

onp(Expression) ->
  onp_helper(get_numbers(string:tokens(Expression, " ")), []).
