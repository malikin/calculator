-module(calculator_eunit).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

parse_expression_test() ->
  ?assertEqual({plus, {num, 1}, {num, 1}}, calculator:parse_expression("(1+1)")),
  ?assertEqual({plus, {num, 1}, {num, 2}}, calculator:parse_expression("(1 + 2)")),
  ?assertEqual(
    {minus, {plus, {num, 2}, {num, 3}}, {num, 4}},
    calculator:parse_expression("((2 + 3) - 4)")
  ),
  ?assertEqual(
    {minus, {num, 4}, {plus, {num, 2}, {num, 3}}},
    calculator:parse_expression("(4 - (2 + 3))")
  ),
  ?assertEqual(
    {plus, {plus, {num, 1}, {num, 1}}, {plus, {num, 2}, {num, 2}}},
    calculator:parse_expression("((1+1)+(2+2))")
  ),
  ?assertEqual({minus, {num, 1}, {num, 1}}, calculator:parse_expression("(1-1)")),
  ?assertEqual({minus, {num, 1}, {num, 1}}, calculator:parse_expression("(1 - 1)")),
  ?assertEqual({multiply, {num, 1}, {num, 1}}, calculator:parse_expression("(1*1)")),
  ?assertEqual({divide, {num, 1}, {num, 1}}, calculator:parse_expression("(1/1)")).

eval_expresstion_test() ->
  ?assertEqual(2, calculator:eval_expression({plus, {num, 1}, {num, 1}})),
  ?assertEqual(0, calculator:eval_expression({minus, {num, 1}, {num, 1}})).

validate_input_test() ->
  ?assertEqual({ok, valid}, calculator:validate_input("(1 + 2 - (0 + 1))")),
  ?assertEqual({error, braces_error}, calculator:validate_input("(1 + 1))")),
  ?assertEqual({error, braces_error}, calculator:validate_input("((1 + 1)")).
