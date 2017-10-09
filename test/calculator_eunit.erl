-module(calculator_eunit).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

eval_expresstion_test() ->
  ?assertEqual(2, calculator:eval_expression({plus, {num, 1}, {num, 1}})),
  ?assertEqual(0, calculator:eval_expression({minus, {num, 1}, {num, 1}})).

validate_input_test() ->
  ?assertEqual({ok, valid}, calculator:validate_input("(1 + 2 - (0 + 1))")),
  ?assertEqual({error, braces_error}, calculator:validate_input("(1 + 1))")).
