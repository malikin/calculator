-module(calculator).

%% API exports
-export([
         eval_expression/1,
         parse_expression/1,
         validate_input/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

eval_expression(Expression) ->
  eval_expession_stack(Expression, []).

parse_expression(_Expr) -> ok.

validate_input(String) ->
  validate_input(String, 0).

%%====================================================================
%% Internal functions
%%====================================================================

eval_expession_stack({plus, {num, X}, {num, Y}}, _Stack) ->
  X + Y.

%% Validate input, check braces input balance

validate_input([], 0) ->
  {ok, valid};

validate_input([], _) ->
  {error, braces_error};

validate_input([H|T], Acc) ->
  case H of
    40 -> validate_input(T, Acc + 1); % Open brace
    41 -> validate_input(T, Acc - 1); % Close brace
    _  -> validate_input(T, Acc)
  end.
