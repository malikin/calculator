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

parse_expression(Expr) ->
  parse_expression(Expr, [], []).

validate_input(String) ->
  validate_input(String, []).

%%====================================================================
%% Internal functions
%%====================================================================

parse_expression([H|T], OperStack, DataStack) ->
  OpenBrace  = 40,
  CloseBrace = 41,
  erlang:display(H),
  erlang:display(T),
  case H of
    OpenBrace  -> parse_expression(T, [OpenBrace|OperStack], DataStack);
    CloseBrace -> {hd(OperStack), [{num, Num} || Num <- DataStack]};
    "+" -> parse_expression(T, [plus|OperStack], DataStack);
    Val -> parse_expression(T, OperStack, [Val, DataStack])
  end.

eval_expession_stack({plus, {num, X}, {num, Y}}, _Stack) ->
  X + Y;

eval_expession_stack({minus, {num, X}, {num, Y}}, _Stack) ->
  X - Y.

%% Validate input, check braces input balance

validate_input([], []) ->
  {ok, valid};

validate_input([], _Stack) ->
  {error, braces_error};

validate_input([H|T], Stack) ->
  OpenBrace  = 40,
  CloseBrace = 41,
  case H of
    OpenBrace  -> validate_input(T, [OpenBrace|Stack]);
    CloseBrace ->
      if
        hd(Stack) =:= OpenBrace -> validate_input(T, tl(Stack));
        true                    -> {error, braces_error}
      end;
    _  -> validate_input(T, Stack)
  end.

