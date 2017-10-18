-module(calculator).

%% API exports
-export([
         eval_expression/1,
         parse_expression/1,
         validate_input/1
        ]).

-define(open_brace, $().
-define(close_brace, $)).
-define(plus, $+).
-define(minus, $-).
-define(multiply, $*).
-define(divide, $/).

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

%% Parse string expression to ast
parse_expression([], _CallStack, DataStack) ->
  hd(DataStack);

parse_expression([H|T], CallStack, DataStack) ->
  case H of
    ?open_brace ->
      parse_expression(T, [?open_brace|CallStack], DataStack);
    ?close_brace ->
      case is_operator(hd(CallStack)) of
        true ->
          Operands = [Num || Num <- DataStack],
          Expression = list_to_tuple([hd(CallStack) | lists:reverse(Operands)]),
          % erlang:display(Expression),
          parse_expression(T, tl(CallStack), [Expression]);
        false ->
          parse_expression(T, tl(CallStack), DataStack)
      end;
    Operator when Operator =:= ?minus;
                  Operator =:= ?plus;
                  Operator =:= ?multiply;
                  Operator =:= ?divide
                  ->
      parse_expression(T, [get_operator(Operator)|CallStack], DataStack);
    Val when Val >= $0, Val =< $9 ->
      parse_expression(T, CallStack, [{num, list_to_integer([Val])}|DataStack]);
    _ ->
      parse_expression(T, CallStack, DataStack)
  end.

get_operator(Operator) ->
  case Operator of
    ?plus     -> plus;
    ?minus    -> minus;
    ?multiply -> multiply;
    ?divide   -> divide
  end.

is_operator(Operator) ->
  lists:member(Operator, [plus, minus, multiply, divide]).

%% Eval parsed expressions from ast
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
  case H of
    ?open_brace  -> validate_input(T, [?open_brace|Stack]);
    ?close_brace ->
      if
        hd(Stack) =:= ?open_brace -> validate_input(T, tl(Stack));
        true                      -> {error, braces_error}
      end;
    _  -> validate_input(T, Stack)
  end.

