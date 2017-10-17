-module(calculator).

%% API exports
-export([
         eval_expression/1,
         parse_expression/1,
         validate_input/1
        ]).

-define(open_brace, $().
-define(close_brace, $)).

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
  Plus = $+,
  Minus = $-,
  Multiply = $*,
  Divide = $/,

  case H of
    ?open_brace ->
      parse_expression(T, [?open_brace|OperStack], DataStack);
    ?close_brace ->
      Operands = [{num, Num} || Num <- DataStack],
      list_to_tuple([hd(OperStack) | Operands]);
    Plus ->
      parse_expression(T, [plus|OperStack], DataStack);
    Minus ->
      parse_expression(T, [minus|OperStack], DataStack);
    Multiply ->
      parse_expression(T, [multiply|OperStack], DataStack);
    Divide ->
      parse_expression(T, [divide|OperStack], DataStack);
    Val when Val >= $0, Val =< $9 ->
      parse_expression(T, OperStack, [list_to_integer([Val])|DataStack]);
    _ ->
      parse_expression(T, OperStack, DataStack)
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
  case H of
    ?open_brace  -> validate_input(T, [?open_brace|Stack]);
    ?close_brace ->
      if
        hd(Stack) =:= ?open_brace -> validate_input(T, tl(Stack));
        true                    -> {error, braces_error}
      end;
    _  -> validate_input(T, Stack)
  end.

