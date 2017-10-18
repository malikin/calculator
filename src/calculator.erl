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
  parse_expression(Expr, [], [], {}).

validate_input(String) ->
  validate_input(String, []).

%%====================================================================
%% Internal functions
%%====================================================================

%% Parse string expression to ast
parse_expression([], _CallStack, _DataStack, {Result}) ->
  erlang:display(Result),
  Result;
parse_expression([], _CallStack, _DataStack, Result) ->
  erlang:display(Result),
  Result;

parse_expression([H|T], CallStack, DataStack, Result) ->
  case H of
    ?open_brace ->
      parse_expression(T, [?open_brace|CallStack], DataStack, Result);
    ?close_brace ->
      case is_operator(hd(CallStack)) of
        false ->
          parse_expression(T, tl(CallStack), DataStack, Result);
        true ->
          Operands      = [Element || Element <- DataStack],
          OperandsCount = length(Operands),
          case OperandsCount of
            0 ->
              parse_expression([H|T], tl(CallStack), [], erlang:insert_element(1, Result, hd(CallStack)));
            OperandsCountOne when OperandsCountOne =:= 1 ->
              NewResult = erlang:insert_element(1, Result, hd(CallStack)),
              parse_expression(
                [H|T],
                tl(CallStack),
                [],
                erlang:insert_element(tuple_size(NewResult) + 1, NewResult, hd(Operands))
               );
            _ ->
              Expression = list_to_tuple([hd(CallStack)|lists:reverse(Operands)]),
              parse_expression(
                [H|T],
                tl(CallStack),
                [],
                erlang:insert_element(tuple_size(Result) + 1, Result, Expression)
               )
          end
      end;
    Operator when Operator =:= ?minus;
                  Operator =:= ?plus;
                  Operator =:= ?multiply;
                  Operator =:= ?divide
                  ->
      parse_expression(T, [get_operator(Operator)|CallStack], DataStack, Result);
    Val when Val >= $0, Val =< $9 ->
      parse_expression(T, CallStack, [{num, list_to_integer([Val])}|DataStack], Result);
    _ ->
      parse_expression(T, CallStack, DataStack, Result)
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

