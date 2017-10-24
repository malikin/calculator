-module(calculator).

%% API exports
-export([
         eval_expression/1,
         parse_expression/1,
         validate_input/1,
         tokenize_expression/1
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

parse_expression(Expression) ->
  Tokenized = tokenize_expression(Expression),
  prepare_result(Tokenized, [], []).

tokenize_expression(Expression) ->
  tokenize_expression(Expression, [], []).

validate_input(String) ->
  validate_input(String, []).

%%====================================================================
%% Internal functions
%%====================================================================

tokenize_expression([], Result, _NumBuffer) ->
  lists:reverse(Result);

tokenize_expression([H|T], Result, NumBuffer) ->
  case H of
    Val when Val >= $0, Val =< $9 ->
      tokenize_expression(T, Result, [Val|NumBuffer]);
    $\s ->
      {NewResult, NewNumBuffer} = check_num_buffer(Result, NumBuffer),
      tokenize_expression(T, NewResult, NewNumBuffer);
    Val ->
      {NewResult, NewNumBuffer} = check_num_buffer(Result, NumBuffer),
      tokenize_expression(T, [get_token(Val)|NewResult], NewNumBuffer)
  end.

check_num_buffer(Result, []) ->
  {Result, []};

check_num_buffer(Result, NumBuffer) ->
  {[{num, list_to_integer(lists:reverse(NumBuffer))}|Result], []}.

get_token(Operator) ->
  case Operator of
    ?plus        -> plus;
    ?minus       -> minus;
    ?multiply    -> multiply;
    ?divide      -> divide;
    ?open_brace  -> open_brace;
    ?close_brace -> close_brace
  end.

prepare_result([], _OpStack, Result) -> hd(Result);

prepare_result(Input, OpStack, Result) ->
  case hd(Input) of
    {num, _} = Val -> prepare_result(tl(Input), OpStack, [Val|Result]);
    Operator when Operator =:= minus;
                  Operator =:= plus;
                  Operator =:= multiply;
                  Operator =:= divide
                  ->
      prepare_result(tl(Input), [Operator|OpStack], Result);
    open_brace -> prepare_result(tl(Input), OpStack, Result);
    close_brace ->
      {NewResult, Operands} = extract_operands(Result, []),
      prepare_result(tl(Input), tl(OpStack), [list_to_tuple([hd(OpStack) | Operands]) | NewResult])
  end.

extract_operands(Stack, Operands = [_,_]) -> {Stack, Operands};

extract_operands([H|T], Operands) ->
  extract_operands(T, [H|Operands]).

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

