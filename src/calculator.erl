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
  {Result} = prepare_result(Tokenized, {}, []),
  Result.

tokenize_expression(Expression) ->
  tokenize_expression(Expression, []).

validate_input(String) ->
  validate_input(String, []).

%%====================================================================
%% Internal functions
%%====================================================================

tokenize_expression([], Result) ->
  lists:reverse(Result);

tokenize_expression([H|T], Result) ->
  case H of
    Val when Val >= $0, Val =< $9 ->
      tokenize_expression(T, [{num, list_to_integer([Val])}|Result]);
    $\s ->
      tokenize_expression(T, Result);
    Val ->
      tokenize_expression(T, [get_token(Val)|Result])
  end.

get_token(Operator) ->
  case Operator of
    ?plus        -> plus;
    ?minus       -> minus;
    ?multiply    -> multiply;
    ?divide      -> divide;
    ?open_brace  -> open_brace;
    ?close_brace -> close_brace
  end.

prepare_result([], _Result, Buffer) ->
  hd(Buffer);

prepare_result(Stack, Result, Buffer) ->
  % erlang:display(Stack),
  % erlang:display(Buffer),
  case hd(Stack) of
    Operator when Operator =:= minus;
                  Operator =:= plus;
                  Operator =:= multiply;
                  Operator =:= divide
                  ->
      BufferSize = length(Buffer),
      if
        BufferSize > 0 ->
          {Operator, hd(Buffer), prepare_result(tl(Stack), Result, Buffer)};
        true ->
          {Operator, prepare_result(tl(Stack), Result, Buffer)}
      end;
    open_brace ->
      {prepare_result(tl(Stack), Result, Buffer)};
    close_brace ->
      prepare_result(tl(Stack), Result, Buffer);
    _ ->
      prepare_result(tl(Stack), Result, [(hd(Stack))|Buffer])
  end.

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

