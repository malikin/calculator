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
  parse_expression(Expr, [], {}).

validate_input(String) ->
  validate_input(String, []).

%%====================================================================
%% Internal functions
%%====================================================================

parse_expression([], CallStack, _Result) ->
  {_NewStack, NewResult} = prepare_result(CallStack, {}, []),
  extract_result(NewResult);

parse_expression([H|T], CallStack, Result) ->
  case H of
    ?open_brace ->
      parse_expression(T, [?open_brace|CallStack], Result);
    ?close_brace ->
      parse_expression(T, [?close_brace|CallStack], Result);
    Operator when Operator =:= ?minus;
                  Operator =:= ?plus;
                  Operator =:= ?multiply;
                  Operator =:= ?divide
                  ->
      parse_expression(T, [get_operator(Operator)|CallStack], Result);
    Val when Val >= $0, Val =< $9 ->
      parse_expression(T, [{num, list_to_integer([Val])}|CallStack], Result);
    _ ->
      parse_expression(T, CallStack, Result)
  end.

extract_result({Result}) ->
  Result;
extract_result(Result) ->
  Result.

% prepare_result(Stack, Result) ->
  % prepare_result(Stack, Result, []).

prepare_result([], Result, _Buffer) ->
  {[], Result};

prepare_result(Stack, Result, Buffer) ->
  % erlang:display(Stack),
  % erlang:display(Buffer),
  case hd(Stack) of
    Operator when Operator =:= minus;
                  Operator =:= plus;
                  Operator =:= multiply;
                  Operator =:= divide
                  ->
      prepare_result(tl(Stack), erlang:insert_element(1, Result, hd(Stack)), Buffer);
    ?open_brace ->
      BufferSize = length(Buffer),
      if
        BufferSize < 2 ->
         TmpResult = erlang:insert_element(2, Result, hd(Buffer)),
         {NextStack, NextNode} = prepare_result(tl(Stack), {}, []),
         erlang:display(NextNode),
         {NextStack, erlang:insert_element(2, TmpResult, NextNode)};
        true ->
          NewResult = lists:foldl(
                    fun(Element, Acc) ->
                        erlang:insert_element(2, Acc, Element)
                    end,
                    Result,
                    Buffer
                   ),
          {tl(Stack), {NewResult}}
      end;
    ?close_brace ->
      prepare_result(tl(Stack), Result, Buffer);
    _ ->
      prepare_result(tl(Stack), Result, lists:reverse([(hd(Stack))|Buffer]))
  end.

get_operator(Operator) ->
  case Operator of
    ?plus     -> plus;
    ?minus    -> minus;
    ?multiply -> multiply;
    ?divide   -> divide
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

