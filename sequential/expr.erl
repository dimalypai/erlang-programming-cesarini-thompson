-module(expr).
-export([ parse/1
        , eval/1
        , print/1
        , compile/1
        , interpret/1
        , simplify/1
        ]).

% Parse a given string according to the grammar:
% expr ::= (sum) | number
% sum ::= expr + expr
% Returns an AST as a tuple.
parse(ExprStr) ->
  {Expr, []} = parseExpr(ExprStr),
  Expr.

parseExpr([ $( | ExprStr ]) ->
  {Expr, [ $) | ExprStr1 ]} = parseSum(ExprStr),
  {Expr, ExprStr1};
parseExpr(ExprStr) -> parseNumber(ExprStr).

parseSum(ExprStr) ->
  {Expr1, ExprStr1} = parseExpr(ExprStr),
  case ExprStr1 of
    [ $+ | ExprStr2 ] ->
      {Expr2, ExprStr3} = parseExpr(ExprStr2),
      Expr = {plus, Expr1, Expr2},
      {Expr, ExprStr3};
    _ -> {Expr1, ExprStr1}
  end.

parseNumber([H|T]) when H >= $0, H =< $9 -> {{num, H - $0}, T}.

% Evaluates a given AST.
eval({num, N}) -> N;
eval({plus, E1, E2}) -> eval(E1) + eval(E2).

% AST pretty printing.
print(Expr) ->
  prprint(Expr),
  io:format("~n", []).

% Core pretty printing function.
prprint({num, N}) -> io:format("~w", [N]);
prprint({plus, E1, E2}) ->
  io:format("(", []),
  prprint(E1),
  io:format("+", []),
  prprint(E2),
  io:format(")", []).

% Expression compiler for a stack machine.
% Instruction set: {push, N}, add.
compile({num, N}) -> [{push, N}];
compile({plus, E1, E2}) ->
  compile(E1) ++ compile(E2) ++ [add].

% Stack machine core. Interprets a given list of instructions.
interpret(Instrs) -> interpret_helper(Instrs, []).

% Stack machine helper. Has instructions and a stack as parameters.
interpret_helper([], [Value]) -> Value;
interpret_helper([ Instr | Instrs ], Stack) ->
  case Instr of
    {push, N} -> interpret_helper(Instrs, [ N | Stack ]);
    add ->
      [N1,N2 | Stack1] = Stack,
      Value = N1 + N2,
      interpret_helper(Instrs, [ Value | Stack1 ])
  end.

% Expression simplifier.
simplify({plus, {num, 0}, E2}) -> simplify(E2);
simplify({plus, E1, {num, 0}}) -> simplify(E1);
simplify({plus, E1, E2}) -> {plus, simplify(E1), simplify(E2)};
simplify(Expr) -> Expr.

