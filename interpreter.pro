%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpreter!
%% Olivia Lucca Fraser
%%
%% Fri Apr 15 20:58:10 ADT 2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('symbols.pro').

%% each function entry is a triple:
%% rettype, paramtypes, body
%% the body is the 'expression'

%% succeed if the variable denotes a value of the specified
%% type, and fail otherwise.
type_check(Var, int) :-
    valueHandler(Var, Val),
    number(Val),!.

type_check(Var, bool) :-
    valueHandler(Var, Val),
    number(Val),
    ((Val is 1,!) ; (Val is 0,!)).

%% type-check the arguments given, and enter them into the
%% symbol table with their corresponding values
process_args([],[]).

process_args([Arg|RestArgs], [Type,Pvar|RestSig]) :-
    valueHandler(Arg, Val),
    type_check(Val, Type),
    add_symbol(Pvar,Val),
    process_args(RestArgs,RestSig).

%% clean up the parameter stack. call this when exiting a function
clear_args_from_stack([]).

clear_args_from_stack([_,Pvar|RestSig]) :-
    remove_symbol(Pvar),
    clear_args_from_stack(RestSig).

%% ------------------------------------------------------------
%% expression handlers. this is where the heavy lifting's done
%% there are quite a few of these clauses, and the syntax gets
%% a bit hairy. first place to go looking for bugs, imo.
%% ------------------------------------------------------------

%% for LET expressions
expression_handler([[let], [Var], [=], Val, [in], Exp], Result) :-
    format('inside let expression...~n'), 
    valueHandler(Val, Value),
    add_symbol(Var, Value),
    expression_handler(Exp, Result),
    remove_symbol(Var),!.

%% for IF expressions
expression_handler([[if], Ante, [then], Cons, [else], Alt], Result) :-
    format('evaluating conditional. Ante: ~w ~n',[Ante]),
    ( expression_handler(Ante, AnteRet),
      format('antecedent returned: ~w~n',[AnteRet]),
      is(AnteRet, 1),
      expression_handler([Cons], Result), !) ;
    expression_handler(Alt, Result),!.
    
%% for function-calling expressions
expression_handler([[[Fn], [['('],Rest,[')']]]|_],
                      ReturnValue) :-
    format('processing function: ~a\n',Fn),
    flatten(Rest, FlatRest),
    subtract(FlatRest, [','], CleanArgs),
    get_symbol(Fn, [ReturnType, Signature, Body]),
    ( process_args(CleanArgs, Signature);
      format('Error: arguments for ~a do not match type signature.\n',
             Fn),!), %% works up to here!
    %%% The parameters are now on the stack. We're ready to
    %%% call the function. 
    expression_handler(Body, ReturnValue),
    ( type_check(ReturnValue, ReturnType);
      format('Error: bad return type from ~a\n', Fn),! ),
    %%% epilogue: clean up the stack
    clear_args_from_stack(Signature),!.

%% for arithmetical operations and logical comparisons
expression_handler([Value, ExtraExpression], Result) :-
    format('handling expression: ~w~n', [[Value, ExtraExpression]]),
    valueHandler(Value, ValueResult),
    format('ValueResult = ~d~n', [ValueResult]),
    format('ExtraExpression = ~w~n', [ExtraExpression]),
    extra_expression_handler(ExtraExpression, ExtraResult),
    evaluate(ValueResult, ExtraResult, Result),!.

% a value is also a (trivial) expression
expression_handler(Value, Result) :-
    valueHandler(Value,Result).

extra_expression_handler([], _) :- !.
%    format('hello from the tiny part of the eeh~n'),!.

extra_expression_handler([[Op], Value], [Op, ValueResult]) :-
%    format('hello from the extra_expression_handler\n'),
    valueHandler(Value, ValueResult), !.
%% it works!

%% values will typically be stored in the form [[val],[]].
%% and if val is an integer, it will be quoted. We need to
%% unpack this, and that's what we do here.
%% it's a good idea for this operation to be idempotent, to
%% guard against various bugs. 
valueHandler(Value, ValueResult) :-
    atom(Value),
    atom_number(Value, Number),
    is(ValueResult,Number),!.

valueHandler(Value, ValueResult) :-
    number(Value),
    is(ValueResult,Value),!.

valueHandler(Value, ValueResult) :-
    (not(number(Value)),
     atom(Value),
     get_symbol(Value, ValueResult), !).

valueHandler([[Val], []], Result) :-
    valueHandler(Val, Result),!.

%% these clauses evaluate arithmetical and logical expressions
%% using the underlying logical and arithmetical machinery in prolog
evaluate(Value1, ['+', Value2], Result) :-
    is(Result, +(Value1, Value2)).

evaluate(Value1, ['==', Value2], Result) :-
    format('comparing ~d and ~d...\n', [Value1, Value2]),
    valueHandler(Value1, Evaled1),
    valueHandler(Value2, Evaled2),
    (is(Evaled1, Evaled2), format('yes they are the same\n'),
     is(Result, 1),!);
    is(Result, 0),!.

evaluate(Value1, ['>', Value2], Result) :-
    (>(Value1, Value2),
     is(Result, 1),!);
    is(Result, 0),!.

evaluate(Value1, ['>=', Value2], Result) :-
    (>=(Value1, Value2),
     is(Result, 1),!);
    is(Result, 0),!.

evaluate(Value1, ['!=', Value2], Result) :-
    ((not(is(Value1, Value2))),
     is(Result,1),!);
    is(Result,0),!.

%% ------------------------------------------------------------
%% that's it for the interpreter's machinery. the rest of this
%% file just provides an interface to the user/tester. 
%% ------------------------------------------------------------

%% for making testing go easier
setup :-
    create_empty_table,
    parse('sample_code.txt', Structure),
    initialize_functions(Structure).

%% a wrapper for a case of expression_handler, really, but
%% with a saner syntax.
call_function_with_params(Function, Params, Result) :-
    expression_handler([[[Function], [['('],Params,[')']]]|_], Result).
