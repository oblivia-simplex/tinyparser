:- consult('symbols.pro').

%% each function entry is a triple:
%% rettype, paramtypes, body
%% the body is the 'expression'


expressionHandler([Value, ExtraExpression], Result) :-
    valueHandler(Value, ValueResult),
    extraExpressionHandler(ExtraExpression, ExtraResult),
    evaluate(ValueResult, ExtraResult, Result),!.

%% a value is a trivial expression
expressionHandler(Value, Result) :-
    valueHandler(Value,Result),!.

expressionHandler([[if]|Body], Result) :-
    ifHandler(Body, Result).

tifHandler([Ante|[[then]|[Cons|[[else]|[Alt]]]]], [Ante,Cons,Alt]) :- !.

ifHandler([Ante|[[then]|[Cons|[[else]|[Alt]]]]], Result) :-
    ( expressionHandler(Ante, 1),
      expressionHandler(Cons, Result),!) ;
    expressionHandler(Alt, Result).
   


extraExpressionHandler([], _) :- !.

extraExpressionHandler([[Op], Value], [Op, ValueResult]) :-
    valueHandler(Value, ValueResult), !.
%% it works!




%% note: numbers are stored as quoted atoms. convert back.
valueHandler(Value, ValueResult) :-
    atom(Value),
    atom_number(Value, Number),
    is(ValueResult,Number),!.

%% but just in case we fix that, let's add this kludge
valueHandler(Value, ValueResult) :-
    number(Value),
    is(ValueResult,Value),!.


valueHandler(Value, ValueResult) :-
    (not(number(Value)),
     atom(Value),
     get_symbol(Value, ValueResult), !).


% first get values out of their listy format
valueHandler([[Val], []], Result) :-
    valueHandler(Val, Result),!.


%% evaluate(Atom, [], Result) :-
%%     (atom(Atom), =(Result, Atom)),!.
%% nope, look up atoms in symbol table. not like quoted symbols. 

evaluate(Value1, ['+', Value2], Result) :-
    is(Result, +(Value1, Value2)).

evaluate(Value1, ['==', Value2], Result) :-
    (is(Value1, Value2),
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

%% -------------------------------------------
%% the grammar lumps assignment in with the
%% arithmetic operators, so we'll follow suit
%% here. big limitation is that it looks like
%% you can only assign constant ints as values
%% -------------------------------------------
evaluate(Variable, ['=', Value], Value) :-
    add_symbol(Variable, Value).


%% ------------------------------------------------------------









setup :-
    create_empty_table,
    parse('sample_code.txt', Structure),
    initialize_functions(Structure).

