%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Symbol table library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('grammar.pro').

/**
 * Initializes the global symbol table.
 **/
create_empty_table :-
    empty_assoc(Assoc),
    b_setval(table, Assoc).

/** 
 * Reads the function list (programme), and pushes their definitions 
 * onto the value-stacks corresponding to their function names, in the
 * global symbol table. 
 **/
initialize_functions([]) :- !.

initialize_functions([[[[RetType], [FuncName]] | [['(']| [ParamList,[')'],[=], FuncBody]]] | [Rest]]) :-
    %% flatten(ParamList, FlatParams),
    %% flatten(FuncBody, FlatBody),
    add_symbol(FuncName, [RetType, ParamList, FuncBody]),
    initialize_functions(Rest).

/**
 * Pushes a new definition onto the value-stack corresponding to 
 * a given key in the symbol table.
 **/
add_symbol(Key,Val) :-
    b_getval(table, OldTable),
    (get_assoc(Key, OldTable, OldVal),! ;
     OldVal = []),
    put_assoc(Key, OldTable, [Val|OldVal], NewTable),
    b_setval(table, NewTable).

/** 
 * Adds a list of keys, and a correlated list of values, to the
 * symbol table.
 **/
add_symbol_list([],[]) :- !.

add_symbol_list([Key|KeyList], [Val|ValList]) :-
    add_symbol(Key, Val),
    add_symbol_list(KeyList, ValList).

/**
 * Removes the most recently entered value for Key from the 
 * symbol table. Leaves a null list instead of deleting when
 * the values for Key have been depleted. 
 **/
remove_symbol(Key) :-
    b_getval(table, Table),
    get_assoc(Key, Table, [Val|Rest]),
    put_assoc(Key, Table, Rest, NewTable),
    b_setval(table, NewTable).
    

get_symbol(Key,LatestEntry) :-
    b_getval(table, Table),
    get_assoc(Key,Table,[LatestEntry|History]).

