:- consult('interpreter.pro').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The front end of the whole mess: loads a src file, tokenizes
%% lexes and parses it, intializes the symbol table, and then
%% calls main, passing a list of arguments as parameters. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_programme(FileName, Arguments, Result) :-
    create_empty_table,
    parse(FileName, Structure),
    initialize_functions(Structure),
    call_function_with_params(main, Arguments, Result).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


