%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The grammar and parser for the language.
%% Olivia Lucca Fraser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% tiny sample:
% int add ( int a , int b ) = a + b 

:- consult('lexer.pro').

parse(Filename, Structure) :-
    tokenize_file(Filename, Tokens),
    lex_list(Tokens, Lexed),
    phrase(programme(Structure),Lexed).

programme(R) --> function_list(R), !.

programme([]) -->  !.

function_list([F , FC]) --> function(F), function_list_collection(FC).

function_list_collection(FL) --> function_list(FL).

function_list_collection([]) --> [].

function([TID,['('],TIDL,[')'],['='],EXP]) --> type_id(TID),
                                         ['OPEN_P'],
                                         type_id_list(TIDL),
                                         ['CLOSE_P'],
                                         ['ASSIGN'],
                                         expression(EXP).

type_id([[int],[ID]]) --> ['TYPE_INT'], [identifier(ID)].

type_id([[bool],[ID]]) --> ['TYPE_BOOL'], [identifier(ID)].

type_id_list([TID,TIDLC]) --> type_id(TID),
                              type_id_list_collection(TIDLC).

type_id_list_collection([[','],TIDL]) --> ['COMMA'],
                                          type_id_list(TIDL).

type_id_list_collection([]) --> [].

expression([[if],CMP,[then],VAL0,[else],VAL1]) -->
    ['COND_IF'], comparison(CMP), ['COND_THEN'],
    value(VAL0), ['COND_ELSE'], value(VAL1).

expression([[let],[ID],['='],VAL,[in],EXP]) -->
    ['LET'], [identifier(ID)], ['ASSIGN'],
    value(VAL), ['LET_IN'], expression(EXP).

expression([VAL,EXP]) --> value(VAL), extra_expression(EXP).

extra_expression(ARITH) --> arithmetic(ARITH).

extra_expression([]) --> []. 

arithmetic([['+'],VAL]) --> ['ARITH_ADD'], value(VAL).

arithmetic([['='],VAL]) --> ['ASSIGN'], value(VAL).

comparison([VAL,CMPR]) --> value(VAL), comparison_right(CMPR).

comparison_right([['=='],VAL]) --> ['LOGIC_EQ'], value(VAL).

comparison_right([['!='],VAL]) --> ['LOGIC_NOT_EQ'], value(VAL).

comparison_right([['>'],VAL]) --> ['LOGIC_GT'], value(VAL).

comparison_right([['>='],VAL]) --> ['LOGIC_GTEQ'], value(VAL).

value([N]) --> [integer(N)].

value([[ID],PARAMS]) --> [identifier(ID)], value_parameters(PARAMS).

value_parameters([['('],PARAMS,[')']]) --> ['OPEN_P'], parameters(PARAMS), ['CLOSE_P'].

value_parameters([]) --> [].

parameters([VAL, PAR]) --> value(VAL), parameters_list(PAR).

parameters_list([[','],PAR]) --> ['COMMA'], parameters(PAR).

parameters([[','],PAR]) --> ['COMMA'], parameters.

parameters_list([]) --> [].
                   




