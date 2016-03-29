%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LEXER
%%% Olivia Lucca Fraser
%%% B00109376
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('tokenizer.pro').

% to keep all the unique keywords in one place (this could also be
% done as a sequence of facts, but using a dict seems a bit more
% elegant).
keywords(_{'int':'TYPE_INT',
           'bool':'TYPE_BOOL',
           ',':'COMMA',
           '=':'ASSIGN',
           'let':'LET',
           'in':'LET_IN',
           'if':'COND_IF',
           'then':'COND_THEN',
           'else':'COND_ELSE',
           '==':'LOGIC_EQ',
           '!=':'LOGIC_NOT_EQ',
           '>':'LOGIC_GT',
           '>=':'LOGIC_GTEQ',
           '+':'ARITH_ADD',
           '-':'ARITH_SUB',
           '(':'OPEN_P',
           ')':'CLOSE_P'}).

% 
lex_unit(Tok, Lex) :-
    (keywords(D),
     =(Lex, D.get(Tok)),!); 
    % now for generic lexemes:
    (number(Tok), =(Lex, integer(Tok)),!);
    (atom(Tok), =(Lex, identifier(Tok)),!).

lex_list([],[]) :- !.

lex_list([TokHead|TokTail], [LexHead|LexTail]) :-
    lex_unit(TokHead,LexHead),
    lex_list(TokTail,LexTail).
    
lexer(FileName, LexedList) :-
    tokenize_file(FileName, TokenList),
    lex_list(TokenList, LexedList).

