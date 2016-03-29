%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TOKENIZER
%%% Olivia Lucca Fraser
%%% B00109376
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Yanked from my listlib. If this section gets too long it would be
%%% better to just import the file.

push(X, List, [X | List]).

remove_all(_, [], []).  

remove_all(X, [X | Tail], Result) :-
    remove_all(X, Tail, Result).

remove_all(X, [E | Tail], Result) :-
    remove_all(X, Tail, Tmp),
    push(E, Tmp, Result).

%% handy for debugging:

print_list([]) :- !.

print_list([A|AtomList]) :-
    write(A),
    write(' '),
    print_list(AtomList).

%%% End listlib section %%%

get_tok(Stream, Tok) :-
    get_code(Stream, Byte),
    cut_at_delim(Byte, BytesRead, Stream),
    atom_codes(Tok, BytesRead).

cut_at_delim(Byte, [], _) :-
    Byte =< 0x20, !.

cut_at_delim(Byte, [Byte|Bytes], Stream) :-
    get_code(Stream, Next),
    cut_at_delim(Next, Bytes, Stream). 

scan_tokens(Stream, []) :-
    at_end_of_stream(Stream).

scan_tokens(Stream, [Tok|TokenList]) :-
    \+ at_end_of_stream(Stream),
    get_tok(Stream, Tok),
    scan_tokens(Stream, TokenList).

remove_empties([], []) :- !.

remove_empties(List, NewList) :-
    remove_all('',List,NewList).

tokenize(Stream, Result) :-
    scan_tokens(Stream, TokenList),
    remove_empties(TokenList, Result), !.

tokenize_file(FileName, TokenList) :-
    open(FileName, read, Stream),
    tokenize(Stream, TokenList),
    length(TokenList, L),
    write(L),
    write(" tokens read"), % why no printf?
    nl,
    close(Stream).


