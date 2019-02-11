%%%% -*- Mode: Prolog -*-
%%%% LITTLE MAN COMPUTER
%%% by 829474 Sgro Mattia (no group)

%%% BEGIN parsing comment
%%% is_slash/1
%%% is_slash(C)
%% check if a given character is a slash
is_slash(C) :-
    char_code(C, N),
    compare(=, N, 47).

%%% is_doubleSlash/1
%%% is_doubleSlash(String).
%% checks if String contains 2 consecutives '//' (after // you got a comment)
has_doubleSlash(S) :-
    string_chars(S, Clist),
    nth0(0, Clist, C1),
    nth0(1, Clist, C2),
    is_slash(C1),
    is_slash(C2).

%%% parsing/2
%%% parsing(String, ParsedString).
%% given String, parses the comment to the output ParsedString, as an atom;
parsing("//", ""):- !.
parsing("", ""):- !.
parsing(String, Parsed):-
    has_doubleSlash(String),
    concat(Parsed, String, String), !.
parsing(String, Parsed):-
    concat(Parsed, Resto, String),
    has_doubleSlash(Resto), !.
parsing(String, String):- !. %reaches this goal if no comment is present.
%%% END parsing comment


%%% parseMem/2
%%% parseMem(Mem, ParsedMem).
%% iterate the parser for each memory entry
parseMem([], []) :- !.
parseMem([X], [Y]):-
    parsing(X, Y),
    !.
parseMem([Row|Rest], [ParsedRow|ParsedMem]):-
    parsing(Row, ParsedRow),
    parseMem(Rest, ParsedMem),!.

%%% to_string/2
%%% to_string(Mem, StringedMem).
%% trasform each memory entry to a String, and lowers digits.
to_string([], []) :- !.
to_string([X], [Y]):-
    string(X),
    string_lower(X, Y), !.
to_string([X], [Z]):-
    atom(X),
    atom_string(X, Y),
    string_lower(Y, Z), !.
to_string([First|Mem], [FirstStr|MemStringed]):-
    string(First),
    string_lower(First, FirstStr),
    to_string(Mem, MemStringed), !.
to_string([First|Mem], [FirstStr|MemStringed]):-
    atom(First),
    atom_string(First, X),
    string_lower(X, FirstStr),
    to_string(Mem, MemStringed), !.

%%% delete_white_in_front/2
%%% delete_white_in_front(Spaced, UnSpaced).
%% used to remove spaces in front of an istruction
delete_white_in_front([], []) :- !.
delete_white_in_front([First], [NewFirst]):-
    normalize_space(string(NewFirst), First), !.
delete_white_in_front([First|Tail], NewStringed ):-
    normalize_space(string(NewFirst), First),
    delete_white_in_front(Tail, NewTail),
    append([NewFirst], NewTail, NewStringed).

%%% is_istr/1
%%% is_istr(Istruction).
%% true if Istruction is an istruction of LMC_assembly, given as String.

is_istr("add") :- !.
is_istr("sub") :- !.
is_istr("sta") :- !.
is_istr("lda") :- !.
is_istr("bra") :- !.
is_istr("brz") :- !.
is_istr("brp") :- !.
is_istr("inp") :- !.
is_istr("out") :- !.
is_istr("hlt") :- !.
is_istr("dat") :- !.

%%% is_label/1
%%% is_label(Label).
%% true if Label is a valid Label of LMC_assembly (nth0 must be a letter).
is_label(X) :-
    not(is_istr(X)),
    string_chars(X, Cs),
    nth0(0, Cs, C),
    char_type(C, csymf),
    !.

%%% is_digit/3
%%% is_digit(Number, LowerBound, HigherBound).
%% true if X is a number between given bounds.
is_digit(X, Low, High) :-
    number_string(N, X),
    integer(N),
    between(Low, High, N), !.
%% X is bounded between 0 and 999
is_legalN(X) :-
    is_digit(X, 0, 999),!.
%% X is bounded between 0 and 99
is_legal(X):-
    is_digit(X, 0, 99),!.

%%% replace/4
%%% replace(List, Index, NewElement, NewList
%% replace an element in a list with NewElement, given the index;
%% produces NewList --- used in store to easily store an element.
replace([_|Tail], 0, X, [X|Tail]).
replace([Head|Tail], Index, X, [Head|Rest]):-
    Index > -1,
    NIndex is Index - 1,
    replace(Tail, NIndex, X, Rest), !.
replace(L, _, _, L).

%%% labeler/3
%%% labeler(Memory, MemoryDeLabeled, ListOfLabels)
%% removes labels from memory at '0' index, and produces a listOfLabels
%% the DeLabeledMemory has no labels at beginning of each instruction.
labeler([], [], []).
labeler([""], [""], [_]).
labeler([X], [Y], [Elem]):-
    split_string(X, " ", " ", Splitted),
    nth0(0, Splitted, Elem),
    is_label(Elem),
    string_concat(Elem, Y, X),
    !.
labeler([X], [X], [""]):-
    split_string(X, " ", " ", Splitted),
    nth0(0, Splitted, Elem),
    is_istr(Elem), !.
labeler([First|Memory], [Y|NewMemory], [Elem|Labels]):-
    split_string(First, " ", " ", Splitted),
    nth0(0, Splitted, Elem),
    is_label(Elem),
    string_concat(Elem, Y, First),
    labeler(Memory, NewMemory, Labels), !.
labeler([First|Memory], [First|NewMemory], [""|Labels]):-
    split_string(First, " ", " ", Splitted),
    nth0(0, Splitted, Elem),
    is_istr(Elem),
    labeler(Memory, NewMemory, Labels), !.

%%% assemble/3
%%% assemble(Memory, Assembled)
%% from a list of istruction as string (i.e. the memory) pulls out
%% an assembled memory (with just CODES understrandable by a LMC).

assemble([], []).
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "dat"),
    nth0(1, I, Numb),
    is_legalN(Numb),
    number_string(X, Numb),
    F1 is X,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    length(I, 1),
    nth0(0, I, "dat"),
    F1 is 0,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "add"),
    nth0(1, I, X),
    is_legal(X),
    number_string(Numb, X),
    F1 is 100 + Numb,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "sub"),
    nth0(1, I, X),
    is_legal(X),
    number_string(Numb, X),
    F1 is 200 + Numb,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "sta"),
    nth0(1, I, X),
    is_legal(X),
    number_string(Numb, X),
    F1 is 300 + Numb,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "lda"),
    nth0(1, I, X),
    is_legal(X),
    number_string(Numb, X),
    F1 is 500 + Numb,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "bra"),
    nth0(1, I, X),
    is_legal(X),
    number_string(Numb, X),
    F1 is 600 + Numb,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "brz"),
    nth0(1, I, X),
    is_legal(X),
    number_string(Numb, X),
    F1 is 700 + Numb,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "brp"),
    nth0(1, I, X),
    is_legal(X),
    number_string(Numb, X),
    F1 is 800 + Numb,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "inp"),
    F1 is 901,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "out"),
    F1 is 902,
    assemble(Memory, Assembled), !.
assemble([F|Memory], [F1|Assembled]):-
    split_string(F, " ", " ", I),
    nth0(0, I, "hlt"),
    F1 is 0,
    assemble(Memory, Assembled), !.
%%fails if it cannot assemble.

%%% substituteLabels/3
%%% substituteLabels(Memory, JustAddresses, Labels)
%% given the listOfLabels, substitute in memory, labels with correct addresses
substituteLabels([F|Memory], [F1|DeLabeled], Labels):-
    split_string(F, " ", " ", I),
    nth0(1, I, X),
    is_label(X),
    nth0(Index, Labels, X),
    nth0(0, I, First),
    string_concat(First, " ", Middle),
    string_concat(Middle, Index, F1),
    substituteLabels(Memory, DeLabeled, Labels), !.
substituteLabels([F|Memory], [F|DeLabeled], Labels):-
    substituteLabels(Memory, DeLabeled, Labels), !.
substituteLabels([F], [F1], Labels):-
    split_string(F, " ", " ", I),
    nth0(1, I, X),
    is_label(X),
    nth0(Index, Labels, X),
    nth0(0, I, First),
    string_concat(First, " ", Middle),
    string_concat(Middle, Index, F1), !.
substituteLabels([F], [F], _).

%%% fill_trail_zeros/2
%%% fill_trail_zeros(Memory, Filled)
%% fills the momory with zeros; if has 100 istr, does nothing,
%% if has >100 istr, fails.
fill_trail_zeros(Memory, LastFilled):-
    length(Memory, X),
    X < 100,
    append(Memory, [0], Filled),
    fill_trail_zeros(Filled, LastFilled), !.
fill_trail_zeros(Memory, Memory):-
    length(Memory, 100), !.
fill_trail_zeros(Memory, _):-
    length(Memory, N),
    N > 100, fail, !.

%%% lmc_load/2
%%% lmc_load(Filename, Mem).
%% loads the FILE, Filename given, and gives a standard memory with just CODES;
%% the goals are divided in each single task to be performed, to simplify the
%% debug process and to clearify each step.
lmc_load(Filename, Filled) :-
    read_file_to_codes(Filename, Codes, []),
    split_string(Codes, "\n", " ", Memory),
    parseMem(Memory, ParsedMem),
    delete(ParsedMem, '', Istructions),
    delete(Istructions, "", Istructions2),
    to_string(Istructions2, Stringed),
    delete_white_in_front(Stringed, NewStringed),
    labeler(NewStringed, CleanMemory, Labels),
    substituteLabels(CleanMemory, ToAssemble, Labels),
    assemble(ToAssemble, Assembled),
    fill_trail_zeros(Assembled, Filled),
    !.

%%% lmc_run/3
%%% lmc_run(Filename, InputQueue, OutputQueue).
%% loads the filname and given input, calls lmc_load/2 and then
%% execution_loop.
lmc_run(Filename, Input, Output):-
    lmc_load(Filename, Memory),
    execution_loop(state(0, 0, Memory, Input, [], noflag), Output), !.

%%% execution_loop/2
%%% execution_lopp(State, NewState).
%% iterates one istruction through all the memory,
%% stops when halted_state appears.
execution_loop(state(Acc, Pc, Mem, In, Out, Flag), NewOut):-
    length(Mem, 100),
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                   state(Nacc, Npc, Nmem, Nin, Nout, Nflag)),
    NewPc is Npc mod 100, 
    execution_loop(state(Nacc, NewPc, Nmem, Nin, Nout, Nflag), NewOut), !.
execution_loop(state(Acc, Pc, Mem, In, Out, Flag), NewOut):-
    one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
                    halted_state(_, _, _, _, NewOut, _)), !.

%%% one_instruction/2
%%% one_instruction(State, NewState).
%% one_istruction is performed for each goal, based on different opCodes.
%HALT
one_instruction(state(Acc, Pc, Mem, In, Out, Flag),
		halted_state(Acc, Pc, Mem, In, Out, Flag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > -1, Numbers < 100, !.
%ADD
one_instruction(state(Acc, Pc, Mem, In, Out, _),
		state(NewAcc, NPc, Mem, In, Out, flag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 99, Numbers < 200,
    Xx is Numbers - 100,
    nth0(Xx, Mem, Cell),
    Nacc is Cell + Acc,
    Nacc > 999,
    NewAcc is Nacc mod 1000,
    NPc is Pc + 1, !.
one_instruction(state(Acc, Pc, Mem, In, Out, _),
		state(Nacc, NPc, Mem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 99, Numbers < 200,
    Xx is Numbers - 100,
    nth0(Xx, Mem, Cell),
    Nacc is Cell + Acc,
    Nacc < 1000,
    NPc is Pc + 1, !.
%SUB
one_instruction(state(Acc, Pc, Mem, In, Out, _),
		state(Nacc, NPc, Mem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 199, Numbers < 300,
    Xx is Numbers - 200,
    nth0(Xx, Mem, Cell),
    Nacc is Acc - Cell,
    Nacc > -1,
    NPc is Pc + 1, !.
one_instruction(state(Acc, Pc, Mem, In, Out, _Flag),
		state(NewAcc, NPc, Mem, In, Out, flag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 199, Numbers < 300,
    Xx is Numbers - 200,
    nth0(Xx, Mem, Cell),
    Nacc is Acc - Cell,
    Nacc < 0,
    NewAcc is Nacc mod 1000,
    NPc is Pc + 1, !.
%STORE
one_instruction(state(Acc, Pc, Mem, In, Out, _),
		state(Acc, NPc, NMem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 299, Numbers < 400,
    Xx is Numbers - 300,
    replace(Mem, Xx, Acc, NMem),
    NPc is Pc + 1, !.
%LOAD
one_instruction(state(_, Pc, Mem, In, Out, _),
		state(NAcc, NPc, Mem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 499, Numbers < 600,
    Xx is Numbers - 500,
    nth0(Xx, Mem, Cell),
    NPc is Pc + 1,
    NAcc is Cell, !.
%BRANCH
one_instruction(state(Acc, Pc, Mem, In, Out, _),
		state(Acc, Xx, Mem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 599, Numbers < 700,
    Xx is Numbers - 600, !.
%BRANCHIFZERO
one_instruction(state(0, Pc, Mem, In, Out, noflag),
		state(0, Xx, Mem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 699, Numbers < 800,
    Xx is Numbers - 700, !.
one_instruction(state(Acc, Pc, Mem, In, Out, _),
		state(Acc, NPc, Mem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 699, Numbers < 800,
    NPc is Pc + 1, !.
%BRANCHIFPOSITIVE
one_instruction(state(Acc, Pc, Mem, In, Out, noflag),
		state(Acc, Xx, Mem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 799, Numbers < 900,
    Xx is Numbers - 800, !.
one_instruction(state(Acc, Pc, Mem, In, Out, flag),
		state(Acc, NPc, Mem, In, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers > 799, Numbers < 900,
    NPc is Pc + 1, !.
%INPUT
one_instruction(state(_, Pc, Mem, [In|NIn], Out, _),
		state(In, NPc, Mem, NIn, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers = 901,
    %deprecated input(In, Nacc, Pc, NPc, NIn), !.
    NPc is Pc + 1,
    !.
one_instruction(state(_, Pc, Mem, [], Out, _),
		state(_, _, _, _, Out, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers = 901,
    fail, !.
%OUTPUT
one_instruction(state(Acc, Pc, Mem, In, Out, _),
		state(Acc, NPc, Mem, In, NOut, noflag)):-
    nth0(Pc, Mem, Numbers),
    Numbers = 902,
    append(Out, [Acc], NOut),
    NPc is Pc + 1, !.
%FAILS
%with 903__> 999 , 400__> 499 automatically fails (thankYouProlog).

%%%% eof --- lmc.pl
