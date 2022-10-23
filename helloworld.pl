satrak_mx(N-M, Fs, Ss, Mx) :-
    get_all_tents(Fs, Ss, Tents, N-M),
    init_matrix(N-M, Tents, Mx2),
    are_tents_colliding(Tents),
    % reverse rows to get correct order (we built the rows 
    % backwards by decrementing the value instead of going 0..M)
    reverseList(Mx2, Mx).

% exit condition for end of matrix
init_matrix(0-_, _, []) :- !.    

% initializes a matrix by rows
init_matrix(N-M, Tents, [OutHead|OutTail]) :-
    init_row(N-M, Tents, OutHead2),
    % reverse row to get correct order (we built it backwards by
    % decrementing the value instead of going 0..N)
    reverseList(OutHead2, OutHead), 
    N1 is N - 1,    % decrement row number
    init_matrix(N1-M, Tents, OutTail).

% exit condition for end of row
init_row(_-0, _, []) :- !.

% initializes a row with ones and zeroes (tent or no tent)
init_row(RowNumber-ColNumber, Tents, [RowHead|RowTail]) :-
    ColNumber1 is ColNumber - 1,    % decrement column number
    init_matrix_field(RowNumber-ColNumber, Tents, RowHead),
    init_row(RowNumber-ColNumber1, Tents, RowTail).

%X-Y is part of Tents -> its value is 1
init_matrix_field(X-Y, Tents, FieldValue) :-
    member(X-Y, Tents),
    FieldValue = 1.

%X-Y is not part of Tents -> its value is 0
init_matrix_field(X-Y, Tents, FieldValue) :-
    \+ member(X-Y, Tents),
    FieldValue= 0.

% exit condition for tent coordinate method
get_all_tents([], [], [], _-_).

% gets all tent coordinates
get_all_tents([X-Y | Trees], [Direction | Directions], [Tent|Tents], N-M) :-
    get_tent(X-Y, Direction, Tent),
    is_tent_on_matrix(Tent, N-M),
    get_all_tents(Trees, Directions, Tents, N-M).

get_tent(X-Y, Direction, Tent) :-
(
    Direction == n -> X1 is X-1, Tent = X1-Y
    ;Direction == s -> X1 is X+1, Tent = X1-Y
    ;Direction == w -> Y1 is Y-1, Tent = X-Y1
    ;Direction == e -> Y1 is Y+1, Tent = X-Y1
).

% is tent located on the matrix
is_tent_on_matrix(Tent, N-M) :-
    Tent = X-Y,
    X >= 1,
    X =< N,
    Y >= 1,
    Y =< M.

% exit condition for tent field checking
are_tents_colliding([]) :- !.

% checks if all the tents are on seperate fields
are_tents_colliding([TentHead|TentTail]) :-
    \+ member(TentHead, TentTail),
    are_tents_colliding(TentTail).

% disclaimer: got it fromhttps://www.educba.com/prolog-reverse-list/
reverseList([H|T], ReversedList):-
    reverseListHelper(T,[H], ReversedList).

reverseListHelper([], Acc, Acc).

reverseListHelper([H|T], Acc, ReversedList):-
    reverseListHelper(T, [H|Acc], ReversedList).