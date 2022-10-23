satrak_mx(N-M, Fs, Ss, Mx) :-
    get_all_tents(Fs, Ss, Tents, N-M),
    init_matrix(N-M, Tents, Mx, 0),
    are_tents_colliding(Tents).
    

% exit condition for end of matrix
init_matrix(N-_, _, [], N) :- !.    

% initializes a matrix by rows
init_matrix(N-M, Tents, [OutHead|OutTail], RowAccumulator) :-
    RowAccumulator2 is RowAccumulator + 1,
    init_row(RowAccumulator2-M, Tents, OutHead, 0),
    init_matrix(N-M, Tents, OutTail, RowAccumulator2).


% exit condition for end of row
init_row(_-M, _, [], M) :- !.

% initializes a row with ones and zeroes (tent or no tent)
init_row(RowNumber-ColNumber, Tents, [RowHead|RowTail], ColAccumulator) :-
    ColAccumulator1 is ColAccumulator + 1,
    init_matrix_field(RowNumber-ColAccumulator1, Tents, RowHead),
    init_row(RowNumber-ColNumber, Tents, RowTail, ColAccumulator1).

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
