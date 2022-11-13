:- use_module(library(lists)).

iranylistak(NM, Fs, ILs) :-
    make_directions(Fs, NM, Directions, Fs),
    (
        member([], Directions) -> ILs = [];
        ILs = Directions
    ).

sator_szukites(Fs, I, ILs0, Ils) :- 
    nth1(I, Fs, Tree),
    nth1(I, ILs0, TentDirection),
    % check if the tent directions are greater than 1
    length(TentDirection, TentLength),
    TentLength = 1,
    nth0(0, TentDirection, TentDir),
    get_tent(Tree, TentDir, Tent),
    neighbouring_fields(Tent, Neighbours),
    traverse_trees(Fs, ILs0, Neighbours, ILs1),
    (
        member([], ILs1) -> Ils = [];
        Ils = ILs1
    ).

traverse_trees(_, [], _, []) :- !.
traverse_trees([_|Trees], [OutHead|DirectionListTail], Neighbouring_fields, [OutHead|OutTail]) :-
    length(OutHead, TentLength),
    TentLength = 1,
    traverse_trees(Trees, DirectionListTail, Neighbouring_fields, OutTail).
traverse_trees([Tree|Trees], [DirectionListHead|DirectionListTail], Neighbouring_fields, [OutHead|OutTail]) :-
    % do not test for one sized direction lists
    length(DirectionListHead, TentLength),
    TentLength > 1,
    findall(Dir, (member(Dir, DirectionListHead),
        get_tent(Tree, Dir, Tent),
         \+ member(Tent, Neighbouring_fields)), OutHead),
    traverse_trees(Trees, DirectionListTail, Neighbouring_fields, OutTail).
    

% gets the neighbouring fields of a field
neighbouring_fields(X-Y, Field) :- 
    NegativeX is X-1,
    PositiveX is X+1,
    NegativeY is Y-1,
    PositiveY is Y+1,
    Field = [NegativeX-PositiveY, X-PositiveY, PositiveX-PositiveY,
    NegativeX-Y, X-Y, PositiveX-Y, 
    NegativeX-NegativeY, X-NegativeY, PositiveX-NegativeY].


% exit condition for make_directions/4
make_directions([], _, [], _) :- !.
    
% generates the possible directions for the trees
make_directions([TreeHead|TreeTail], NM, [Direction|Directions], Trees) :-
    get_tent(TreeHead, n, NorthTent),
    get_tent(TreeHead, s, SouthTent),
    get_tent(TreeHead, w, WestTent),
    get_tent(TreeHead, e, EastTent),
    % if the direction is on the grid and it's not occupied by a tree, it gets the value of
    % the direction it's supposed to represent (n,s,w,e)
    (
        is_tent_on_matrix(NM, NorthTent) -> (is_tent_on_tree(NorthTent, Trees) -> N = n;
        N = []);
        N = []
    ),
    (
        is_tent_on_matrix(NM, SouthTent) -> (is_tent_on_tree(SouthTent, Trees) -> S = s;
        S = []);
        S = []
    ),
    (
        is_tent_on_matrix(NM, WestTent) -> (is_tent_on_tree(WestTent, Trees) -> W = w;
        W = []);
        W = []
    ),
    (
        is_tent_on_matrix(NM, EastTent) -> (is_tent_on_tree(EastTent, Trees) -> E = e;
        E = []);
        E = []
    ),
    % flatten([E,N,S,W], Direction),
    exclude(empty, [E,N,S,W], Direction),
    
    make_directions(TreeTail, NM, Directions, Trees).

empty([]).

is_tent_on_tree(Tent, Trees) :-
    \+ member(Tent, Trees).



get_tent(X-Y, Direction, Tent) :-
    (
        Direction == n -> X1 is X-1, Tent = X1-Y
        ;Direction == s -> X1 is X+1, Tent = X1-Y
        ;Direction == w -> Y1 is Y-1, Tent = X-Y1
        ;Direction == e -> Y1 is Y+1, Tent = X-Y1
    ).
    

% is tent located on the matrix
is_tent_on_matrix(N-M, Tent) :-
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
