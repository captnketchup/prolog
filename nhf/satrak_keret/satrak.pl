% :- type fLeiro  ---> satrak(sSzS, sSzO, list(parc)).
% :- type sSzS    == list(int).
% :- type sSzO    == list(int).
% :- type parc    == int-int.
% :- type irany   ---> n    % észak
%                 ;    e    % kelet
%                 ;    s    % dél
%                 ;    w.   % nyugat
% :- type sHelyek   == list(irany).
% :- pred satrak(fLeiro::in, sHelyek::out).

% Ss: sátrak soronkénti száma
% Os: sátrak oszloponkénti száma
% Fs: fák sor(i)-oszlop(j)
satrak(Leiro, Solution) :-
    Leiro = satrak(Ss, Os, Fs),
    % mátrix paraméterek
    length(SS, N),
    length(Os, M),
    % iránylisták generálása
    iranylistak(NM, Fs, Ils0),
    OG_list is Ils0
    .

oszloponkent_szukites(_, [], _, Ils0, Ils) :- 
    write_ln(Ils0),
    Ils = Ils0.

oszloponkent_szukites(Fs, [OszlopHead|OszlopTail], Index1, Ils0, Ils) :-
    Osszegfeltetel = oszl(Index1, OszlopHead),
    write("index "),
    write_ln(Index1),
    write_ln("eleinte:"),
    write_ln(Ils0),
    write_ln(""),
    (
        (
            osszeg_szukites(Fs, Osszegfeltetel, Ils0, Ils1),
            write_ln("Osszegfeltetel:"),
            write_ln(Osszegfeltetel),
            write_ln("osszegszukites:"),
            write_ln(Ils1),
            write_ln(""),
            Index is Index1 + 1,
            oszloponkent_szukites(Fs, OszlopTail, Index, Ils1, Ils)
        )
        ;
        (
            write_ln("nem osszegszukites:"),
            write_ln(Ils0),
            write_ln(""),
            Index is Index1 + 1,
            oszloponkent_szukites(Fs, OszlopTail, Index, Ils0, Ils)
        )
    )    
    .


% src: https://stackoverflow.com/questions/22545726/prolog-two-lists-are-exactly-the-same
same([], []).

same([H1|R1], [H2|R2]):-
    H1 = H2,
    same(R1, R2).



% khf6.pl
% Fs: fák
% ILs0: bemeneti iránylisták fákhoz->
% ILs: kimeneti (csökkentett) változó
% pattern matching sorra
osszeg_szukites(Fs, Osszegfeltetel, ILs0, ILs) :-
    Osszegfeltetel = sor(_,Db1),
    (Db1 < 0 -> Db is 999; Db is Db1),
    fa_szamlalas(Fs, Osszegfeltetel, ILs0, Bs, Es),
    length(Bs, B),
    length(Es, E),
    (
        B + E < Db -> ILs = []
    ;   B + E =:= Db -> esetleg_szukites(Fs, Osszegfeltetel, ILs0, Es, ILs)
    ;   B == Db -> biztos_szukites(Fs, Osszegfeltetel, ILs0, Es, ILs)
    ;   B > Db -> ILs = []
    ;   Ils = Ils0
    ).

% pattern matching oszlopra
osszeg_szukites(Fs, Osszegfeltetel, ILs0, ILs) :-
    Osszegfeltetel = oszl(_,Db1),
    (Db1 < 0 -> Db is 999; Db is Db1),
    write_ln("Db"),
    write_ln(Db),
    fa_szamlalas(Fs, Osszegfeltetel, ILs0, Bs, Es),
    length(Bs, B),
    length(Es, E),
    write_ln("Bs:"),
    write_ln(Bs),
    write_ln("Es:"),
    write_ln(Es),
    (
        B + E < Db -> ILs = []
    ;   B + E =:= Db -> esetleg_szukites(Fs, Osszegfeltetel, ILs0, Es, ILs)
    ;   B == Db -> biztos_szukites(Fs, Osszegfeltetel, ILs0, Es, ILs)
    ;   B > Db -> ILs = []
    ;   Ils = Ils0
    )
    .

% megkeresni minden biztosan és esetlegesen teljesülő fát
fa_szamlalas(Fak, Osszegfeltetel, Iranylistak, BiztosFak, EsetlegesFak) :-
    findall(Fa, (
        member(Fa, Fak),
        nth0(Index, Fak, Fa),
        nth0(Index, Iranylistak, Iranylista),
        biztos_teljesul(Fa, Osszegfeltetel, Iranylista)
        ), BiztosFak),
    findall(Fa, (
        member(Fa, Fak),
        nth0(Index, Fak, Fa),
        nth0(Index, Iranylistak, Iranylista),
        \+ biztos_teljesul(Fa, Osszegfeltetel, Iranylista),
        esetleg_teljesul(Fa, Osszegfeltetel, Iranylista)
        ), EsetlegesFak).


% #esetleges fa == #biztos fa, akkor minden esetlegesből biztos fát csinálunk úgy, hogy kitöröljük
% az esetlegesek alternatív irányait (ii. eset)
esetleg_szukites([], _, _, _, []) :- !.
esetleg_szukites([FakHead|FakTail], sor(I, Db), [IranyHead|IranyTail], EsetlegesFak, [SzukHead|SzukTail]) :-
    (
        % megnézni esetleges fa-e, ha igen biztost csinálni belőle
        member(FakHead, EsetlegesFak) -> esetlegesbol_biztos(FakHead, sor(I, Db), IranyHead, SzukHead)
    ;   SzukHead = IranyHead
    ),
    esetleg_szukites(FakTail, sor(I, Db), IranyTail, EsetlegesFak, SzukTail).


esetleg_szukites([FakHead|FakTail], oszl(I, Db), [IranyHead|IranyTail], EsetlegesFak, [SzukHead|SzukTail]) :-
    (
        % megnézni esetleges fa-e, ha igen biztost csinálni belőle
    member(FakHead, EsetlegesFak) -> esetlegesbol_biztos(FakHead, oszl(I, Db), IranyHead, SzukHead)
    ;   SzukHead = IranyHead
    ),
    esetleg_szukites(FakTail, oszl(I, Db), IranyTail, EsetlegesFak, SzukTail).

% hogyha #biztos fa == #összes fa akkor ki kell törölni közülük az esetlegeseket (iii. eset)
biztos_szukites([], _, _, _, []) :- !.
biztos_szukites([FakHead|FakTail], oszl(I, Db), [IranyHead|IranyTail], EsetlegesFak, [SzukHead|SzukTail]) :-
    (
        member(FakHead, EsetlegesFak) -> esetleges_torles(FakHead, oszl(I, Db), IranyHead, SzukHead)
        ;   SzukHead = IranyHead
    ),
    biztos_szukites(FakTail, oszl(I, Db), IranyTail, EsetlegesFak, SzukTail).

biztos_szukites([FakHead|FakTail], sor(I, Db), [IranyHead|IranyTail], EsetlegesFak, [SzukHead|SzukTail]) :-
    (
    member(FakHead, EsetlegesFak) -> esetleges_torles(FakHead, sor(I, Db), IranyHead, SzukHead)
    ;   SzukHead = IranyHead
    ),
    biztos_szukites(FakTail, sor(I, Db), IranyTail, EsetlegesFak, SzukTail).

% esetleges fákból biztos fákat csinálunk úgy, hogy elhagyjuk az egyéb irányokat
esetlegesbol_biztos(FaX-_, sor(I, _), Iranylista, SzukLista) :-
    (
        FaX =:= I - 1 -> SzukLista = [s]   % ha a fa feljebb van akkor a sátor s
      ; FaX =:= I + 1 -> SzukLista = [n]   % ha a fa lejjebb van akkor a sátor n
      ; FaX == I -> delete(Iranylista, s, SzukLista2), delete(SzukLista2, n, SzukLista) % hogyha a fa ugyanabban a sorban törlöm az s-t és n-t
    ).
esetlegesbol_biztos(_-FaY, oszl(I,_), Iranylista, SzukLista) :-
    (
        FaY =:= I - 1 -> SzukLista = [e]   % ha a fa balrabb van akkor e
      ; FaY =:= I + 1 -> SzukLista = [w]   % ha a fa jobbrabb van akkor w
      ; FaY == I -> delete(Iranylista, e, SzukLista2), delete(SzukLista2, w, SzukLista) % hogyha a fa ugyanabban a sorban törlöm az w-t és e-t
    ).

% #biztos fák == Db, kitöröljük az éppen Osszegfeltetelbe eső esetleges irányokat
esetleges_torles(FaX-_, sor(I, _), Iranylista, SzukLista) :-
    (
        FaX =:= I - 1 -> delete(Iranylista, s, SzukLista)   % ha a fa feljebb van akkor delete
      ; FaX =:= I + 1 -> delete(Iranylista, n, SzukLista)   % ha a fa lejjebb van akkor delete
      ; FaX == I -> delete(Iranylista, e, SzukLista2), delete(SzukLista2, w, SzukLista) % hogyha a fa ugyanabban a sorban törlöm az e-t és w-t
    ).

esetleges_torles(_-FaY, oszl(I, _), Iranylista, SzukLista) :-
    (
        FaY =:= I - 1 -> delete(Iranylista, e, SzukLista)   
      ; FaY =:= I + 1 -> delete(Iranylista, w, SzukLista)   
      ; FaY == I -> delete(Iranylista, s, SzukLista2), delete(SzukLista2, n, SzukLista) 
    ).

% egy adott fára biztos-e a feltételre nézve
biztos_teljesul(_, _, []) :- !.
biztos_teljesul(Fa, Osszegfeltetel, [IranyHead|IranyTail]) :-
    Osszegfeltetel = sor(I,_),
    length([IranyHead|IranyTail], L),
    L < 3,
    get_tent(Fa, IranyHead, X-_),
    (
        X \= I;
    biztos_teljesul(Fa, Osszegfeltetel, IranyTail)
    ).

biztos_teljesul(Fa, Osszegfeltetel, [IranyHead|IranyTail]) :-
    Osszegfeltetel = oszl(I,_),
    length([IranyHead|IranyTail], L),
    L < 3,
    get_tent(Fa, IranyHead, _-Y),
    (
        Y \= I;
    biztos_teljesul(Fa, Osszegfeltetel, IranyTail)
    ).

esetleg_teljesul(_, _, []) :- false.
esetleg_teljesul(Fa, Osszegfeltetel, [IranyHead|IranyTail]) :- 
    Osszegfeltetel = sor(I,_),
    (
        (
        get_tent(Fa,IranyHead, X-_),
        X == I
        % ezt az elemet kell bent hagyni a listában    
        ),!
        ;
        % a többi elemet removeolni
        (esetleg_teljesul(Fa, Osszegfeltetel, IranyTail))
    ).

esetleg_teljesul(Fa, Osszegfeltetel, [IranyHead|IranyTail]) :- 
    Osszegfeltetel = oszl(I,_),
    (
        (
        get_tent(Fa, IranyHead, _-Y),
        Y == I
        % ezt az elemet kell bent hagyni a listában
        ),!
        ;
        % a többi elemet removeolni
        (esetleg_teljesul(Fa, Osszegfeltetel, IranyTail))
    ).

% sátor pozíciója egy fa helyzetéből és irányából
get_tent(X-Y, Direction, Tent) :-
    (
        Direction == n -> X1 is X-1, Tent = X1-Y
        ;Direction == s -> X1 is X+1, Tent = X1-Y
        ;Direction == w -> Y1 is Y-1, Tent = X-Y1
        ;Direction == e -> Y1 is Y+1, Tent = X-Y1
    ).

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
