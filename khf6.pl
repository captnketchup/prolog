% Fs: fák
% ILs0: bemeneti iránylisták fákhoz->
% ILs: kimeneti (csökkentett) változó
% pattern matching sorra
osszeg_szukites(Fs, Osszegfeltetel, ILs0, ILs) :-
    Osszegfeltetel = sor(I,Db),
    fa_szamlalas(Fs, Osszegfeltetel, ILs0, B, E)
    .

% pattern matching oszlopra
osszeg_szukites(Fs, Osszegfeltetel, ILs0, ILs) :-
    Osszegfeltetel = oszl(I,Db),
    fa_szamlalas(Fs, Osszegfeltetel, ILs0, B, E)
    .

fa_szamlalas([], _, [], 0, 0).
fa_szamlalas([FaHead|FaTail], Osszegfeltetel, [IlHead|IlTail], B, E) :-
    (
        biztos_teljesul(FaHead, Osszegfeltetel, IlHead),
        fa_szamlalas(FaTail, Osszegfeltetel, IlTail, B1, E),
        B is B1 + 1
        );
    (
        esetleg_teljesul(FaHead, Osszegfeltetel, IlHead),
        fa_szamlalas(FaTail, Osszegfeltetel, IlTail, B, E1),
        E is E1 + 1
    );
    fa_szamlalas(FaTail, Osszegfeltetel, IlTail, B, E).

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
        X == I,
        % ezt az elemet kell bent hagyni a listában    
        )
        ;
        % a többi elemet removeolni
    (esetleg_teljesul(Fa, Osszegfeltetel, IranyTail))
    ).

esetleg_teljesul(Fa, Osszegfeltetel, [IranyHead|IranyTail]) :- 
    Osszegfeltetel = oszl(I,_),
    (
        (
        get_tent(Fa, IranyHead, _-Y),
        Y == I,
        % ezt az elemet kell bent hagyni a listában
        )
        ;
        % a többi elemet removeolni
        (esetleg_teljesul(Fa, Osszegfeltetel, IranyTail))
    ).


get_tent(X-Y, Direction, Tent) :-
    (
        Direction == n -> X1 is X-1, Tent = X1-Y
        ;Direction == s -> X1 is X+1, Tent = X1-Y
        ;Direction == w -> Y1 is Y-1, Tent = X-Y1
        ;Direction == e -> Y1 is Y+1, Tent = X-Y1
    ).
    