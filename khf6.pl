% Fs: fák
% ILs0: bemeneti iránylisták fákhoz->
% ILs: kimeneti (csökkentett) változó
% pattern matching sorra
osszeg_szukites(Fs, Osszegfeltetel, ILs0, ILs) :-
    Osszegfeltetel = sor(_,Db),
    fa_szamlalas(Fs, Osszegfeltetel, ILs0, Bs, Es),
    write_ln(Bs),
    write_ln(Es),
    length(Bs, B),
    length(Es, E),
    (
        B + E < Db -> ILs = []
    ;   B + E =:= Db -> esetleg_szukites(Fs, Osszegfeltetel, ILs0, Es, ILs)
    ;   B == Db -> biztos_szukites(Fs, Osszegfeltetel, ILs0, Es, ILs)
    ;   B > Db -> ILs = []
    )
    .

% pattern matching oszlopra
osszeg_szukites(Fs, Osszegfeltetel, ILs0, ILs) :-
    Osszegfeltetel = oszl(_,Db),
    fa_szamlalas(Fs, Osszegfeltetel, ILs0, Bs, Es),
    write_ln(Bs),
    write_ln(Es),
    length(Bs, B),
    length(Es, E),
    (
        B + E < Db -> ILs = []
    ;   B + E =:= Db -> esetleg_szukites(Fs, Osszegfeltetel, ILs0, Es, ILs)
    ;   B == Db -> biztos_szukites(Fs, Osszegfeltetel, ILs0, Es, ILs)
    ;   B > Db -> ILs = []
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

% egy adott fa esetleges-e a feltételre nézve
% picit átírni XD
esetleg_teljesul(_-Fy, oszl(J, _), IL0) :-
    (   Fy == J ->      (member(e, IL0), !; member(w, IL0)), 
                        (member(n, IL0), !; member(s, IL0))
    ;   Fy =:= J - 1 -> member(e, IL0), length(IL0, Len), Len > 1
    ;   Fy =:= J + 1 -> member(w, IL0), length(IL0, Len), Len > 1
    ).
esetleg_teljesul(Fx-_, sor(I, _), IL0) :-
    (   Fx == I ->  (member(e, IL0), !; member(w, IL0)), 
                    (member(n, IL0), !; member(s, IL0))
    ;   Fx =:= I - 1 -> member(s, IL0), length(IL0, Len), Len > 1
    ;   Fx =:= I + 1 -> member(n, IL0), length(IL0, Len), Len > 1
    ).

% esetleg_teljesul(_, _, []) :- false.
% esetleg_teljesul(Fa, Osszegfeltetel, [IranyHead|IranyTail]) :- 
%     Osszegfeltetel = sor(I,_),
%     (
%         (
%         get_tent(Fa,IranyHead, X-_),
%         X == I
%         % ezt az elemet kell bent hagyni a listában    
%         ),!
%         ;
%         % a többi elemet removeolni
%         (esetleg_teljesul(Fa, Osszegfeltetel, IranyTail))
%     ).

% esetleg_teljesul(Fa, Osszegfeltetel, [IranyHead|IranyTail]) :- 
%     Osszegfeltetel = oszl(I,_),
%     (
%         (
%         get_tent(Fa, IranyHead, _-Y),
%         Y == I
%         % ezt az elemet kell bent hagyni a listában
%         ),!
%         ;
%         % a többi elemet removeolni
%         (esetleg_teljesul(Fa, Osszegfeltetel, IranyTail))
%     ).

% sátor pozíciója egy fa helyzetéből és irányából
get_tent(X-Y, Direction, Tent) :-
    (
        Direction == n -> X1 is X-1, Tent = X1-Y
        ;Direction == s -> X1 is X+1, Tent = X1-Y
        ;Direction == w -> Y1 is Y-1, Tent = X-Y1
        ;Direction == e -> Y1 is Y+1, Tent = X-Y1
    ).
    