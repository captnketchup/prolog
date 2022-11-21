m(2,0).
m(1,3).
m(2,1).
m(X,Y) :-
    Y is X+1.

p(1).
p(X) :-
    X > 2.
p(1).

q(X,Y) :- m(X,Y), p(Y).

novelt([], Betu, [Betu=1]).
novelt([InH|InT], Betu, [KiH|KiT]) :-
    InH = (HBetu=HSzam),
    HBetu = Betu ->
        (
            HSzam2 is HSzam + 1,
            KiH = (HBetu=HSzam2),
            KiT = InT
        )
        ;
        (
            KiH = InH,
            novelt(InT, Betu, KiT)
        ).

fokszamai([], []).
fokszamai(L, Ki) :-
    fokszamai(L, [], Ki).
fokszamai([A-B], F0, Ki) :-
    novelt(F0, A, Ki1),
    novelt(Ki1, B, Ki).
    
fokszamai([A-B|T], F0, Ki) :-
    novelt(F0, A, Ki1),
    novelt(Ki1, B, Ki2),
    fokszamai(T, Ki2, Ki).
