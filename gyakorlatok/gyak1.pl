max(N,N) :-
    N > 0.

max(N,X) :-
    N > 1,
    N1 is N - 1,
    max(N1, X).


hatv(_A, 0, H) :-
    H = 1.
hatv(A, E, H) :-
    E > 0,
    E1 is E-1,
    hatv(A, E1, H1),
    H is A*H1.

fa_pontszama(leaf(_), 0).
fa_pontszama(node(L,R), P) :-
    fa_pontszama(L, LP),
    fa_pontszama(R, RP),
    P is LP+RP+1.

fa_noveltje(leaf(Be), leaf(Ki)) :-
    Ki is Be+1.
fa_noveltje(node(L,R), node(NL, NR)) :-
    fa_noveltje(L, NL),
    fa_noveltje(R, NR).

lista_hossza([], 0).
lista_hossza([_LHead|LTail], Hossz) :-
    lista_hossza(LTail, Hossz2),
    Hossz is Hossz2 + 1.

lista_noveltje([], _).
lista_noveltje([L0H|L0T], [LH|LT]) :-
    LH is L0H + 1,
    lista_noveltje(L0T, LT).


lista_utolso_eleme([Ertek|[]], Ertek).
lista_utolso_eleme([Lh|Lt], Ertek) :-
    lista_utolso_eleme(Lt, Ertek).

fa_levelerteke(leaf(E), E).
fa_levelerteke(node(L,_), E) :- 
    fa_levelerteke(L, E).
fa_levelerteke(node(_,R), E) :- 
    fa_levelerteke(R, E).

fa_reszfaja(Fasz, Fasz).
fa_reszfaja(node(L, _), Fasz):-
    fa_reszfaja(L, Fasz).
fa_reszfaja(node(_, R), Fasz):-
    fa_reszfaja(R, Fasz).

lista_prefixuma(_, []).
lista_prefixuma([Lh|Lt], [OutH|OutT]) :-
    OutH is Lh,
    lista_prefixuma(Lt, OutT).

% lista_prefixuma(_, []).
% lista_prefixuma([X|L], [X,P]) :-
%     lista_prefixuma(L,P).