p(1).
p(2).
p(X) :-
    X > 1.

m(2,0).
m(1,2).
m(1,3).
m(2,1).
m(_,4).

q(X) :-
    m(_, X), p(X).

:- op(550, fx, @).
:- op(600, xfx, --).

p(@ I, @ I1) :-
	I1 is I+1.
p(L--R, TL--TR) :-
	p(R, TR),
	p(L, TL).

parospoz(Lista, X) :-
    Lista = [_X1, X2|T],
    (
        X2 > 0 ->
        (
            X = X2
        ;   parospoz(T, X)
        )
    ;   parospoz(T, X)
    ).

lista_ki(Lista, O) :-
    Lista = [H|T],
    (
        lista_ki(T, O)
        ;  O = H
    ).

% count(X, L, N) :-
%     findall(X,member(X,L),Bag),
%     length(Bag, Hossz),
%     Hossz = N.

count(X, L, N) :-
    count_acc(X, L, 0, N).

count_acc(X, [], Acc, N) :-
    Acc = N.
count_acc(X, [H|T], Acc, N) :-
    X = H -> 
    (
        Acc1 is Acc +1,
        count_acc(X, T, Acc1, N)
    )
    ;
    count_acc(X, T, Acc, N).

    
hosszabb(H, R) :-
    length(H, LH),
    length(R, LR),
    LH > 0,
    LR > 0,
    LR < 100,
    LH < 100,    
    LH > LR.

helyettesitese(F, [], 0).
helyettesitese(F, [Betu-Szam|T], H) :-
number(F) -> 
    (H is F)
    ;
    (
        Betu = F ->      
            (
                H is Szam
            )
            ;
            (
                helyettesitese(F, T, H)
            )
    ).



szetszed(Kifejezes,SzetszedettKifejezes):-
    Kifejezes =.. [Muvelet,Elso,Masodik] ,
    (compound(Elso) ->
        szetszed(Elso,ElsoMegoldas)
        ;
        ElsoMegoldas = Elso
        ),
        (compound(Masodik) ->
    szetszed(Masodik,MasodikMegoldas)
    ;
    MasodikMegoldas = Masodik
    ),
    SzetszedettKifejezes = [Muvelet,ElsoMegoldas,MasodikMegoldas].

kisbetu(K) :-
    K >= 0'a, K=< 0'z.

kezdo_szava([BeH1, BeH2|BeT], Kezdet, Maradek) :-
    (kisbetu(BeH1), kisbetu(BeH2)) ->
        (
            KezdetH = [BeH1, BeH2]
            ,
            kezdo_rekurziv(BeT, KezdetT, Maradek),
            UjKezdet = [KezdetH|KezdetT],
            flatten(UjKezdet, Kezdet)
        )
        ;
        (
            false   
        ).

    
kezdo_rekurziv([], Kezdet, Maradek).
kezdo_rekurziv([BeH|BeT], [KezdetH|KezdetT], Maradek) :-
    kisbetu(BeH) ->
        (
            KezdetH = BeH,
            kezdo_rekurziv(BeT, KezdetT, Maradek)
        )
        ;
        (
            KezdetH = [],
            KezdetT = [],
            Maradek = [BeH|BeT]
        )
        .


/*⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⠁⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠁⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣏⣛⠿⣿⠉⠀⠀⠀⢛⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⠛⠛⠻⠿⠿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⡈⠳⣄⠀⠀⠀⢹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠉⠀⠀⠀⠀⠀⠀⠀⠹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡏⠁⡀⠸⡀⠀⠀⠈⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡷⠂⠀⠀⠀⠀⠀⠈⠀⠀⠀⡿⢿⣿⣿⣿⣿⣿⣿⣿⣧⠀⠀⠸⡄⠳⡄⠀⠀⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠐⠀⠘⢂⣁⠙⢿⣿⣿⣿⣿⣿⣷⠄⠀⣽⡆⠹⠀⠀⠀⢹⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠀⢸⣿⠀⢸⣿⣿⣿⣿⣿⣿⡄⠀⢻⠻⡄⠀⠀⠀⠘⢻⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠄⠈⠃⠈⠻⢿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⢠⠁⠀⠀⠀⠀⠈⢿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⡿⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⣀⣱⠀⢀⠀⠘⠈⢿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀⠀⣀⣀⡀⠀⠀⠀⠀⠀⠈⡟⠀⡇⠀⡀⠀⢸⣿⣿⣿⣿⣿⣿⡃⠀⠀⡇⠀⠀⠀⠀⠀⠀⠀⠹⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⡇⢤⣼⠿⠿⣿⡿⣷⣶⣦⣤⣤⡇⠀⣷⣄⣠⣴⣿⣿⣿⣿⣿⣿⣿⣄⠀⠀⠂⠀⠀⠀⠀⠀⠀⠀⠀⣽⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⠿⣾⠃⣀⣠⣤⣴⣾⣿⣿⡿⠛⠁⠀⣿⣿⣿⣿⣿⣿⣿⣾⣶⣿⣿⣿⣷⠀⢧⢀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⠁⠀⠀⠛⠛⠛⠛⠿⢿⣿⡀⠀⠀⠀⣿⣿⣿⣿⣿⣯⢉⣙⣿⣿⣿⣿⣿⠀⢸⠘⠀⠀⠀⠀⠀⠀⠀⠀⠸⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣶⠀⠀⠀⠀⠀⠀⢀⡤⠟⠀⠀⠀⠠⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⢸⢠⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣧⠀⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠸⡸⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⠠⡄⠀⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⣿⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣶⣅⠈⠀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⢠⡏⠀⠀⠀⠀⠀⠀⠀⣴⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠁⣼⡟⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⡀⢸⠁⠀⠀⠀⠀⠀⢀⣼⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡀⠀⠀⠀⠘⠐⠀⠀⠀⠀⠀⣿⠀⠘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⣽⣾⣀⠀⠀⠀⢀⣶⣾⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣇⠀⠀⠘⡇⠸⠓⠒⠲⠶⠶⠶⢾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣼⣟⣁⠀⠀⢀⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⠂⠀⠃⠀⠀⠀⠀⠐⣶⣶⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢷⣿⣿⣷⣵⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣦⣀⠀⠀⠀⠀⠀⠉⠉⠉⠉⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⡘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣽⣶⡀⠀⡀⠀⠀⣀⣤⣤⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⢹⣟⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣤⣬⣽⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡄⣸⣿⡄⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⣟⡏⠹⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠙⠲⣄⡙⠿⡟⠛⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⠀⠀⠀⠀⠙⠲⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣀⠀⠀⠀⢠⣼⡿⢟⣻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣅⣠⣴⣿⣷⣾⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
*/