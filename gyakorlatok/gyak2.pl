insert_ord([], E, [E]).
insert_ord(L0, E, L) :-
	L0 = [X|L1],
	(   X > E -> L = [E|L0]
	;   X =:= E -> L = L0
	;   insert_ord(L1, E, L2),
        L = [X|L2]
	    
	).

graph([]).
graph([N1-N2|Es]) :-
	atom(N1), atom(N2), graph(Es).

same_edge(A-B, A-B).
same_edge(A-B, B-A).

same_graph0([], []).
same_graph0([E|FGT], SG) :-
	same_edge(E, E1),
	select(E1, SG, SGR),
	same_graph0(FGT, SGR).

same_graph(FG, SG) :-
	same_length(FG, SG),
	same_graph0(FG, SG).


line([], _).
line([P-Q|L], P) :-
	line(L, Q).

draw1(G, L) :-
	same_length(G, L),
	line(L, _),
	same_graph(L, G).