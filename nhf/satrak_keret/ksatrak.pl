% :- type fLeiro  ---> satrak(sSzS, sSzO, list(parc)).
% :- type sSzS    == list(int).
% :- type sSzO    == list(int).
% :- type parc    == int-int.
% :- type irany   ---> n    % észak
%                 ;    e    % kelet
%                 ;    s    % dél
%                 ;    w.   % nyugat
% :- type sHelyek   == list(irany).
% :- type file == atom.

:- module(ksatrak, [
	satrak_be/2,    % satrak_be(file::in, fLeiro::out) 
	satrak_ki/3,	% satrak_ki(file::in, sHelyek::in, fLeiro::in)
	megold/2,	% megold(file::in, file::in)
	stopper/2,	% stopper(file::in, file::in)
	teljes_teszt/1
			]
	 ).

:- use_module(library(lists), [select/3]).

%-------------- Bejaratok ------------------------------------

% satrak_be(+File, -Leiro):
% A File-bol beolvashato egy feladat, melyet a Leiro ir le
satrak_be(File, Leiro) :-
	open_file(File, read, Stream),
	read_in(Stream, Characters),
	close(Stream),
	parse_data(Characters, Leiro).

% satrak_ki(+File, +SHelyek, +Leiro):
% A Leiro altal leirt feladat Helyek megoldasat kiirja a File allomanyba.
satrak_ki(File, SHelyek, Leiro) :-
	open_file(File, write, Stream),
	current_output(OOut),
	set_output(Stream),
	call_cleanup(pretty_print(SHelyek, Leiro),
		     (set_output(OOut),close(Stream))).

% megold(+FileIn, +FileOut, -S, -T):
% A FileIn allomanybol beolvasott feladat minden megoldasat
% kiirja a FileOut allomanyba. A megoldasok listaja S, a futasi ido T msec.
megold(FileIn, FileOut, Solutions, T) :-
 	satrak_be(FileIn, Leiro),
	ensure_loaded(user:satrak),
	statistics(runtime, [T0|_]),
	megoldasok(Leiro, Solutions),
	statistics(runtime, [T1|_]),
	T is T1 - T0,
	open_file(FileOut, write, Stream),
	current_output(OOut),
	set_output(Stream),
	call_cleanup((format('~p.\nend_of_file.\n',[Solutions]),
		      print_solutions(Leiro)),
		     (set_output(OOut),close(Stream))).

% megoldasok(+Colours, +Hints, -Solutions):
% A Leiro jellemzoju feladat osszes megoldasa
% a Solutions rendezett lista.
megoldasok(Leiro, Solutions) :-
	findall(Solution,
		user:satrak(Leiro, Solution),
		Solutions0),
	samsort(Solutions0, Solutions).


% % megold(+FileIn, +FileOut):
% % A FileIn allomanybol beolvasott feladat minden megoldasat
% % kiirja a FileOut allomanyba.
% megold(FileIn, FileOut) :-
% 	satrak_be(FileIn, Leiro),
% 	ensure_loaded(user:satrak),
% 	open_file(FileOut, write, Stream),
% 	current_output(OOut),
% 	set_output(Stream),
% 	call_cleanup(print_solutions(Leiro),
% 		     (set_output(OOut),close(Stream))).

% print_solutions(+Leiro):
% a Leiro altal megadott feladat osszes megoldasat
% kiirja az aktualis kimenetre.
print_solutions(Leiro) :-
	user:satrak(Leiro, Helyek),
	pretty_print(Helyek, Leiro),
	fail.
print_solutions(_).

% stopper(+FileIn, +FileOut, -Solutions):
% A FileIn allomanybol beolvasott feladat minden megoldasat kiirja a
% FileOut allomanyba. A szabvanyos kimenetre kiirja a futasi idot es a
% megoldasok szamat. Solutions a megoldasok listaja.
stopper(FileIn, FileOut, Solutions) :-
	megold(FileIn, FileOut, Solutions, T),
	jellemzok_ki(FileIn),
	length(Solutions, SolNb),
	format(', megoldas: ~|~t~d~4+ db, ~|~t~3d~8+ sec', [SolNb,T]).

jellemzok_ki(FileIn) :-
	satrak_be(FileIn, satrak(Ss, So, Fs)),
	length(Ss, N),
	length(So, M),
	length(Fs, FaNb),
	findall(1, (   member(L, [Ss,So]), member(O, L), O >= 0   ), OFelts),
	length(OFelts, OsszegNb),
	format('Feladvany: ~a,~|~t~w~6+,~|~t~d~4+ fa, ~|~t~d~4+ osszf', [FileIn,N*M,FaNb,OsszegNb]).



% stopper(+FileIn, +FileOut):
% Mint megold/2, de a futashoz szukseges idot is kiirja.
stopper(FileIn, FileOut) :-
	stopper(FileIn, FileOut, _).

% megold(+FileIn, +FileOut):
% A FileIn allomanybol beolvasott feladat minden megoldasat
% kiirja a FileOut allomanyba.
megold(FileIn, FileOut) :-
	megold(FileIn, FileOut, _S, _T).



%-------------- Allomanyok kezelese --------------------------

% open_file(+File, +Mode, -Stream):
% A File allomanyt megnyitja a Stream folyam formajaban Mode modban.
open_file(user, M, S) :-
	!, standard_stream(M, S).
open_file(File, Mode, Stream) :-
	open(File, Mode, Stream).

% standard_stream(?Mode, ?Stream):
% A Mode modu szabvanyos folyam a Stream.
standard_stream(read, user_input).
standard_stream(write, user_output).

%-------------- Beolvasas ------------------------------------

% read_in(+S, -L):
% S folyambol beolvashato karakterek listaja L.
read_in(S, L) :-
	get_code(S, C),
	read_in(C, S, L).

% read_in(+C, +S, -L):
% Ha C az S folyambol elozoleg beolvasott karakter, akkor L az S-bol
% beolvashato maradek karakterek listaja, C-t is beleertve.  Ha C = -1,
% akkor L = [].
read_in(C, _, L) :-
	C =:= -1, !, L = [].
read_in(C, S, [C|L]) :-
	get_code(S, C1),
	read_in(C1, S, L).

% parse_data(+Chars, -Leiro):
% Chars fuzerbol kielemezheto egy Leiroval megadott feladat
parse_data(Chars, Leiro) :-
	descriptor(Leiro, Chars, Rest), !,
	correct_syntax(Rest),
	correct_matrix(Leiro).
parse_data(_, _) :-
	warning('Bad format of input file.',[]),
	fail.

%-------------- Ellenorzesek ---------------------------------

% correct_syntax(+Rest):
% A Rest fuzer a bemenet helyes maradeka, azaz ures.
correct_syntax(Rest) :-
	Rest = [_|_], !, 
	chars_of_type(mid_line, L, Rest, _),
	warning('non-expected character in line: "~s"', [L]),
	fail.
correct_syntax(_).

% correct_matrix(+Leiro):
% a Leiro altal megadott feladat lehetseges
% (nem lognak ki a tablarol satrak)
correct_matrix(satrak(Ss, So, Fs)) :-
	max_val(Fs, Msor, Moszl),
	length(Ss, Ssz),
	length(So, Osz),
	( Ssz<Msor ; Osz<Moszl ), !,
	warning('invalid table', []),
	fail.
correct_matrix(_).

% max_val(+Fak, -Max_sor, -Max_oszlop)
% Fak elemei kozul a legjobboldalibb Max_oszlop. oszlopban van,
% a legalso pedig a Max_sor. sorban
max_val(Fs, Ms, Mo) :-
	max_val(Fs, 0, 0, Ms, Mo).

% max_val(+Fakm, +Ms0, +Mo0, -Ms, -Mo): Fak eddig feldolgozott reszeben
% a legjobboldalibb pozicio Mo0, a legalso pedig Ms0;
% Fakm Fak meg fel nem dolgozott resze
% Fak-ban a legjobboldalibb pozicio Mo, a legalso pedig Mo
max_val([], Ms, Mo, Ms, Mo).
max_val([Sor-Oszlop|L], Ms0, Mo0, Ms2, Mo2) :-
	Ms1 is integer(max(Sor, Ms0)),
	Mo1 is integer(max(Oszlop, Mo0)),
	max_val(L, Ms1, Mo1, Ms2, Mo2).

% dir(+Irany, -Xd, -Yd, -Ell_irany): Iranyhoz tartozo vizszintes eltolas Xd,
% fuggoleges eltolas Yd. Irany es Ell_irany ellentetesek.
dir(e, 1, 0, w).
dir(w, -1, 0, e).
dir(s, 0, 1, n).
dir(n, 0, -1, s).

% tent_places(+Fak, +Iranyok, -Fak_i, -Satrak_i): Fak koordinata-parok
% listaja, Fak n. elemenek megfelelo fatol Irany n. elemenek megfelelo
% iranyban van egy sator; Fak_i n. eleme az n. fa koordinatai es a hozza
% tartozo sator iranya, Satrak_i n. eleme az n. sator koordinatai es a
% hozza tartozo fa iranya
tent_places([] ,[] , [], []) :- !.
tent_places([Y-X|Fs], [Dir|Dirs], [Y-X-Dir|Fh], [Y1-X1-NDir|Sh]) :-
	dir(Dir, Xd, Yd, NDir),
	X1 is X+Xd,
	Y1 is Y+Yd,
	!,
	tent_places(Fs, Dirs, Fh, Sh).
tent_places(_, [Dir|_], _, _) :-
	dir(Dir, _, _, _),
	!,
	warning('Too long direction list', []),
	fail.
tent_places(_, [Dir|_], _, _) :-
	!,
	warning('Invalid direction: ~w', [Dir]),
	fail.
tent_places(_, _, _, _) :-
	warning('Too short direction list', []),
	fail.

%-------------- DCG elemzes ----------------------------------

% Kielemezheto egy feladvany-leiro
descriptor(satrak(Ss, So, Fs)) -->
	chars_of_type(horiz_sp, _),
	int_list(So),
	row_list(Ss, Fs, 1),
	chars_of_type(layout, _).

% Kielemezheto az Int egesz, amit esetleg nem lathato karakterek
% eloznek meg.
first_int(Int) -->
	chars_of_type(horiz_sp, _),
	int_number(Int).

% row_list(-Sorsatrak, -Fak, +Sorszam, +Chars, -Rest)
% kielemezheto egy feladvany leirasa az Lnum. sortol kezdodoen
row_list([S|Ss], Fs, Lnum) -->
	first_int(S),
	chars_of_type(horiz_sp, _),
	field_list(Lnum, 1, Fs, Fs0),
	!,
	{Lnum1 is Lnum+1},
	row_list(Ss, Fs0, Lnum1).
row_list([], [], _) --> [].

% kielemezheto '-' es 'F' szimbolumok listaja Lnum-Fnum poziciotol kezdodoen,
% Fs0-Fs falista
field_list(Lnum, Fnum, Fs, Fs0) -->
	[0'-],
	{Fnum0 is Fnum+1},
	chars_of_type(horiz_sp, _),
	field_list(Lnum, Fnum0, Fs, Fs0).
field_list(Lnum, Fnum, [Lnum-Fnum|Fs], Fs0) -->
	[0'F],
	{Fnum0 is Fnum+1},
	chars_of_type(horiz_sp, _),
	field_list(Lnum, Fnum0, Fs, Fs0).
field_list(_, _, Fs, Fs) -->
	char_of_type(vert_sp, _).

% int_list(-IntList) -->
% Kielemezheto az IntList egeszek listaja, ujsorral lezarva.  
% A bemenet nem kezdodhet nem lathato karakterrel.
int_list([Int|IntList]) -->
	int_number(Int), !,
	chars_of_type(horiz_sp, _),
	int_list(IntList).
int_list([]) -->
	char_of_type(vert_sp, _).

% int_number(Intn) -->
% Kielemezheto egy (tizes szamrendszerbeli) Int egesz szam.
int_number(Intn) -->
	[0'-], !,
	int_number(Int),
	{Intn is -Int}.
int_number(Int) -->
	chars_of_type(digit, Ds),
	{Ds = [_|_], number_codes(Int, Ds)}.

% layout_char(Dir, C):
% C egy Dir (horiz_sp|vert_sp) iranyu nem lathato karakter.
layout_char(horiz_sp, 0' ).
layout_char(horiz_sp, 0'\t).
layout_char(vert_sp,  0'\n).
layout_char(vert_sp,  0'\r).
layout_char(vert_sp,  0'\f).
layout_char(vert_sp,  0'\v).

% Kielemezheto egy Type tipusu C karakter.
char_of_type(Type, C) -->
	[C], {char_type(Type, C)}.

% chars_of_type(Type, Cs) -->
% Kielemezheto Type tipusu karakterek egy Cs listaja.	
chars_of_type(Type, [C|Cs]) -->
	char_of_type(Type, C), !,
	chars_of_type(Type, Cs).
chars_of_type(_, []) --> [].

% char_type(Type, C) :
% A C karakter Type tipusu.
char_type(layout, C) :-
	layout_char(_, C).
char_type(digit, D) :-
	D >= 0'0, D =< 0'9.
char_type(mid_line, C) :-
	\+ layout_char(vert_sp, C).
char_type(Dir, C) :-
	layout_char(Dir, C).

%-------------- Kiiratas -------------------------------------

% pretty_print(+Helyek, +Leiro):
% kiirja a Leiro-hoz tartozo feladat Helyek megoldasat.
pretty_print(SHelyek, satrak(Ss, So, Fs)) :-
	length(Ss, Ssz),
	length(So, Osz),
	tent_places(Fs, SHelyek, Fak, Satrak),
	make_table(Ssz, Osz, Tabla, 1, Fak, Satrak),
	write('    '),
	printnums(So),
	nl,
	write('    '),
	print_separator(Osz, 2),
	pretty_print2(Tabla, Ss).

% printnum(+Szam): kiirja a legfeljebb 2 jeggyel kiirhato egesz Szam-ot
% egy 3 szeles mezo kozepebe
printnum(D) :-
	format(' ~|~t~w~2+',[D]).

% printnums(+Szamlista): kiirja a Szamlista elemeit ugy, hogy minden szam
% negy mezot foglal el
printnums([]).
printnums([X|XS]) :-
	printnum(X),
	printnums(XS).

% make_table(+Sorokszama, +Oszlopokszama, -Sorlista, +Sorszam, +Fak, +Satrak):
% Sorokszama*Oszlopokszama meretu tabla sorait kesziti el Sorszam. sortol
% kezdve a Sorlista-ba ugy, hogy ebbe elhelyezi a Fak es Satrak poziciolistak
% altal kijelolt fakat es satrakat.
make_table(Ssz, _, [], S, [], []) :-
	S>Ssz, !.
make_table(Ssz, _, _, S, _, _) :-
	S>Ssz, !,
	warning('Bad solution: several items (tent/tree) in the same square\n\
                        or items out of field', []),
	fail.
make_table(Ssz, Osz, [Sor|Sorok], S, Fak, Satrak) :-
	make_row(Osz, Sor, S, 1, Fak, Satrak, Fak1, Satrak1),
	S1 is S+1,
	!,
	make_table(Ssz, Osz, Sorok, S1, Fak1, Satrak1).

% make_row(+Oszlopokszama, -Sor, +Sorszam, +Mezoszam, +Fak0, +Satrak0,
%                                                         -Fak1, -Satrak1):
% Sor-ba elkesziti a Sorszam. sor Mezoszam. poziciojatol kezdodo reszet
% ugy, hogy elhelyezi benne a Fak0 es Satrak0 poziciolistak alapjan
% megadott fak es satrak kozul azokat, amelyek az adott sorba esnek;
% az elhelyezett fak es sartakat kihagyva a listabol adodik Fak1 es Satrak1
make_row(Osz, [], _, X, Fak, Satrak, Fak, Satrak) :-
	Osz<X, !.
make_row(Osz, [M|Sor], S, X, Fak, Satrak, Fak1, Satrak1) :-
	(   select(S-X-D, Fak, Fak0) -> Satrak=Satrak0, M='F'-D
	;   select(S-X-D, Satrak, Satrak0) -> Fak=Fak0, M='S'-D
	;   Fak=Fak0, Satrak=Satrak0, M=' '
	),
	X1 is X+1,
	make_row(Osz, Sor, S, X1, Fak0, Satrak0, Fak1, Satrak1).

% pretty_print2(+Tabla, +Ss): elokeszitett Tabla es Ss sorlista alapjan
% kiirja az aktualis output-ra az adott megoldast
pretty_print2([],[]) :- nl.
pretty_print2([R|Rs],[S|Ss]) :-
	printnum(S),
	write(' |'),
	pretty_print_row(R),
	write('    +'),
	pretty_print_horiz(R),
	pretty_print2(Rs,Ss).

% pretty_print_row(+Sor): kiirja Sor sort
pretty_print_row([]) :-
	nl.
pretty_print_row([M-e|R]) :-
	!,
	format('~w--',[M]),
	pretty_print_row(R).
pretty_print_row([M-_|R]) :-
	!,
	format('~w |',[M]),
	pretty_print_row(R).
pretty_print_row([_|R]) :-
	write('  |'),
	pretty_print_row(R).	

% pretty_print_horiz(+Sor): kiirja Sor sor alatti vizszintes elvalasztot
pretty_print_horiz([]) :-
	nl.
pretty_print_horiz([_-s|R]) :-
	!,
	write('|-+'),
	pretty_print_horiz(R).
pretty_print_horiz([_|R]) :-
	write('--+'),
	pretty_print_horiz(R).

% print_separator(+N, +P):
% Egy N oszlopos, P mezoszelessegu szeparator-sort nyomtat.
print_separator(0, _) :-
	write('+\n').
print_separator(N, P) :-
	N > 0,
	format('+~*c', [P,0'-]),
	N1 is N-1,
	print_separator(N1, P).

%-------------- Seged-eljarasok ------------------------------

% warning(+Txt, +Args):
% Kiirja a Txt szoveggel es Args argumentumlistaval
% megadott figyelmezteto szoveget.
warning(Txt, Args) :-
	print_message(warning, format(Txt, Args)).

% Az alábbi eljárást Eisenberger András ültette át Prologra Erlangból, és
% egészítette ki a megoldás ellenörzésével.

:- use_module(library(file_systems)).
:- use_module(library(timeout)).
:- use_module(library(samsort)).

% A "tests" könyvtárban lévõ összes "testXXXd.txt" tesztállomány esetén
%  - lefuttatja a tesztet Timeout másodperces idõkorláttal,
%  - ellenõrzi, hogy a "testXXXs.txt" állományban megadott megoldáshalmazt kapta-e
%    eredményül,
%  - olvasható formában (lásd megold/2) kiírja az eredményt a "tests_out_pl"
%    könyvtár "testXXXt.txt" nevû állományába.
% Az állománynevekben az XXX szám tetszõleges hosszúságú lehet.
teljes_teszt(Timeout) :-
	Time_MS is Timeout * 1000,
	(   directory_exists(tests_out_pl) -> true
	;   make_directory(tests_out_pl)
	),
	file_members_of_directory('tests', 'test*d.txt', FoundFiles),
	(   member(BaseName-_AbsPath, FoundFiles),
	    atom_concat('tests/', BaseName, InPath),
	    atom_concat(TestName, 'd.txt', BaseName),
	    atom_concat('tests/', TestName, 's.txt', SolsPath),
	    atom_concat('tests_out_pl/', TestName, 't.txt', OutPath),
            time_out(stopper(InPath, OutPath, Sols),
		     Time_MS, Result),
            (   Result == success ->
		samsort(Sols, SolsSorted),
		catch(read_term_from_file(SolsPath, SolsRead), _,
		      SolsRead = none),
		(   SolsRead == none -> write(' NINCS MEGOLDÁS\n')	
	;   SolsRead == SolsSorted -> write(' HELYES\n')
		;   write(' NEM HELYES\n')
		)
	    ;	jellemzok_ki(InPath), format(', tullepte az idokorlatot (~d s)~n',
		       [Timeout])
            ),
	    flush_output,
	    fail
	;   true
	).

% atom_concat(A, B, C, ABC): Az A, B es C atomok összef zése ABC.
atom_concat(A, B, C, ABC) :-
	atom_concat(A, B, AB),
	atom_concat(AB, C, ABC).

% read_term_from_file(File, Term): A Term Prolog kifejezést beolvassa a
% File állományból.
read_term_from_file(File, Term) :-
	open_file(File, read, Stream),
	call_cleanup(read(Stream, Term),
		     close(Stream)).
