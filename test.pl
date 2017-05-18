

start(NazwaPliku) :-
	readFromFile("Program.cs", R),
	%readFromFile(NazwaPliku, R),
	print('\nKonwerter z C# na JAVE\n\n'),
	plik(Z, R, []),
	naglowki(N),
	concat_atom([N,Z],Gotowe),
	zapisDoPliku("wynik.java", Gotowe),
	write(Gotowe),!.

readFromFile(File, Output) :-
	open(File, read, Temp, [encoding(utf8)]),
	%open(File, read, Temp),
	read_stream_to_codes(Temp, Output),
	close(Temp).

zapisDoPliku(Plik, Tekst) :-
	open(Plik, write, X),
	current_output(C0),
	set_output(X),
	atom_codes(Y,Tekst),
	write(Y),
	close(X),
	set_output(C0).
	
%plik(P) --> tekst(P).
plik(P) --> modul(M,-1), nl_k, plik(P2), {info('moduly\n'), concat_atom([M,P2],P)}.
plik(P) --> modul(M,-1), nl_o, {info('modul\n'), concat_atom([M],P)}.

naglowki(N) :- concat_atom(['#include "stdafx.h"\n#include <stdio.h>\n#include <iostream>\n#include <math.h>\nusing namespace std;\n\n'],N).
%naglowki(N) :- concat_atom(['#include <stdio.h>\n#include <iostream>\n#include <math.h>\nusing namespace std;\n\n'],N).

%modul(M) --> "Module", tekst(Cialo), "End Module", {concat_atom([Cialo],M)}.
%modul(M) --> "Module", odstep, wyraz(Nazwa), nowa_linia, tekst(MC), "End Module", !, {concat_atom([W,'modul:',Nazwa,'\n',MC],M)}.
namespace(M,Wci) --> "namespace", odstep_k, nazwa(_), nl_k, "{", !, {Wci1 is Wci+1}, modul_cialo(MC,Dekl,Wci1), !, "}", {concat_atom([Dekl,'\n',MC],M)}.
namespace_cialo(M,Dekl,Wci) --> funkcja(F,N,Wci),   modul_cialo(M2,Dekl2,Wci), {concat_atom([F,M2],M), concat_atom([N,Dekl2],Dekl), info(' funkcja ')}.
namespace_cialo(M,Dekl,Wci) --> class(P,N,Wci), modul_cialo(M2,Dekl2,Wci), {concat_atom([P,M2],M), concat_atom([N,Dekl2],Dekl), info(' Klasa ')}.
namespace_cialo('','',_) --> "".

funkcja(F,Dekl,Wci) --> funkcja_nagl(FN), {Wci1 is Wci+1}, funkcja_cialo(FC,Wci1),  {info('funkcja\n'), wciecie(W,Wci), concat_atom(['\n',W,FN,'\n',W,'{\n',FC,W,'}\n'],F), concat_atom([FN, ';\n'],Dekl)}.
funkcja_nagl(FN) --> typ(T), odstep_k, nazwa(Nazwa), odstep, !, "(", parametry(Parametry), ")", !, nl_k, !, {concat_atom([Typ,' ',Nazwa, '(', Parametry, ')'],FN), info(FN)}.
funkcja_cialo('',_) --> funkcja_stopka.
funkcja_cialo(FC,Wci) --> funkcja_return(R,Wci), funkcja_cialo(FC1,Wci), {concat_atom([R,FC1],FC)}.
funkcja_cialo(FC,Wci) --> instrukcja(Cialo,Wci), funkcja_cialo(FC1,Wci), {concat_atom([Cialo,FC1],FC)}.
funkcja_stopka --> "}", nl_k.
funkcja_return(R,Wci) --> "Return", odstep_k, wyrazenie(Wyr), {wciecie(W,Wci), concat_atom([W, 'return ',Wyr,';'],R)}.

class(F,Dekl,Wci) --> class_nagl(PN), {Wci1 is Wci+1}, class_cialo(PC,Wci1), {info('class\n'), wciecie(W,Wci), concat_atom(['\n',W,PN,'\n',W,'{\n',PC,W,'}\n'],F), concat_atom([PN, '\n'],Dekl)}.
class_nagl(PN) --> "class", odstep_k, wyraz(Nazwa), odstep, !, nl_k, {concat_atom(['class ',Nazwa, '{', class_cialo, '}'],PN), info(PN)}.
class_cialo('',_) --> class_stopka.
class_cialo(PC,Wci) --> instrukcja(Cialo, Wci), class_cialo(PC1,Wci), {concat_atom([Cialo,PC1],PC)}.
class_stopka --> "}", nl_k.

parametry(P) --> parametr(P1), odstep, ",", odstep, parametry(P2), {concat_atom([P1,', ',P2],P)}.
parametry(P) --> parametr(P).
parametry('') --> "".
parametr(P) --> typ(T), odstep_k, nazwa(N), odstep_k, {concat_atom([T,' ',N],P)}.

instrukcje(I,Wci) --> instrukcja(I1,Wci), instrukcje(I2,Wci), {concat_atom([I1,I2],I)}.
instrukcje('',_) --> "".

%typ
typ('int')--> "int".
typ('boolean')--> "bool".
typ('double')--> "double".
typ('String')--> "string".
typ('car')--> "char".

%Definicja zmiennych
list_def(LD,Wci) --> definition(D,Wci), nl_k, list_def(LD1,Wci), {concat_atom([D,'\n',LD1],LD)}.
list_def(LD,Wci) --> definition(D,Wci), {concat_atom([D, '\n'],LD)}.
definition(L,Wci) --> typ(T), odstep_k, lista_list_zmiennych(L,Wci).
lista_list_zmiennych(L,Wci) --> lista_zmiennych(LZ,Wci), odstep, ",", odstep, lista_list_zmiennych(LLZ,Wci), {concat_atom([LZ,'\n',LLZ],L),info(L)}.
lista_list_zmiennych(L,Wci) --> lista_zmiennych(L,Wci).
%lista_zmiennych(L,Wci) --> lista_nazw_zmiennnych(LNZ), odstep_k, "As", odstep_k, typ(T), ew_podstawienie(P), {wciecie(W,Wci), concat_atom([W, T,' ',LNZ,P,';'],L),info(L)}.
lista_zmiennych(L,Wci) --> typ(T), zmienne_bez_wart(LNZ), odstep_k, {wciecie(W,Wci), concat_atom([W, T,' ',LNZ,';'],L),info(L)}.
lista_zmiennych(L,Wci) --> typ(T), zmienne_z_wart(LNZ), nazwa(N), odstep_k, podstawienie(P), {wciecie(W,Wci), concat_atom([W,LNZ,T,' ',N, ' = ', P,';'],L),info(L)}.
lista_zmiennych(L,Wci) --> typ(T), zmienne_z_wart(ZZW), zmienne_bez_wart(ZBW), odstep_k, {wciecie(W,Wci), concat_atom([W,ZZW,T,' ', ZBW,';'],L),info(L)}.
zmienne_z_wart(L) --> zmienne_z_wart_(L).
zmienne_z_wart('') --> "".
zmienne_z_wart_(L) --> nazwa(N), podstawienie(P,Typ), odstep, ",", odstep, zmienne_z_wart_(LZ), {concat_atom([Typ, ' ', N, ' = ', P,';\n',LZ],L)}.
zmienne_z_wart_('') --> "".
zmienne_bez_wart(L) --> nazwa(N), odstep, ",", odstep, zmienne_bez_wart(LZ), {concat_atom([N, ', ', LZ],L)}.
zmienne_bez_wart(L) --> nazwa(N),"(", calkowita(LC), ")", odstep, ",", odstep, zmienne_bez_wart(LZ), {concat_atom([N,'[', LC, '], ',LZ],L)}.
zmienne_bez_wart(N) --> nazwa(N).
zmienne_bez_wart(Z) --> nazwa(N),"(", calkowita(LC), ")", {concat_atom([N,'[', LC, ']'],Z)}.
podstawienie(P) --> odstep, "=", odstep, wartosc(P).
podstawienie(P,Typ) --> odstep, "=", odstep, wartosc(P,Typ).


wartosc(W) --> wartosc_int(W).
wartosc(W) --> wartosc_double(W).
wartosc(W) --> wartosc_char(W).
wartosc(W) --> wartosc_bool(W).
wartosc(W, 'int') --> wartosc_int(W).
wartosc(W, 'double') --> wartosc_double(W).
wartosc(W, 'char') --> wartosc_char(W).
wartosc(W, 'bool') --> wartosc_bool(W).
wartosc_int(W) --> calkowita(W).
wartosc_double(W) --> zmiennap(W).
wartosc_char(W) --> "\"", [Znak], "\"", {atom_codes(Z, [Znak]), concat_atom(['\'', Z, '\''],W)}.
wartosc_bool('true') --> "true".
wartosc_bool('false') --> "false".