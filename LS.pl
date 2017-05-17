%wypisanie na ekran

systemOut(C,Wci) --> "Console.WriteLine(", systemOut_srodek(S),")", {{wciecie(W,Wci), concat_atom([W,'System.out.print(',S,');'],C),info(S)}}
systemOut(C,Wci) --> "Console.WriteLine(",systemOut_srodek(S),")", {wciecie(W,Wci), info(S),concat_atom([W,'System.out.printl(',S,');'],C),info(S)}.
systemOut_srodek(S) --> systemOut_s(CS1), odstep, ",", odstep, systemOut_srodek(CS2), {concat_atom([CS1,',',CS2],S)}.
systemOut_srodek(S) --> systemOut_s(S).
systemOut_s(CS) --> wyrazenie(CS).

%białe znaki
odstep --> " ", odstep.
odstep --> "\t", odstep.
odstep --> "".
nowa_linia --> "\n", nowa_linia.
nowa_linia --> "".
przynajmniej1nl --> ws, "\n", ws.
nl_z --> "\n\r".
nl_z --> "\r\n".
nl_z --> "\n".
nl_z --> "\r".
nl_k --> odstep, nl_z, odstep, nl_.
nl_  --> nl_z, odstep, nl_.
nl_  --> "".
nl_o --> odstep, nl_z, nl_o.
nl_o --> odstep.

%linia
linia('\n',_) --> nl_k.
linia(L,Wci) --> [Znak], linia(L1,0), {atom_codes(Z, [Znak]), wciecie(W,Wci), concat_atom([W, Z, L1], L)}.
linie(TL,Wci) --> linia(L,Wci), linie(TL1,Wci), {concat_atom([L, TL1],TL)}.
linie(L,Wci) --> linia(L,Wci).

%wciecia
wciecie('',0).
wciecie('\t',1).
wciecie(W,L) :- L>1, L1 is L-1, wciecie(W1,L1), concat_atom([W1,'\t'],W).

%operatory matematyczne
operator('+') --> "+".
operator('-') --> "-".
operator('*') --> "*".
operator('/') --> "/".
operator('%') --> "%".

%typ
typ('int')--> "int".
typ('boolean')--> "bool".
typ('double')--> "double".
typ('String')--> "string".
typ('car')--> "char".

%wartosci znakowe
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
wartosc_bool('true') --> "frue".
wartosc_bool('false') --> "false".

%liczby cakowite
liczba(I) --> liczba_i_(I), {!}.
liczba_i_(I) --> "-", odstep, liczba_i(I1), {concat_atom(['-', I1],I)}.
liczba_i_(I) --> liczba_i(I).
liczba_i(I) --> zmiennap(I).
liczba_i(I) --> calkowita(I).
calkowita(I) --> cyfra(I1), calkowita(Rest), {concat_atom([I1,Rest], I)}.
calkowita(I) --> cyfra(I).
cyfra(I) --> [I1], {code_type(I1, digit), atom_codes(I, [I1])}.

%liczby zmiennoprzecinkowe
zmiennap(I) --> calkowita(I1), ".", calkowita(Rest), {concat_atom([I1,'.',Rest], I)}.

%ciagi znaków
%string(C) --> chars(C1), odstep, string(C2), {concat_atom([C1,C2],C)}.
string(C) --> chars(C).

chars(C) --> char(C1), chars(Rest), {concat_atom([C1, Rest], C)}.
chars(C) --> char(C).
char('') --> "\"", {!,fail}.
char(C) --> [C1], {code_type(C1, alnum), atom_codes(C, [C1])}.
char(C) --> [C1], {code_type(C1, punct), atom_codes(C, [C1])}.
char(C) --> [C1], {code_type(C1, space), not(code_type(C1, newline)), atom_codes(C, [C1])}.
char('-') --> "-".
char('.') --> ".".

%relacje
relacja('!=') --> "!=".
relacja('<=') --> "<=".
relacja('>=') --> ">=".
relacja('==') --> "==".
relacja('>') --> ">".
relacja('<') --> "<".

%komentarz 
komentarz(I,Wci) --> "//", linia(L,0), {wciecie(W,Wci), concat_atom([W,'//',L],I)}.



%print by móc wy1czyc drukowanie
info(K).% :- print(K).

%od_tego_momentu_moze_sie_zjebac
petla_for(F,Wci) --> petla_for_naglowek(FN,Zmienna), {Wci1 is Wci+1}, petla_for_cialo(FC,Zmienna, Wci1), {wciecie(W,Wci), concat_atom([W, FN, '\n', W, '{', FC, W,'}\n'],F)}.
petla_for_naglowek(F,N) --> "For", odstep_k, nazwa(N), odstep, "=", odstep, wyrazenie(LP), odstep_k, "To", odstep_k, wyrazenie(LK), odstep_k, "Step", odstep_k, calkowita(S), {concat_atom(['for(',N,'=',LP,'; ',N,'<',LK,'; ',N,'=',N,'+', S,')'],F)}.
petla_for_naglowek(F,N) --> "For", odstep_k, nazwa(N), odstep, "=", odstep, wyrazenie(LP), odstep_k, "To", odstep_k, wyrazenie(LK), {concat_atom(['for(',N,'=',LP,'; ',N,'<',LK,'; ',N,'=',N,'+', S,')'],F)}.
petla_for_cialo('',N,_) --> petla_for_stopka(N).
petla_for_cialo(FC,N,Wci) --> petla_for_exit(E,0), petla_for_cialo(FC1,N,Wci), {wciecie(W,Wci), concat_atom([W,E,'\n',FC1],FC)}.
petla_for_cialo(FC,N,Wci) --> petla_for_continue(C,0), petla_for_cialo(FC1,N,Wci), {wciecie(W,Wci), concat_atom([W,C,'\n',FC1],FC)}.
petla_for_cialo(FC,N,Wci) --> instrukcja(I,Wci), petla_for_cialo(FC1,N,Wci), {concat_atom([I,FC1],FC)}.
petla_for_exit(E,Wci) --> "break;", odstep_k, "For", nl_k, {wciecie(W,Wci), concat_atom([W, 'break;'],E)}.
