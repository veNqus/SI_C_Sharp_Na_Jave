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

