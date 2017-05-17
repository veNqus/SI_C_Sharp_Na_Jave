start(NazwaPliku) :-
	readFromFile("test.cs", R),
	%readFromFile(NazwaPliku, R),
	print('\nKonwerter z C# na JAVE\n\n'),

	zapisDoPliku("wynik.java", R).

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
	
