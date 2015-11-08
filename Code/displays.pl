%%%%%%%%%%%%%%%%%%%%%%
%%	Limpar			%%
%%%%%%%%%%%%%%%%%%%%%%

cls :- write('\e[2J').


%%%%%%%%%%%%%%%%%%%%%%
%%	Menu 			%%
%%%%%%%%%%%%%%%%%%%%%%

menu_principal :-
	cls,
	write('++++++++++++++++++++++++++++++++++++++++'), nl,
	write('++                                    ++'), nl,
	write('++           D O M I N U P            ++'), nl,
	write('++                                    ++'), nl,
	write('++ ---------------------------------- ++'), nl,
	write('++            1 - Jogar               ++'), nl,
	write('++            2 - Jogar CPU           ++'), nl,
	write('++            3 - Sair                ++'), nl,
	write('++++++++++++++++++++++++++++++++++++++++'), nl,
	readInt('Escrever a opção seguida de um ponto', I, 1, 3),
	!,
	main_menu_opcoes(I).
	
main_menu_opcoes(1) :-	jogar(0).
main_menu_opcoes(2) :-	menu_dificuldade.
main_menu_opcoes(3).	
	
	
menu_dificuldade :-
	write('++++++++++++++++++++++++++++++++++++++++'), nl,
	write('++   Dificuldade?                     ++'), nl,
	write('++ ---------------------------------- ++'), nl,
	write('++  1 - Fácil                         ++'), nl,
	write('++  2 - Dificil                       ++'), nl,
	write('++  3 - CPU vs CPU                    ++'), nl,
	write('++++++++++++++++++++++++++++++++++++++++'), nl,
	read(I), I < 4, I > 0,
	!,
	jogar(I).
	
%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%


mostra_peca([_|0]) :- write('     | |').
mostra_peca([V|H]) :- write('  '),
						write(V),
						write('  |'),
						write(H),
						write('|').
						
mostra_linha([]).
mostra_linha([P|[]]) :- 
						P \= [],
						mostra_peca(P).
mostra_linha([P|R]) :- 
					P \= [],
					mostra_peca(P),
					mostra_linha(R).

mostra(_, [], LL) :- mostra_separador(0,LL).
mostra(N, [L|R], LL) :- 
					NN is N+1,
					N >= 10,
					mostra_separador(0,LL),
					write(N), write('|'),
					mostra_linha(L),
					nl,
					mostra(NN, R, LL).
mostra(N, [L|R], LL) :- NN is N+1,
					N < 10,
					mostra_separador(0,LL),
					write(N), write(' |'),
					mostra_linha(L),
					nl,
					mostra(NN, R, LL).

mostra_separador(N,L):-
	N > L,
	nl,
	true.
mostra_separador(N,L):-
	N = 0,
	N =< L,
	NN is N + 1,
	write('--+-'),
	mostra_separador(NN, L).
mostra_separador(N,L):-
	N =< L,
	NN is N + 1,
	write('----+-+-'),
	mostra_separador(NN, L).
	
mostra_N_col(N,L) :-
	N > L,
	true.
mostra_N_col(N,L) :- 
	N = 0,
	N =< L,
	NN is N + 1,
	write('--+'),
	mostra_N_col(NN, L).	
mostra_N_col(N,L) :- 
	N < 10,
	N =< L,
	NN is N + 1,
	write('  '),
	write(N),
	write('  +-+'),
	mostra_N_col(NN, L).			
mostra_N_col(N,L) :- 
	N >= 10,
	N =< L,
	NN is N + 1,
	write(' '),
	write(N),
	write('  +-+'),
	mostra_N_col(NN, L).				
				
				
mostra_tabuleiro([L|R]) :-  	
		%tabuleiro([L|R]),
		length(L,N),
		mostra_N_col(0, N), nl,
		mostra(1, [L|R], N),
		nl.

%%%%%%Exemplo - comando |?- exemplo_mostra_tab. em Prolog
exemplo_mostra_tab :- tabuleiro(L), write('Tabuleiro:'), nl, mostra_tabuleiro(L).

%%%%%%%%%%%%%%%%%%%%%%
%%	Mãos 			%%
%%%%%%%%%%%%%%%%%%%%%%

%+ # + # + # +
%+---+---+---+
%| V | V | V |
%+---+---+---+
%| V | V | V |
%+---+---+---+

mostra_mao_linha_V1([]).					
mostra_mao_linha_V1([ [V1|_] |R]) :-
	write(' '), write(V1), write(' |'),
	mostra_mao_linha_V1(R).
	
mostra_mao_linha_V2([]).					
mostra_mao_linha_V2([ [_|V2] |R]) :-
	write(' '), write(V2), write(' |'),
	mostra_mao_linha_V2(R).
				
mostra_mao_cabecalho(L):-
	write('+'),
	!,
	incrementador(1, N, L),
	mostra_mao_cabecalho_aux(N),
	N>=L.
mostra_mao_cabecalho_aux(N) :- N < 10, write(' '),write(N),write(' +').
mostra_mao_cabecalho_aux(N) :- N >= 10, write(' '),write(N),write('+').
	
mostra_mao_separador(L):-
	write('+'),
	!,
	incrementador(1, N, L),
	write('---+'),
	N>=L.


mostra_mao_jogador(J) :- 
	jogador(J),
	mao(J, M),
	write('Mao do Jogador '),
	write(J),
	write(':'), 
	nl, 
	length(M, L),
	mostra_mao_cabecalho(L),nl,
	mostra_mao_separador(L),nl,
	write('|'), mostra_mao_linha_V1(M), nl,
	mostra_mao_separador(L),nl,
	write('|'), mostra_mao_linha_V2(M), nl,
	mostra_mao_separador(L),nl.