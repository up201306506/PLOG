%%%%%%%%%%%%%%%%%%%%%%
%%	Limpar			%%
%%%%%%%%%%%%%%%%%%%%%%

cls :- write('\e[2J').

%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%
				


mostra_peca([_|0]) :- write('     ').
mostra_peca([V|H]) :- write('['),
						write(V),
						write('|'),
						write(H),
						write(']').
						
mostra_linha([]).
mostra_linha([P|[]]) :- mostra_peca(P).
mostra_linha([P|R]) :- mostra_peca(P),
					mostra_linha(R).

mostra(N, [L|[]]) :- write(N), write('|'), mostra_linha(L).
mostra(N, [L|R]) :- NN is N+1,
					N >= 10,
					write(N), write('|'),
					mostra_linha(L),
					nl,
					mostra(NN, R).
mostra(N, [L|R]) :- NN is N+1,
					N < 10,
					write(N), write(' |'),
					mostra_linha(L),
					nl,
					mostra(NN, R).
			
mostra_N_col(N,L) :-
						N > L,
						true.
mostra_N_col(N,L) :- 
					N = 0,
					N =< L,
					NN is N + 1,
					write('   '),
					mostra_N_col(NN, L).
					
					
mostra_N_col(N,L) :- 
					N < 10,
					N =< L,
					NN is N + 1,
					write('  '),
					write(N),
					write('  '),
					mostra_N_col(NN, L).			
mostra_N_col(N,L) :- 
					N >= 10,
					N =< L,
					NN is N + 1,
					write(' '),
					write(N),
					write('  '),
					mostra_N_col(NN, L).				
				
mostra_tabuleiro(T) :-  	tabuleiro(T),
							mostra_N_col(0, 12), nl,
							mostra(1, T),
							nl.

%%%%%%Exemplo - comando |?- exemplo_mostra_tab. em Prolog
exemplo_mostra_tab :- tabuleiro(L), write('Tabuleiro:'), nl, mostra_tabuleiro(L).


%%%%%%%%%%%%%%%%%%%%%%
%%	Mãos 			%%
%%%%%%%%%%%%%%%%%%%%%%


mostra_mao_peca([V|H]) :- write('['),
						write(V),
						write('|'),
						write(H),
						write(']').
mostra_mao_linha([]).
mostra_mao_linha([P|[]]) :- mostra_mao_peca(P).
mostra_mao_linha([P|R]) :- mostra_mao_peca(P),
					mostra_mao_linha(R).


mostra_mao_jogador(J) :- 
	jogador(J),
	mao(J, M),
	write('Mao do Jogador '),
	write(J),
	write(':'), 
	nl, 
	mostra_mao_linha(M),
	nl.
	
	
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
	write('++            2 - Regras              ++'), nl,
	write('++            3 - Sair                ++'), nl,
	write('++++++++++++++++++++++++++++++++++++++++'), nl.
	
menu_dificuldade :-
	cls,
	write('++++++++++++++++++++++++++++++++++++++++'), nl,
	write('++   Dificuldade?                     ++'), nl,
	write('++ ---------------------------------- ++'), nl,
	write('++  1 - Fácil                         ++'), nl,
	write('++  2 - Dificil                       ++'), nl,
	write('++++++++++++++++++++++++++++++++++++++++'), nl.