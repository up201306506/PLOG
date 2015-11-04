%%%%%%%%%%%%%%%%%%%%%%
%%	Displays		%%
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
				
mostra_tabuleiro([L|R]) :-  tabuleiro([L|R]),
							mostra_N_col(0, 12), nl,
							mostra(1, [L|R]),
							nl.

%%%%%%Exemplo - comando |?- exemplo_mostra_tab. em Prolog
exemplo_mostra_tab :- tabuleiro(L), write('Tabuleiro:'), nl, mostra_tabuleiro(L).





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