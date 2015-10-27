		
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
					LL is L+1,
					mostra_N_col(NN, L).			
mostra_N_col(N,L) :- 
					N >= 10,
					N =< L,
					NN is N + 1,
					write(' '),
					write(N),
					write('  '),
					LL is L-1,
					mostra_N_col(NN, L).
					
		
tabuleiro([ [[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]], 
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]] ]).
				
				
mostra_tabuleiro([L|R]) :-  mostra_N_col(0, 12), nl,
							mostra(1, [L|R]).
							
		
%%%%%%Exemplo - comando |?- mostra_tab_exemplo. em Prolog
mostra_tab_exemplo :- tabuleiro(L), write('Tabuleiro:'), nl, mostra_tabuleiro(L).
