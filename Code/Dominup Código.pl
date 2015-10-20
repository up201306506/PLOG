
%%%%%%%%%%%%%%%%%%%%%%
%%	tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%
mostra_peca([V|0]) :- write('     ').
mostra_peca([V|H]) :- write('['),
						write(V),
						write('|'),
						write(H),
						write(']').
						
mostra_linha([]).
mostra_linha([P|[]]) :- mostra_peca(P).
mostra_linha([P|R]) :- mostra_peca(P),
					mostra_linha(R).

mostra([L|[]]) :- mostra_linha(L).
mostra([L|R]) :- mostra_linha(L),
				nl,
				mostra(R).
				
				
mostra_tabuleiro([L|R]) :-  write('  1    2    3    4    5    6    7    8    9   10   11   12  '), nl,
							mostra([L|R]).
			
%%%%%%Exemplo - comando |?- mostra_tab_exemplo. em Prolog

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

mostra_tab_exemplo :- tabuleiro(L), write('Tabuleiro:'), nl, mostra_tabuleiro(L).


%%%%%%%%%%%%%%%%%%%%%%
%%	Jogador			%%
%%%%%%%%%%%%%%%%%%%%%%

jogador(a).
jogador(b).


%%%%%%%%%%%%%%%%%%%%%%
%%	mao jogador		%%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%Exemplo - comando |?- mostra_mao_exemplo. em Prolog

:- dynamic mao/2.

mao(a, []).
mostra_mao_exemplo :- mao(J, L), write('Mao do Jogador '), write(J), jogador(J), write(':'), nl, mostra_linha(L).


%%%%%%%%%%%%%%%%%%%%%%
%%	Estado			%%
%%%%%%%%%%%%%%%%%%%%%%

:- dynamic estado/1.
estado(T, J) :- tabuleiro(T), jogador(J).



%%%%%%%%%%%%%%%%%%%%%%
%%	Main			%%
%%%%%%%%%%%%%%%%%%%%%%



:- dynamic baralho/1.
baralho([ [2|2], [2|1], [1|1], [3|2] ]).

%baralho_vazio :- 	baralho(B),
%					mostra_linha(B),
%					!,
%					length(B,0).


first(F, [F|_]).
delete_one(X,L1,L2) :- 	append(A1,[X|A2],L1),
						append(A1,A2,L2).

dar_baralho :- 	repeat,
				baralho(B),
				mostra_linha(B),
				nl,
				first(F, B),
				delete_one(F,B,C),
				retract(baralho(B)),
				assert(baralho(C)),
				baralho(Y),
				length(Y, 0).
				
				


mudar_mao :-	mostra_mao_exemplo,
		retract(mao(J, T)),
		nl,
		assert(mao(a, [ [2|1],[3|4],[5|5],[6|6],[8|1] ])),
		mostra_mao_exemplo.
		
		
		