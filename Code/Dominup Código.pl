
:- use_module(library(random)).

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
mao(b, []).
mostra_mao_exemplo :- mao(J, L), write('Mao do Jogador '), write(J), jogador(J), write(':'), nl, mostra_linha(L).

% V - Mao Velha
% N - Mao Nova
% J - Jogador
% P - Peça
acrecentar_peca_mao(P, J) :- 	mao(J, V),
								append(P,V,N),
								retract(mao(J,V)),
								assert(mao(J, N)).
										
						
mudar_mao :-	mostra_mao_exemplo,
				retract(mao(J, T)),
				nl,
				assert(mao(a, [ [2|1],[3|4],[5|5],[6|6],[8|1] ])),
				mostra_mao_exemplo.				
							


%%%%%%%%%%%%%%%%%%%%%%
%%	Estado			%%
%%%%%%%%%%%%%%%%%%%%%%

:- dynamic estado/1.
estado(T, J) :- tabuleiro(T), jogador(J).



%%%%%%%%%%%%%%%%%%%%%%
%%	Main			%%
%%%%%%%%%%%%%%%%%%%%%%

jogar :- 	repeat,
			dar_baralho,
			!.



:- dynamic baralho/1.
baralho([ [0|0], [0|1], [0|2], [0|3], [0|4], [0|5], [0|6], [0|7], [1|1], [1|2], 
			[1|3], [1|4], [1|5], [1|6], [1|7], [2|2], [2|3], [2|4], [2|5], [2|6],
			[2|7], [3|3], [3|4], [3|5], [3|6], [3|7], [4|4], [4|5], [4|6], [4|7],
			[5|5], [5|6], [5|7], [6|6], [6|7], [7|7] ]).

baralho_vazio :- 	baralho(B),
					mostra_linha(B),
					!,
					length(B,0).

					
element_at(X,[X|_],1).
element_at(X,[_|L],K) :- element_at(X,L,K1), K is K1 + 1.

get_peca_from_baralho(P) :- baralho(B),	
							length(B, L),
							LS is L + 1,
							random(0, LS, X),
							element_at(P, B, X).
							
first(F, [F|_]).


delete_one(X,L1,L2) :- 	append(A1,[X|A2],L1),
						append(A1,A2,L2).

						:- dynamic jogador_escolhido/1.
jogador_escolhido(a).

trocar_jogador(J) :- jogador(X),
						X \= J,
						retract(jogador_escolhido(J)),
						assert(jogador_escolhido(X)).
						
						
dar_baralho :- 	repeat,
				baralho(B),
				get_peca_from_baralho(P),
				delete_one(P,B,C),
				retract(baralho(B)),
				assert(baralho(C)),
				jogador_escolhido(J),
				acrecentar_peca_mao([P],J),
				trocar_jogador(J),
				baralho(Y),
				length(Y, 0).
		
				



		
		
		