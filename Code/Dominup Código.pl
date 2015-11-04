
:- use_module(library(random)).
:- use_module(displays).

:-setrand(888).



%%%%%%%%%%%%%%%%%%%%%%
%%	Init			%%
%%%%%%%%%%%%%%%%%%%%%%


jogador(a).
jogador(b).

:- dynamic tabuleiro/1.
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
			

:- dynamic mao/2.			
mao(a, []).
mao(b, []).

:- dynamic jogador_escolhido/1.
jogador_escolhido(a).

:- dynamic estado/1.
estado(T, J) :- tabuleiro(T), jogador_escolhido(J).


%%%%%%%%%%%%%%%%%%%%%%
%%	Jogador			%%
%%%%%%%%%%%%%%%%%%%%%%

jogadro_trocar_vez(J) :- jogador(X),
						X \= J,
						retract(jogador_escolhido(J)),
						assert(jogador_escolhido(X)).

%%%!!!!!!!!!!!!!!!!!!!
%jogador_jogadas_disponiveis_climb(J, P, C1, C2)
%jogador_jogadas_disponiveis_expand(J, P, C1, C2)

%jogador_pode_jogar(T, J)

						

%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%

tabuleiro_set(TI, [C|L], V, A, TF) :-
	tabuleiro(TI),
	matrix_setCell(L, C, TI, [V|A], TF),
	retract(tabuleiro(TI)),
	assert(tabuleiro(TF)).
	
tabuleiro_get(T,[C|L], V, A) :- 
	tabuleiro(T),
	list_element_at(Y, T, L),
	list_element_at([V|A], Y, C).

%%%!!!!!!!!!!!!!!!!!!!
%tabuleiro_jogar_peca_climb(P, C1, C2)
%tabuleiro_jogar_peca_expand(P, C1, C2)


%%%%%%%%%%%%%%%%%%%%%%
%%	mao jogador		%%
%%%%%%%%%%%%%%%%%%%%%%

% V - Mao Velha
% N - Mao Nova
% J - Jogador
% P - Peça
mao_acrecentar_peca(P, J) :- 	mao(J, V),
								append(P,V,N),
								retract(mao(J,V)),
								assert(mao(J, N)).
															
mao_remover_peca(J, P) :- 
	jogador(J),
	mao(J,MV),
	list_delete_one(P, MV, MN),
	retract(mao(J,MV)),
	assert(mao(J,MN)).
						
%%%!!!!!!!!!!!!!!!!!!!							
%mao_escolher_peca(J, X) :-

mao_vazia(J) :-
	jogador(J),
	mao(J, M),
	length(M,0).
						
								
mao_quem_tem_peca(P, J) :-
	mao(J,M),
	member(P,M).

				

%%%%%%%%%%%%%%%%%%%%%%
%%	Main			%%
%%%%%%%%%%%%%%%%%%%%%%

jogar :- 	baralho_dar_as_pecas,
			mostra_tabuleiro(_),
			!,
			main_jogada_inicial,
			!,
			main_loop.


			
main_jogada_inicial :-
	mao_quem_tem_peca([7|7], JI),
	mao_remover_peca(JI, [7|7]),
	tabuleiro_set(_, [6|6], 7, 1, _),
	tabuleiro_set(_, [6|7], 7, 1, _).
			
main_loop :- 	repeat,
			mostra_tabuleiro(_).
			%mostrar a mao do jogador de qum for a vez
			%pedir qual a peca que joga-a
			%pedir as coordenadas
			%verificar se e valido
			%alterar tabuleiro e a vez do jogador
			%verificar se o jogo acabou
			

%%%%%%%%%%%%%%%%%%%%%%
%%	Baralho			%%
%%%%%%%%%%%%%%%%%%%%%%

:- dynamic baralho/1.
baralho([ [0|0], [0|1], [0|2], [0|3], [0|4], [0|5], [0|6], [0|7], [1|1], [1|2], 
			[1|3], [1|4], [1|5], [1|6], [1|7], [2|2], [2|3], [2|4], [2|5], [2|6],
			[2|7], [3|3], [3|4], [3|5], [3|6], [3|7], [4|4], [4|5], [4|6], [4|7],
			[5|5], [5|6], [5|7], [6|6], [6|7], [7|7] ]).

baralho_vazio :- 	baralho(B),
					mostra_linha(B),
					!,
					length(B,0).
					
baralho_get_peca_from(P) :- baralho(B),	
							length(B, L),
							LS is L + 1,
							random(0, LS, X),
							list_element_at(P, B, X).

baralho_dar_as_pecas :- 	
				repeat,
				baralho(B),
				baralho_get_peca_from(P),
				list_delete_one(P,B,C),
				retract(baralho(B)),
				assert(baralho(C)),
				jogador_escolhido(J),
				mao_acrecentar_peca([P],J),
				jogadro_trocar_vez(J),
				baralho(Y),
				length(Y, 0).		

%%%%%%%%%%%%%%%%%%%%%%
%% Manipular Listas	%%
%%%%%%%%%%%%%%%%%%%%%%
				
list_element_at(X,[X|_],1).
list_element_at(X,[_|L],K) :- list_element_at(X,L,K1), K is K1 + 1.
			

list_delete_one(X,L1,L2) :- 	append(A1,[X|A2],L1),
						append(A1,A2,L2).

matrix_setCell(1, Col, [H|T], Piece, [H1|T]) :-
	matrix_setCellCol(Col,H,Piece,H1).
matrix_setCell(N,Col,[H|T],Piece,[H|T1]):-
	Prev is N-1,
	matrix_setCell(Prev, Col, T, Piece, T1).

matrix_setCellCol(1, [_|T], Piece, [Piece|T]).
matrix_setCellCol(N, [H|T], Piece, [H|T1]):-
	Prev is N-1,
	matrix_setCellCol(Prev, T, Piece, T1).