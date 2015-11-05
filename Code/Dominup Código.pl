﻿
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
	matrix_setCell(L, C, TI, [V|A], TF).
	
	
tabuleiro_get(T,[C|L], V, A) :- 
	tabuleiro(T),
	list_element_at(Y, T, L),
	list_element_at([V|A], Y, C).
	

tabuleiro_distancia_coordenadas([C1|L1], [C2|L2], D) :-
	DX is C1-C2,
	X is abs(DX),
	DY is L1-L2,
	Y is abs(DY),
	D is X+Y.
	
tabuleiro_jogar_peca([V1|V2], [C1|L1], [C2|L2]) :-
	tabuleiro_get(_,[C1|L1], _, A1),
	tabuleiro_get(_,[C2|L2], _, A2),
	A1 == A2,
	A is A1+1,
	tabuleiro_set(TI, [C1|L1], V1, A, TF),
	retract(tabuleiro(TI)),
	assert(tabuleiro(TF)),
	tabuleiro_set(TF, [C2|L2], V2, A, TFF),
	retract(tabuleiro(TF)),
	assert(tabuleiro(TFF)).

	
%%%!!!!!!!!!!!!!!!!!!!
%tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2])


tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]) :-
	tabuleiro(T),
	%buscar os valores no tabuleiro
		tabuleiro_get(T, [C1|L1], TV1, TA1),
		tabuleiro_get(T, [C2|L2], TV2, TA2),
		!,
	%as coordenadas devem estar a tocar-se
		tabuleiro_distancia_coordenadas([C1|L1], [C2|L2], D),
		D == 1,
	%as alturas devem ser iguais
		TA1 == TA2,
	%os valores no tabuleiro devem ser iguais ao da peça
		TV1 == V1,
		TV2 == V2.
	
%%%!!!!!!!!!!!!!!!!!!!
%tabuleiro_reiniciar

%%%%%%%%%%%%%%%%%%%%%%
%%	mao jogador		%%
%%%%%%%%%%%%%%%%%%%%%%

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
						
mao_escolher_peca(J, X, P) :-
	jogador(J),
	mao(J,M),
	list_element_at(P, M, X).

mao_vazia(J) :-
	jogador(J),
	mao(J, M),
	length(M,0).
						
								
mao_quem_tem_peca(P, J) :-
	mao(J,M),
	member(P,M).

%%%!!!!!!!!!!!!!!!!!!!
%mao_reiniciar

				

%%%%%%%%%%%%%%%%%%%%%%
%%	Main			%%
%%%%%%%%%%%%%%%%%%%%%%

dominup :- menu_principal.

jogar(Dificuldade) :- 	
	baralho_reiniciar,
	baralho_dar_as_pecas,
	mostra_tabuleiro(_),
	!,
	main_jogada_inicial,
	!,
	main_loop(Dificuldade).


			
main_jogada_inicial :-
	mao_quem_tem_peca([7|7], JI),
	mao_remover_peca(JI, [7|7]),
	tabuleiro_jogar_peca([7|7], [6|6], [6|7]).
			
main_loop(Dificuldade) :- 	repeat,
	cls,
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

baralho_reiniciar :-
			baralho(B),
			retract(baralho(B)),
			assert(
			baralho([ 
				[0|0], [0|1], [0|2], [0|3], [0|4], [0|5], [0|6], [0|7], [1|1], [1|2], 
				[1|3], [1|4], [1|5], [1|6], [1|7], [2|2], [2|3], [2|4], [2|5], [2|6],
				[2|7], [3|3], [3|4], [3|5], [3|6], [3|7], [4|4], [4|5], [4|6], [4|7],
				[5|5], [5|6], [5|7], [6|6], [6|7], [7|7] ])
			).
			
							
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
list_element_at(X,[_|L],K) :- 
	list_element_at(X,L,K1), 
	K is K1 + 1.
			

list_delete_one(X,L1,L2) :- 	
	append(A1,[X|A2],L1),
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
	

	
%%%%%%%%%%%%%%%%%%%%%%
%% Outras funções	%%
%%%%%%%%%%%%%%%%%%%%%%

readInt(Texto, I, Min, Max) :-
	repeat,
	write(Texto), nl,
	read(I),
	I >= Min, I =< Max.
