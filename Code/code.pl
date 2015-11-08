
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(displays).
:- use_module(auxiliar).
:- use_module(cpu).

%:-now(time), setrand(time).



%%%%%%%%%%%%%%%%%%%%%%
%%	Init			%%
%%%%%%%%%%%%%%%%%%%%%%


jogador(jogador1).
jogador(jogador2).

:- dynamic tabuleiro/1.
tabuleiro([ [[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0]] ]).
			

:- dynamic mao/2.			
mao(jogador1, []).
mao(jogador2, []).

:- dynamic jogador_escolhido/1.
jogador_escolhido(jogador1).
	
:- dynamic estado/1.
estado(T, J) :- tabuleiro(T), jogador_escolhido(J).


%%%%%%%%%%%%%%%%%%%%%%
%%	Jogador			%%
%%%%%%%%%%%%%%%%%%%%%%

jogador_trocar_vez(J) :- jogador(X),
						X \= J,
						retract(jogador_escolhido(J)),
						assert(jogador_escolhido(X)).

jogador_pode_jogar(J) :-
	(
	jogador_jogadas_disponiveis_climb(J);
	jogador_jogadas_disponiveis_expand(J)
	).
						

jogador_jogadas_disponiveis_climb(J) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		jogador_jogadas_disponiveis_climb_aux_linhas(L,M);
		jogador_jogadas_disponiveis_climb_aux_colunas(L,M)
	).

jogador_jogadas_disponiveis_climb_aux_linhas(L, M) :- 
	tabuleiro([Z|X]),
	length(Z, NC),
	length([Z|X], NL),
	NLL is NL-1,
	!,
	incrementador(1, N, L),
	list_element_at([V1|V2], M, N),
	incrementador(1, L1, NLL),
	L2 is L1+1,
	incrementador(1, C1, NC),
	C2 is C1,
	(tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]);
	tabuleiro_pode_jogar_peca_climb([V2|V1], [C1|L1], [C2|L2])).

jogador_jogadas_disponiveis_climb_aux_colunas(L, M) :- 
	tabuleiro([Z|X]),
	length(Z, NC),
	length([Z|X], NL),
	NCC is NC-1,
	!,
	incrementador(1, N, L),
	list_element_at([V1|V2], M, N),
	incrementador(1, L1, NL),
	L2 is L1,
	incrementador(1, C1, NCC),
	C2 is C1+1,
	(tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]);
	tabuleiro_pode_jogar_peca_climb([V2|V1], [C1|L1], [C2|L2])).



jogador_jogadas_disponiveis_expand(J) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		jogador_jogadas_disponiveis_expand_aux_linhas(L,M);
		jogador_jogadas_disponiveis_expand_aux_colunas(L,M)
	).
	
jogador_jogadas_disponiveis_expand_aux_linhas(L, M) :- 
	tabuleiro([Z|X]),
	length(Z, NC),
	length([Z|X], NL),
	NLL is NL-1,
	!,
	incrementador(1, N, L),
	list_element_at([V1|V2], M, N),
	incrementador(1, L1, NLL),
	L2 is L1+1,
	incrementador(1, C1, NC),
	C2 is C1,
	(tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2]);
	tabuleiro_pode_jogar_peca_expand([V2|V1], [C1|L1], [C2|L2])).
	
jogador_jogadas_disponiveis_expand_aux_colunas(L, M) :- 
	tabuleiro([Z|X]),
	length(Z, NC),
	length([Z|X], NL),
	NCC is NC-1,
	!,
	incrementador(1, N, L),
	list_element_at([V1|V2], M, N),
	incrementador(1, L1, NL),
	L2 is L1,
	incrementador(1, C1, NCC),
	C2 is C1+1,
	(tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2]);
	tabuleiro_pode_jogar_peca_expand([V2|V1], [C1|L1], [C2|L2])).
						

%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%

tabuleiro_set(TI, [C|L], V, A, TF) :-
	tabuleiro(TI),
	matrix_setCell(L, C, TI, [V|A], TF).
	
	
tabuleiro_get(T,[C|L], V, A) :- 
	tabuleiro(T),
	C > 0,
	L > 0,
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
	

tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2]) :-
	tabuleiro(T),
	%buscar os valores no tabuleiro
		tabuleiro_get(T, [C1|L1], _, TA1),
		tabuleiro_get(T, [C2|L2], _, TA2),
		!,
	%as coordenadas devem estar a tocar-se
		tabuleiro_distancia_coordenadas([C1|L1], [C2|L2], D),
		D == 1,
	%as alturas devem ser iguais
		TA1 == TA2,
	%uma das peças circundantes devem ter o mesmo valor		
		A is TA1 + 1,
		(
			expand_aux(V1, [C1|L1], A);
			expand_aux(V2, [C2|L2], A)
		).
			expand_aux(V, [C|L], A) :-
				%à esquerda
					C2 is C-1,
					tabuleiro_get(_, [C2|L], V2, A2),
					A == A2,
					V == V2.
			expand_aux(V, [C|L], A) :-
				%à direita
					C2 is C+1,
					tabuleiro_get(_, [C2|L], V2, A2),
					A == A2,
					V == V2.
			expand_aux(V, [C|L], A) :-
				%abaixo
					L2 is L+1,
					tabuleiro_get(_, [C|L2], V2, A2),
					A == A2,
					V == V2.
			expand_aux(V, [C|L], A) :-
				%acima
					L2 is L-1,
					tabuleiro_get(_, [C|L2], V2, A2),
					A == A2,
					V == V2.


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
	%a altura não pode ser 0
		TA1 \= 0,
	%os valores no tabuleiro devem ser iguais ao da peça
		TV1 == V1,
		TV2 == V2.
	
tabuleiro_reiniciar :-
		tabuleiro(T),
		retract(tabuleiro(T)),
		assert(
			tabuleiro([ 
						[[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0]] 
						])
		).

%%%%%%%%%%%%%%%%%%%%%%
%%	mao jogador		%%
%%%%%%%%%%%%%%%%%%%%%%

mao_acrescentar_peca(P, J) :- 	mao(J, V),
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

mao_reiniciar(J) :-
		jogador(J),
		mao(J, M),
		retract(mao(J, M)),
		assert(mao(J, [])).
				

%%%%%%%%%%%%%%%%%%%%%%
%%	Main			%%
%%%%%%%%%%%%%%%%%%%%%%

dominup :- menu_principal.

jogar(Dificuldade) :- 
	Dificuldade >= 0, Dificuldade < 3,
	baralho_reiniciar,
	mao_reiniciar(jogador1),
	mao_reiniciar(jogador2),
	tabuleiro_reiniciar,
	baralho_dar_as_pecas,
	!,
	main_jogada_inicial,
	!,
	repeat,
	main_loop(Dificuldade).
			
main_jogada_inicial :-
	mao_quem_tem_peca([7|7], JI),
	mao_remover_peca(JI, [7|7]),
	tabuleiro_jogar_peca([7|7], [3|3], [3|4]),
	jogador_escolhido(J),
	(JI = J -> jogador_trocar_vez(J); true).
	
main_loop(0) :- 
		jogador_escolhido(J),
		main_jogador_humano(J),
		!,
		(mao_vazia(J) ->  main_victoria(J); main_loop(0)).

main_loop(1) :-
	jogador_escolhido(J),
	(J = 'jogador1' -> main_jogador_humano(J); main_jogador_computador_facil(J)),
	!,
	(mao_vazia(J) ->  main_victoria(J); main_loop(1)).
	
main_loop(2) :-
	repeat,
	cls,
	mostra_tabuleiro(_).
	
main_victoria(J) :-
	cls,
	nl,nl,nl,
	write('                VICTORIA DO JOGADOR '), write(J),nl,nl,
	write('tabuleiro final:'), nl,
	mostra_tabuleiro(_).
	
main_jogador_humano(J) :-
	%Escolher a Peca
		cls,
		mostra_tabuleiro(_),
		mostra_mao_jogador(J),
			mao(J,M), length(M, ML),
		readInt('Qual a peca que quer jogar?', Input, 1, ML),
	%Escolher a Posição da cabeça
		!,
		cls,
		mostra_tabuleiro(_),
		mao_escolher_peca(J, Input, [V1|V2]),
		write('Peca escolhida: ['), write(V1), write('|'), write(V2), write(']. Valor da cabeca: '), write(V1) , nl,
			tabuleiro([TH|TR]), length([TH|TR], NL), length(TH,NC),
		write('Quais as coordenadas da cabeca da peca que quer jogar?'), nl,
		readInt('Coluna?', C1, 1, NC),
		readInt('Linha?', L1, 1, NL),
	%Escolher a Posição da cauda
		write('Peca escolhida: ['), write(V1), write('|'), write(V2), write(']. Valor da cauda: '), write(V2) , nl,
			tabuleiro([TH|TR]), length([TH|TR], NL), length(TH,NC),
		write('Quais as coordenadas da cauda da peca que quer jogar?'), nl,
		readInt('Coluna?', C2, 1, NC),
		readInt('Linha?', L2, 1, NL),
	%Verificar se e valido
		!,
		cls,
		write('A verificar se a jogada e valida...'), nl, sleep(1),
		(
		tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]);
		tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2]) 
		),
		write('A jogada e valida e foi efectuada'), nl, sleep(1),
	%alterar tabuleiro, tirar a peça ao jogador
		tabuleiro_jogar_peca([V1|V2], [C1|L1], [C2|L2]),
		mao_remover_peca(J, [V1|V2]),
	%Ver se o outro jogador pode jogar, trocar a vez se sim
		!,
		jogador(Joutro), Joutro \= J,
		(jogador_pode_jogar(Joutro) -> jogador_trocar_vez(J); true),	
	%expandir o tabuleiro se necessário
		tabuleiro_dimensiona, tabuleiro_dimensiona.
		
main_jogador_computador_facil(J) :-
		cls,
		mostra_tabuleiro(_),
		mostra_mao_jogador(J),
		mao(J,M), length(M, ML),
	%Escolher a Peca
		write('O computador esta escolher uma peca para jogar...'), nl, sleep(3),
		
	%Ver se o outro jogador pode jogar, trocar a vez se sim
		!,
		jogador(Joutro), Joutro \= J,
		(jogador_pode_jogar(Joutro) -> jogador_trocar_vez(J); true),	
	%expandir o tabuleiro se necessário
		tabuleiro_dimensiona, tabuleiro_dimensiona.
		
			

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
				mao_acrescentar_peca([P],J),
				jogador_trocar_vez(J),
				baralho(Y),
				length(Y, 0).