%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Encontrar Jogadas	%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

cpu_todas_jogadas_climb(J, G, T) :-
	jogador(J),
	findall([P,CL1,CL2],cpu_uma_jogada_climb(J,P,CL1,CL2,T),G).

cpu_uma_jogada_climb(J,P,CL1,CL2,T) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		cpu_uma_jogada_climb_aux_linhas1(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_climb_aux_linhas2(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_climb_aux_colunas1(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_climb_aux_colunas2(L,M,P,CL1,CL2,T)
	).

cpu_uma_jogada_climb_aux_linhas1(L,M,[V1|V2],[C1|L1],[C2|L2],[Z|X]) :- 
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
	cpu_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2],[Z|X]).
cpu_uma_jogada_climb_aux_linhas2(L,M,[V2|V1],[C1|L1],[C2|L2],[Z|X]) :- 
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
	cpu_pode_jogar_peca_climb([V2|V1], [C1|L1], [C2|L2],[Z|X]).
cpu_uma_jogada_climb_aux_colunas1(L,M,[V1|V2],[C1|L1],[C2|L2],[Z|X]) :- 
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
	cpu_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2],[Z|X]).
cpu_uma_jogada_climb_aux_colunas2(L,M,[V2|V1],[C1|L1],[C2|L2],[Z|X]) :- 
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
	cpu_pode_jogar_peca_climb([V2|V1], [C1|L1], [C2|L2],[Z|X]).

	
	%%%%
	%testar:
	%%%%
		%Interromper dominup.
		%mao_reiniciar(jogador1),mao_acrescentar_peca([[7|7]], jogador1),mostra_mao_jogador(jogador1).
		%cpu_todas_jogadas_climb(jogador1, G), length(G,L).
		%Length: 2

	
	
	
	
	
cpu_todas_jogadas_expand(J, G, T) :-
	jogador(J),
	findall([P,CL1,CL2],cpu_uma_jogada_expand(J,P,CL1,CL2,T),G).
	

cpu_uma_jogada_expand(J,P, CL1, CL2,T) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		cpu_uma_jogada_expand_aux_linhas1(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_expand_aux_linhas2(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_expand_aux_colunas1(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_expand_aux_colunas2(L,M,P,CL1,CL2,T)
	).
	
cpu_uma_jogada_expand_aux_linhas1(L,M,[V1|V2],[C1|L1],[C2|L2],[Z|X]) :- 
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
	cpu_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2],[Z|X]).
cpu_uma_jogada_expand_aux_linhas2(L,M,[V2|V1],[C1|L1],[C2|L2],[Z|X]) :- 
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
	cpu_pode_jogar_peca_expand([V2|V1], [C1|L1], [C2|L2],[Z|X]).
cpu_uma_jogada_expand_aux_colunas1(L,M,[V1|V2],[C1|L1],[C2|L2],[Z|X]) :- 
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
	cpu_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2],[Z|X]).
cpu_uma_jogada_expand_aux_colunas2(L,M,[V2|V1],[C1|L1],[C2|L2],[Z|X]) :- 
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
	cpu_pode_jogar_peca_expand([V2|V1], [C1|L1], [C2|L2],[Z|X]).
	
	
	%%%%
	%testar:
	%%%%
		%Interromper dominup.
		%mao_reiniciar(jogador1),mao_acrescentar_peca([[1|7]], jogador1),mostra_mao_jogador(jogador1).
		%cpu_todas_jogadas_expand(jogador1, G), length(G,L).
		%Length: 18

		
		
		
cpu_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2],T) :-
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
			cpu_expand_aux(V1, [C1|L1], A, T);
			cpu_expand_aux(V2, [C2|L2], A, T)
		).
			
			cpu_expand_aux(V, [C|L], A, T) :-
				%à esquerda
					C2 is C-1,
					tabuleiro_get(T, [C2|L], V2, A2),
					A == A2,
					V == V2.
			cpu_expand_aux(V, [C|L], A, T) :-
				%à direita
					C2 is C+1,
					tabuleiro_get(T, [C2|L], V2, A2),
					A == A2,
					V == V2.
			cpu_expand_aux(V, [C|L], A, T) :-
				%abaixo
					L2 is L+1,
					tabuleiro_get(T, [C|L2], V2, A2),
					A == A2,
					V == V2.
			cpu_expand_aux(V, [C|L], A, T) :-
				%acima
					L2 is L-1,
					tabuleiro_get(T, [C|L2], V2, A2),
					A == A2,
					V == V2.



cpu_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2],T) :-
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
		
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ver a qualidade		%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


	
%qualidade_a_melhor_jogada(J, [P,CL1,CL2], T) :-
%	cpu_todas_jogadas_climb_com_resultado(J,G,T),
%	qualidade_aux([P,CL1,CL2], G, Q).
	

%qualidade_aux(_, [], _).
%qualidade_aux([P,CL1,CL2], G, Q) :-
	

	
tabuleiro_se_jogasse_peca([V1|V2], [C1|L1], [C2|L2], TFF) :-
	tabuleiro(TI),
	tabuleiro_get(T,[C1|L1], _, A1),
	tabuleiro_get(T,[C2|L2], _, A2),
	A1 == A2,
	A is A1+1,
	tabuleiro_set(TI, [C1|L1], V1, A, TF),
	tabuleiro_set(TF, [C2|L2], V2, A, TFF).
	
	

cpu_todas_jogadas_climb_com_resultado(J, G, T) :-
	jogador(J),
	findall([P,CL1,CL2,TFF],cpu_uma_jogada_climb(J,P,CL1,CL2,T,TFF),G).

cpu_uma_jogada_climb_com_resultado(J,P,CL1,CL2,T,TFF) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		cpu_uma_jogada_climb_aux_linhas1(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_climb_aux_linhas2(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_climb_aux_colunas1(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_climb_aux_colunas2(L,M,P,CL1,CL2,T)
	),
	tabuleiro_se_jogasse_peca(P,CL1,CL2,TFF).