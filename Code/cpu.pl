%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Encontrar Jogadas Aleatorias	%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%	
	%	Funções Importantes:
	%		
	%	:-cpu_uma_ao_calhas(J,G,T).
	%		Recebe um jogador J e um tabuleiro T, e primeiro devolve em G a lista de, ou:
	%			*	cpu_todas_jogadas_climb(J,G,T). - Todas as jogadas do typo Climb que pode efectuar
	%		e se não existirem;
	%			*	cpu_todas_jogadas_expand(J,G,T). - Todas as jogadas do typo Expand que pode efectuar
	%		a funções encontram jogadas possiveis a percorrer o tabuleiro com as peças posicionadas na vertical 
	%		ou na horizontal, indicado por aux_linhas# ou aux_colunas#. O valor 1 ou 2 do # indica se a peça está
	%		a ser testada num sentido ou no outro.
	%		Nas posições, as peças verificam se são jogada válida por uso a
	%			*	cpu_pode_jogar_peca_expand(P, CL1, CL2, T)
	%			*	cpu_pode_jogar_peca_climb(P, CL1, CL2, T)
	%		Onde P é a peça a encaixar, CL1 a coordenada da cabeça da peça e CL2 as coordenadas da cauda da peça. 
	%		Estas três variaveis tem todas dois valores.
	%
	%
	%
	%
	%

cpu_uma_ao_calhas(J,G,T) :-
	cpu_todas_jogadas_climb(J,G,T), 
	G \= [].
cpu_uma_ao_calhas(J,G,T) :-
	cpu_todas_jogadas_expand(J,G,T), 
	G \= [].


	
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
	%	
	%	Funções Importantes:
	%		
	%	:-qualidade_a_melhor_jogada(J, P,CL1,CL2).
	%		Recebe um jogador J e, assumindo o tabuleiro de jogo T, procura qual é a melhor de todas jogadas,
	%		ou seja, qual a peça P e onde - CL1, CL2 - a deve jogar para efectuar a melhor jogada.
	%		Utiliza uma redefinição das funções usadas por cpu_uma_ao_calhas:
	%			*	cpu_uma_jogada_climb_com_resultado
	%			*	cpu_uma_jogada_expand_com_resultado
	%		Com comportamento semelhante às versões acima, mas para alem das jogadas, tambem geram o tabuleiro
	%		resultante dessas jogadas:
	%			* 	tabuleiro_se_jogasse_peca( P,CL1,CL2, TFF) - TFF é igual ao tabuleiro do jogo se a peça P
	%															fosse colocada em CL1 CL2.
	%		O que se faz com esses tabuleiros resultantes vai ser acontecer na função
	%			* 	qualidade_aux(N, Q,  Pprev,CL1prev,CL2prev,  P,CL1,CL2, G, J) - A partir da lista de jogadas G
	%									que o jogador J pode efectuar, e por uso de recursividade encontra a jogada
	%									P,CL1,CL2 que obtem a qualidade de jogada Q mais baixa possivel, ou seja
	%									aquela cujo tabuleiro oferece o menor numero de jogadas ao jogador oponente.
	%																			  Se uma jogada que ofereça 0 jogadas 
	%									de resposta é encontrada, essa é retornada de imediato.
	%

qualidade_a_melhor_jogada(J, P,CL1,CL2) :-
	tabuleiro(T),
	jogador(J),
	cpu_todas_jogadas_climb_com_resultado(J,G,T),
	G \= [],
	%write('CPU Climb'), nl,
	jogador(Joutro), Joutro \= J,
	qualidade_aux(1, 10000, 0,0,0, P,CL1,CL2, G, Joutro).
qualidade_a_melhor_jogada(J, P,CL1,CL2) :-
	tabuleiro(T),
	jogador(J),
	cpu_todas_jogadas_expand_com_resultado(J,G,T),
	G \= [],
	%write('CPU Expand'), nl,
	jogador(Joutro), Joutro \= J,
	qualidade_aux(1, 30000, 0,0,0, P,CL1,CL2, G, Joutro).

	
qualidade_aux(_, 0,  Pprev,CL1prev,CL2prev, P,CL1,CL2, _, _) :-
	P = Pprev,
	CL1 = CL1prev,
	CL2 = CL2prev.

qualidade_aux(N, _,  Pprev,CL1prev,CL2prev, P,CL1,CL2, G, _) :-
	length(G, L),
	N > L,
	P = Pprev,
	CL1 = CL1prev,
	CL2 = CL2prev.
	
qualidade_aux(N, Q,  Pprev,CL1prev,CL2prev,  P,CL1,CL2, G, J) :-
	%write(Pprev),write(','),write(CL1prev),write(','),write(CL2prev),nl,
	length(G, L),
	N =< L,
	list_element_at(Valores,G,N),
		list_element_at(Pcurr,Valores,1),
		list_element_at(CL1curr,Valores,2),
		list_element_at(CL2curr,Valores,3),
		list_element_at(TFF,Valores,4),
	cpu_todas_jogadas_climb(J,JGC,TFF), length(JGC, L1C),
	cpu_todas_jogadas_expand(J,JGE,TFF), length(JGE, L2E),
	QRes is L1C+L2E,
	Next is N+1,	
	(
	QRes < Q 
		-> 	qualidade_aux(Next, QRes,  Pcurr,CL1curr,CL2curr, Pnext,CL1next,CL2next,    G, J)
		;	qualidade_aux(Next, Q,   Pprev,CL1prev,CL2prev, Pnext,CL1next,CL2next,     G, J)
	),
	P = Pnext,
	CL1 = CL1next,
	CL2 = CL2next.
	
	
	



cpu_todas_jogadas_climb_com_resultado(J, G, T) :-
	findall([P,CL1,CL2,TFF],cpu_uma_jogada_climb_com_resultado(J,P,CL1,CL2,T,TFF),G).
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
	
cpu_todas_jogadas_expand_com_resultado(J, G, T) :-
	jogador(J),
	findall([P,CL1,CL2,TFF],cpu_uma_jogada_expand_com_resultado(J,P,CL1,CL2,T,TFF),G).
cpu_uma_jogada_expand_com_resultado(J,P,CL1,CL2,T,TFF) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		cpu_uma_jogada_expand_aux_linhas1(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_expand_aux_linhas2(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_expand_aux_colunas1(L,M,P,CL1,CL2,T);
		cpu_uma_jogada_expand_aux_colunas2(L,M,P,CL1,CL2,T)
	),
	tabuleiro_se_jogasse_peca(P,CL1,CL2,TFF).
	

	
tabuleiro_se_jogasse_peca([V1|V2], [C1|L1], [C2|L2], TFF) :-
	tabuleiro(TI),
	tabuleiro_get(TI,[C1|L1], _, A1),
	tabuleiro_get(TI,[C2|L2], _, A2),
	A1 == A2,
	A is A1+1,
	tabuleiro_set(TI, [C1|L1], V1, A, TF),
	tabuleiro_set(TF, [C2|L2], V2, A, TFF).	 
 
	 
	%%%%
	%testar:
	%%%%		
			%teste :-
			%	tabuleiro_jogar_peca([7|7],[7|5],[7|6]), tabuleiro(T), mostra_tabuleiro(T),
			%	mao_reiniciar(jogador1),mao_acrescentar_peca([[6|6]], jogador1), 
			%	mao_reiniciar(jogador2),mao_acrescentar_peca([[7|7]], jogador2),mao_acrescentar_peca([[6|7]], jogador2),
			%	qualidade_a_melhor_jogada(jogador2, P,CL1,CL2),
			%	write(P),write(','),write(CL1),write(','),write(CL2),nl.