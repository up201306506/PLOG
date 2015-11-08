%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Encontrar Jogadas	%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

cpu_todas_jogadas_climb(J, G) :-
	jogador(J),
	findall([P,CL1,CL2],cpu_uma_jogada_climb(J,P,CL1,CL2),G).

cpu_uma_jogada_climb(J,P,CL1,CL2) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		cpu_uma_jogada_climb_aux_linhas(L,M,P,CL1,CL2);
		cpu_uma_jogada_climb_aux_colunas(L,M,P,CL1,CL2)
	).

cpu_uma_jogada_climb_aux_linhas(L,M,[V1|V2],[C1|L1],[C2|L2]) :- 
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

cpu_uma_jogada_climb_aux_colunas(L,M,[V1|V2],[C1|L1],[C2|L2]) :- 
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

	
	%%%%
	%testar:
	%%%%
		%Interromper dominup.
		%mao_reiniciar(jogador1),mao_acrescentar_peca([[7|7]], jogador1),mostra_mao_jogador(jogador1).
		%cpu_todas_jogadas_climb(jogador1, G), length(G,L).
		%Length: 2

	
	
	
	
	
cpu_todas_jogadas_expand(J, G) :-
	jogador(J),
	findall([P,CL1,CL2],cpu_uma_jogada_expand(J,P,CL1,CL2),G).
	

cpu_uma_jogada_expand(J,P, CL1, CL2) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		cpu_uma_jogada_expand_aux_linhas(L,M,P,CL1,CL2);
		cpu_uma_jogada_expand_aux_colunas(L,M,P,CL1,CL2)
	).
	
cpu_uma_jogada_expand_aux_linhas(L,M,[V1|V2],[C1|L1],[C2|L2]) :- 
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

cpu_uma_jogada_expand_aux_colunas(L,M,[V1|V2],[C1|L1],[C2|L2]) :- 
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
	
	
	%%%%
	%testar:
	%%%%
		%Interromper dominup.
		%mao_reiniciar(jogador1),mao_acrescentar_peca([[1|7]], jogador1),mostra_mao_jogador(jogador1).
		%cpu_todas_jogadas_expand(jogador1, G), length(G,L).
		%Length: 18