%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%

/*
	tabuleiro_contruir_solucoes
		Controi um tabuleiro pronto a ser mostrado no cliente a partir de uma matriz de soluções
		e outra com os valores das regiões.
*/
tabuleiro_contruir_solucoes(Solucao, Regioes, Tabuleiro):-
	/*mesmo numero de linhas e colunas em ambas as tabelas input e na de resultado*/
	length(Solucao,NumeroLinhas),
	length(Regioes,NumeroLinhas),
	length(Tabuleiro,NumeroLinhas),
	transpose(Solucao, TSolucao),
	transpose(Regioes, TRegioes),
	transpose(Tabuleiro, TTabuleiro),
	length(TSolucao,NumeroColunas),
	length(TRegioes,NumeroColunas),
	length(TTabuleiro,NumeroColunas),
	/*Introduzir valores na tabela*/
	tabuleiro_contruir_solucoes_aux(Solucao,Regioes,Tabuleiro).

tabuleiro_contruir_solucoes_aux([],[],[]).
tabuleiro_contruir_solucoes_aux([Sl|Sr],[Rl|Rr],[Tl|Tr]):-
	tabuleiro_contruir_solucoes_aux_linha(Sl,Rl,Tl),
	tabuleiro_contruir_solucoes_aux(Sr,Rr,Tr).
	
tabuleiro_contruir_solucoes_aux_linha([],[],[]).
tabuleiro_contruir_solucoes_aux_linha([Sn|Sr],[Rn|Rr],[Tn|Tr]):-
	Tn = [Sn,Rn],
	tabuleiro_contruir_solucoes_aux_linha(Sr,Rr,Tr).