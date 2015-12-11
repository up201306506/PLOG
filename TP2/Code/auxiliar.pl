%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%

/*
 =============
	tabuleiro_contruir_solucoes
		Controi um tabuleiro pronto a ser mostrado no cliente a partir de uma matriz de soluções
		e outra com os valores das regiões.
 =============
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

%%%%%%%%%%%%%%%%%%%%%%
%%	Matriz			%%
%%%%%%%%%%%%%%%%%%%%%%

/*
 =============
	matriz
		Gera uma matriz M*N (m linhas, n colunas)
 =============
*/	
matriz(M,N, Resultado) :-
        length(Resultado, M),
        maplist(matriz_aux(N), Resultado).

matriz_aux(L, Ls) :- length(Ls, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Restrições do Main		%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 =============
	solve_domains_matrix
		aplica dominio em todas as linhas de uma matriz. Recebe a tabela de soluções, 
		o minimo e maximo do dominio
 =============
*/
solve_domains_matrix([], _, _).
solve_domains_matrix([Linha|Resto], Minimo, Maximo) :-
	domain(Linha,Minimo,Maximo),
	solve_domains_matrix(Resto, Minimo, Maximo).

/*
 =============
	restrict_1stclue
		Aplica a restrição da primeira pista na tabela de soluções: 
			A contagem das quadriculas pintadas igual à pista
		Recebe a tabela de soluções, e as pista para as linhas dessa tabela. A transposta pode usar as pistas de colunas na mesma função.
 =============
*/
restrict_1stclue([],[]).
restrict_1stclue([SolucaoLinha|SolucaoResto], [[Pista|_]|PistasResto]) :-
	count(1, SolucaoLinha, #=, Pista),
	restrict_1stclue(SolucaoResto, PistasResto).

/*
 =============
	restrict_2ndclue
		Verifica que a solução encontrada obedece às regras da segunda pista: 
			A contagem de blocos de quadriculas pintadas (separados pelo menos por uma quadricula branca) igual à pista
		Recebe a tabela de soluções, e as pista para as linhas dessa tabela. A transposta pode usar as pistas de colunas na mesma função.
		!!! Não funciona antes de fazer labeling, mas falhar implica backtrack para o labeling !!!
 =============
*/
restrict_2ndclue([],[]).
restrict_2ndclue([SolucaoLinha|SolucaoResto], [[_,Pista]|PistasResto]) :-
	restrict_2ndclue_aux(Pista, SolucaoLinha, 0, 0),
	restrict_2ndclue(SolucaoResto, PistasResto).

restrict_2ndclue_aux(Expected, [], _, Count) :-
	Expected = Count.
restrict_2ndclue_aux(Expected, [Elemento|Resto], 0, Count) :-
	CountPlus is Count+1,
	(
		Elemento = 1 
		->	restrict_2ndclue_aux(Expected, Resto, 1, CountPlus)
		;	restrict_2ndclue_aux(Expected, Resto, 0, Count)
	).
restrict_2ndclue_aux(Expected, [Elemento|Resto], 1, Count) :-
	(
		Elemento = 0 
		->	restrict_2ndclue_aux(Expected, Resto, 0, Count)
		;	restrict_2ndclue_aux(Expected, Resto, 1, Count)
	).
	
/*
 =============
	restrict_regions
		Aplica as regiões (quadriculas que devem ter o mesmo valor) às soluções.
		Recebe a tabela de soluções e a tabela de Regiões.
 =============
*/
restrict_regions([],[]).
restrict_regions([PrimeiroSolucao|RestoSolucao], [PrimeiroRegions|RestoRegions]) :-
	restrict_regions_aux(PrimeiroSolucao, PrimeiroRegions, RestoSolucao, RestoRegions),
	restrict_regions(RestoSolucao, RestoRegions).

restrict_regions_aux(_, _, [], []).
restrict_regions_aux(S, R, [PrimeiroSolucao|RestoSolucao], [PrimeiroRegions|RestoRegions]):-
	(R = PrimeiroRegions -> PrimeiroSolucao #= S; true),
	restrict_regions_aux(S,R, RestoSolucao, RestoRegions).