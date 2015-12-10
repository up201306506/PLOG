%%%%%%%%%%%%%%%%%%%%%%
%%	Testes			%%
%%%%%%%%%%%%%%%%%%%%%%
/*
 =============================================================================================
	Testar mostra_tabuleiro
	| ? - get_PC(PC),get_PL(PL),get_Reg(RT),teste_1_solucao(ST),tabuleiro_contruir_solucoes(ST,RT,T),mostra_tabuleiro(PL,PC,T).
*/
	teste_1_solucao([
		[0,0,1,0,0,1,0,0,1,0],
		[0,0,1,1,0,0,0,1,0,0],
		[0,0,0,1,1,1,0,1,0,1],
		[0,0,0,0,1,0,1,1,1,1],
		[0,0,0,0,1,1,1,1,0,0],
		[0,0,0,1,0,0,1,0,1,1],
		[1,1,1,0,1,0,1,1,1,0],
		[1,0,0,0,0,0,1,0,0,0],
		[0,1,1,1,0,0,1,1,0,0],
		[0,0,0,1,0,0,0,1,1,1]
	]).
/*
 =============================================================================================
	Testar construção de uma tabela
	
	| ? - tabuleiro_contruir_solucoes([[1,1,1],[1,0,1],[0,1,0]],[[1,1,2],[3,3,2],[4,4,4]],Tabuleiro).                          

 =============================================================================================
	Testar geração de matrizes não definidas
	
	| ? - matriz(4,5,M).
 
 =============================================================================================
	Testar dominios
	
	| ? - matriz(2,2,M),solve_domains_matrix(M, 0,1).
	
 =============================================================================================
	Testar Labeling de uma matriz

	| ? - solve_domains_matrix([[A,B],[C,D]],1,5), A #> B, B #> C, C #> D, append([[A,B],[C,D]], R), labeling([], R).

 =============================================================================================
	Testar a primeira pista
	
	| ? - matriz(2,2,M),solve_domains_matrix(M,0,1),P = [[1,0],[0,0]],restrict_1stclue(M,P),append(M,R) ,labeling([],R).

 =============================================================================================
	
 */	