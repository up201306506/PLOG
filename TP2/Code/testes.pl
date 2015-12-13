%%%%%%%%%%%%%%%%%%%%%%
%%	Testes			%%
%%%%%%%%%%%%%%%%%%%%%%
/*
 =============================================================================================
	Testar mostra_tabuleiro

	test1 :- get_PC(PC),get_PL(PL),get_Reg(RT),teste_1_solucao(ST),tabuleiro_contruir_solucoes(ST,RT,T),mostra_tabuleiro(PL,PC,T).

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

 =============================================================================================
	Testar construção de uma tabela
	
	test2 :- tabuleiro_contruir_solucoes([[1,1,1],[1,0,1],[0,1,0]],[[1,1,2],[3,3,2],[4,4,4]],Tabuleiro).                          

 =============================================================================================
	Testar geração de matrizes não definidas
	
	test3 :- matriz(4,5,M).
 
 =============================================================================================
	Testar dominios
	
	test4 :- matriz(2,2,M),solve_domains_matrix(M, 0,1).
	
 =============================================================================================
	Testar Labeling de uma matriz

	test5 :- solve_domains_matrix([[A,B],[C,D]],1,5), A #> B, B #> C, C #> D, append([[A,B],[C,D]], R), labeling([], R).

 =============================================================================================
	Testar a primeira pista
	
	test6 :- matriz(2,2,M),solve_domains_matrix(M,0,1),P = [[1,0],[0,0]],restrict_1stclue(M,P),append(M,R) ,labeling([],R).

 =============================================================================================
	Testar selecionar valores numa matriz
	test7(X,Y,Z) :- teste_7_matriz(M),matriz_selecionar_valor(1,1,M,X),matriz_selecionar_valor(2,2,M,Y),matriz_selecionar_valor(3,3,M,Z).
	teste_7_matriz( [[1,2,3],[4,5,6],[7,8,9]] ).
 =============================================================================================
	Testar a geração aleatória de soluções de puzzles
	
	test10(S):- generate_puzzle_solution(10, 10, S).
 =============================================================================================
  	Testar a geração pistas e regiões

	test11(P):- generate_puzzle_solution(10, 10, S), write(S), nl, generate_puzzle_clues(S, P), write(P),nl.
	test12(R):- generate_puzzle_solution(5,5,S), write(S), nl,generate_puzzle_regions(S,5,5,R), write(R), nl.
 =============================================================================================
	Testar a geração aleatória de puzzles e a sua resolução
*/	
	test20(Size):- generate_puzzle(Size, Size, TabelaRegioes, PistasLinhas, PistasColunas), main_args(Solucao, PistasLinhas, PistasColunas, TabelaRegioes).
 /*
 =============================================================================================
 
		Ver tempo de excução de p:

		statistics(runtime, [T0|_]),
			p,
				statistics(runtime, [T1|_]),
					T is T1 - T0,
						format('p/0 took ~3d sec.~n', [T]).

						
		statistics(runtime, [T0|_]),test20(5),statistics(runtime, [T1|_]),T is T1 - T0,format('p/0 took ~3d sec.~n', [T]).
		statistics(runtime, [T0|_]),test20(10),statistics(runtime, [T1|_]),T is T1 - T0,format('p/0 took ~3d sec.~n', [T]).
		statistics(runtime, [T0|_]),test20(12),statistics(runtime, [T1|_]),T is T1 - T0,format('p/0 took ~3d sec.~n', [T]).
		statistics(runtime, [T0|_]),main,statistics(runtime, [T1|_]),T is T1 - T0,format('p/0 took ~3d sec.~n', [T]).
 */	