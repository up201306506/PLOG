%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Puzzle Generation			%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
 =============
	generate_puzzle
		Inventa um puzzle aleat�rio com tamanho MxN (Linhas x Colunas), e devolve as suas restri��es.
 =============
*/

generate_puzzle(Linhas, Colunas, TabelaRegioes, PistasLinhas, PistasColunas) :-
	generate_puzzle_solution(Linhas, Colunas, Solucao).
	



/*
 =============
	generate_puzzle_solution
		Inventa uma solu��o de um puzzle aleat�rio.
 =============
*/

generate_puzzle_solution(Linhas, Colunas, Solucao) :-
	matriz(Linhas, Colunas, Solucao),
	write(Solucao),
	append(Solucao, Slinear),
	generate_puzzle_solution_aux(Slinear).
generate_puzzle_solution_aux([]).
generate_puzzle_solution_aux([Primeiro|Resto]):-
	random(0,2,Primeiro),
	generate_puzzle_solution_aux(Resto).

/*
 =============
	generate_puzzle_regions
		
 =============
*/

/*
 =============
	generate_puzzle_1stclue
		
 =============
*/

/*
 =============
	generate_puzzle_2nclue
		
 =============
*/