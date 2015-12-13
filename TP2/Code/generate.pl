%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Puzzle Generation			%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
 =============
	generate_puzzle
		Inventa um puzzle aleatório com tamanho MxN (Linhas x Colunas), e devolve as suas restrições.
 =============
*/

generate_puzzle(Linhas, Colunas, TabelaRegioes, PistasLinhas, PistasColunas) :-
	generate_puzzle_solution(Linhas, Colunas, Solucao),
	generate_puzzle_clues(Solucao, PistasLinhas),
	transpose(Solucao, TS),
	generate_puzzle_clues(TS, PistasColunas),
	generate_puzzle_regions(Solucao, Linhas, Colunas, TabelaRegioes).
	



/*
 =============
	generate_puzzle_solution
		Inventa uma solução de um puzzle aleatório.
 =============
*/

generate_puzzle_solution(Linhas, Colunas, Solucao) :-
	matriz(Linhas, Colunas, Solucao),
	append(Solucao, Slinear),
	generate_puzzle_solution_aux(Slinear).
generate_puzzle_solution_aux([]).
generate_puzzle_solution_aux([Primeiro|Resto]):-
	random(0,2,Primeiro),
	generate_puzzle_solution_aux(Resto).

/*
 =============
	generate_puzzle_regions
		Gera uma tabla de regiões válida para a solução dada.
		Preenche linha a linha, pelo que nem todas as tabelas de regiões são possiveis.
 =============
*/

	generate_puzzle_regions(Solucao, Linhas, Colunas, TabelaRegioes) :-
		matriz(Linhas, Colunas, TabelaRegioes),
		generate_puzzle_regions_first(Solucao, TabelaRegioes).

	/* Geram a primeira linha*/
	generate_puzzle_regions_first([LinhaS|RestoS], [LinhaR|RestoR]) :-
		generate_puzzle_regions_first_aux(LinhaS, LinhaR, 0, 0, 1, LastR),
		generate_puzzle_regions_rest(RestoS, RestoR, LinhaS, LinhaR, LastR).
	
	generate_puzzle_regions_first_aux([],[], _, _, CR, LR) :- LR = CR.
	generate_puzzle_regions_first_aux([PrimeiroS|RestoS], [PrimeiroR|RestoR], _, 0, 1, LastR):-
		PrimeiroR is 1,
		CR is 2,
		generate_puzzle_regions_first_aux(RestoS, RestoR, PrimeiroS, PrimeiroR, CR, LastR).
	generate_puzzle_regions_first_aux([PrimeiroS|RestoS], [PrimeiroR|RestoR], PreviousS, PreviousR, CurrentR, LastR):-
		( 	PreviousS == PrimeiroS 
			-> 	(random(0,3,RNG),
				( 	RNG < 2 
					->	(PrimeiroR is  PreviousR, CR is CurrentR)
					;	(PrimeiroR is  CurrentR, CR is CurrentR+1)
				))
			; (PrimeiroR is  CurrentR, CR is CurrentR+1)
		),
		generate_puzzle_regions_first_aux(RestoS, RestoR, PrimeiroS, PrimeiroR, CR, LastR).
		
	/*Geram as restantes linhas*/
		generate_puzzle_regions_rest([], [], _, _, _).
		generate_puzzle_regions_rest([LinhaS|RestoS], [LinhaR|RestoR], AnteriorS, AnteriorR, CurrentR) :-
			generate_puzzle_regions_rest_aux(LinhaS, LinhaR, AnteriorS, AnteriorR, 0, 0, CurrentR, LastR),
			generate_puzzle_regions_rest(RestoS,RestoR,LinhaS,LinhaR,LastR).
			
		generate_puzzle_regions_rest_aux([], [], [], [], _, _, CR, LR) :- LR is CR.
		generate_puzzle_regions_rest_aux([PriS|RestoS],[PriR|RestoR],[APriS|ARestoS],[APriR|ARestoR], 0, 0, CR, LR) :-
			(	APriS == PriS 
				-> 	(random(0,3,RNG),
				( 	RNG < 2 
					->	(PriR is  APriR, NCR is CR)
					;	(PriR is  CR, NCR is CR+1)
				))
			; (PriR is  CR, NCR is CR+1)
			),
			generate_puzzle_regions_rest_aux(RestoS,RestoR,ARestoS,ARestoR, PriS, PriR, NCR, LR).
		generate_puzzle_regions_rest_aux([PriS|RestoS],[PriR|RestoR],[APriS|ARestoS],[APriR|ARestoR], PS, PR, CR, LR):-
			(	APriS == PriS 
				-> 	(random(0,3,RNG),
				( 	RNG < 2 
					->	(PriR is  APriR, NCR is CR)
					;	(PriR is  CR, NCR is CR+1)
				))
			; 	( 	PS == PriS
					-> 	(random(0,3,RNG2),
					( 	RNG2 < 2 
						->	(PriR is  PR, NCR is CR)
						;	(PriR is  CR, NCR is CR+1)
					))
				;	(PriR is  CR, NCR is CR+1)
				)
			),
			generate_puzzle_regions_rest_aux(RestoS,RestoR,ARestoS,ARestoR, PriS, PriR, NCR, LR).
		
/*
 =============
	generate_puzzle_clues
		Gera a lista de pistas para as linhas de uma dada mariz.
		Aplicar sobre a transposta para receber colunas.
 =============
*/
	generate_puzzle_clues(Solucao, Pistas) :-
		length(Solucao, Linhas),
		matriz(Linhas,2,Pistas),
		generate_puzzle_clues_aux(Solucao,Pistas).
	
	generate_puzzle_clues_aux([],[]).
	generate_puzzle_clues_aux([LinhaSolucao | RestoSolucao], [[P1st,P2nd] | RestoPistas] ) :-
		count(1, LinhaSolucao, #=, P1st),
		restrict_2ndclue_aux(P2nd, LinhaSolucao, 0, 0),
		generate_puzzle_clues_aux(RestoSolucao, RestoPistas).