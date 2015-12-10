
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(clpfd)).
:- use_module(display).
:- use_module(auxiliar).
:- use_module(testes).

:- use_module(puzzle).

%%%%%%%%%%%%%%%%%%%%%%
%%	Program			%%
%%%%%%%%%%%%%%%%%%%%%%

/*
 =============
	main
		Procedimento principal. Usa valores de pistas e regiões definidas no ficheiro pl. 

	main_args
		Igual a main mas aceita inputs para as pistas e regiões e devolve uma solução.
 =============
*/

%main_args(Solucao, PistasLinhas, PistasColunas, TabelaRegioes).

main :- 
	/* buscar valores a ficheiro*/
	get_PC(PistasColunas),
	get_PL(PistasLinhas),
	get_Reg(TabelaRegioes),
	length(PistasColunas,NumeroColunas),
	length(PistasLinhas,NumeroLinhas),
	
	/*Ver se estão correctos+/
	transpose(PistasLinhas, TPistasLinhas),
	transpose(PistasColunas, TPistasColunas),
	transpose(TabelaRegioes, TTabelaRegioes),
	length(TPistasColunas,NumeroLinhas),
	length(TPistasLinhas,NumeroColunas),
	length(TabelaRegioes,NumeroLinhas),
	length(TTabelaRegioes,NumeroColunas),
	
	/*Resolver*/
	matriz(NumeroLinhas,NumeroColunas,Solucao),
	solve_crossapix(Solucao,PistasLinhas,PistasColunas,TabelaRegioes).
	
/*
 =============
	solve_crossapix
		Resolve um puzzle Cross-a-Pix dado
		
		Solucao - Matriz com as regiões pintadas. 0: Branco 1: Pintado
		PistasLinhas - Lista de pistas dadas nas linhas, de cima para baixo
		PistasColunas - Lista de pistas dadas nas colunas, da esquerda para direita
		TabelaRegioes - Matriz com os valores das regiões no tabuleiro vazio
 =============
*/
solve_crossapix(Solucao, PistasLinhas, PistasColunas, TabelaRegioes) :-
	/*Definir Dominios*/
	solve_domains_matrix(Solucao, 0,1),	
	
	/*Restringir a primeira pista*/
	restrict_1stclue(Solucao, PistasLinhas),
	transpose(Solucao, TSolucao),
	restrict_1stclue(TSolucao, PistasColunas),
	
	/*Pôr as tabelas em linha*/
	append(Solucao, Label),
	append(TabelaRegioes, Regions),
	
	/*Restringir por regiões*/
	restrict_regions(Label, Regions),
	
	/*labeling*/
	labeling([],Label),
	
	/*Restringir a segunda pista, voltar ao labeling se a solução não se encaixar*/
	restrict_2ndclue(Solucao, PistasLinhas),
	restrict_2ndclue(TSolucao, PistasColunas),
	
	/*Display da solução*/
	tabuleiro_contruir_solucoes(Solucao,TabelaRegioes,TabuleiroFinal),
	mostra_tabuleiro(PistasLinhas,PistasColunas,TabuleiroFinal).
	
/*=============*/

solve_domains_matrix([], _, _).
solve_domains_matrix([Linha|Resto], Minimo, Maximo) :-
	domain(Linha,Minimo,Maximo),
	solve_domains_matrix(Resto, Minimo, Maximo).


restrict_1stclue([],[]).
restrict_1stclue([SolucaoLinha|SolucaoResto], [[Pista|_]|PistasResto]) :-
	count(1, SolucaoLinha, #=, Pista),
	restrict_1stclue(SolucaoResto, PistasResto).

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
	

restrict_regions([],[]).
restrict_regions([PrimeiroSolucao|RestoSolucao], [PrimeiroRegions|RestoRegions]) :-
	restrict_regions_aux(PrimeiroSolucao, PrimeiroRegions, RestoSolucao, RestoRegions),
	restrict_regions(RestoSolucao, RestoRegions).

restrict_regions_aux(_, _, [], []).
restrict_regions_aux(S, R, [PrimeiroSolucao|RestoSolucao], [PrimeiroRegions|RestoRegions]):-
	(R = PrimeiroRegions -> PrimeiroSolucao #= S; true),
	restrict_regions_aux(S,R, RestoSolucao, RestoRegions).

	
