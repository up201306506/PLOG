
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(clpfd)).
:- use_module(display).
:- use_module(auxiliar).
:- use_module(testes).
:- use_module(generate).

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



main :- 
	/* buscar valores a ficheiro*/
	get_PC(PistasColunas),
	get_PL(PistasLinhas),
	get_Reg(TabelaRegioes),
	length(PistasColunas,NumeroColunas),
	length(PistasLinhas,NumeroLinhas),
	
	/*Ver se estão correctos*/
	transpose(TabelaRegioes, TTabelaRegioes),
	length(TabelaRegioes,NumeroLinhas),
	length(TTabelaRegioes,NumeroColunas),
	
	/*Resolver*/
	matriz(NumeroLinhas,NumeroColunas,Solucao),
	solve_crossapix(Solucao,PistasLinhas,PistasColunas,TabelaRegioes).
	
main_args(Solucao, PistasLinhas, PistasColunas, TabelaRegioes) :-
	/* Tamanhos dos argumentos*/
	length(PistasColunas,NumeroColunas),
	length(PistasLinhas,NumeroLinhas),
	
	/*Ver se estão correctos*/
	transpose(TabelaRegioes, TTabelaRegioes),
	length(TabelaRegioes,NumeroLinhas),
	length(TTabelaRegioes,NumeroColunas),
	
	/*Resolver*/
	matriz(NumeroLinhas,NumeroColunas,Solucao),
	solve_crossapix(Solucao,PistasLinhas,PistasColunas,TabelaRegioes).
	
/*
 =============
	solve_crossapix
		Resolve e mostra um puzzle Cross-a-Pix dado
		
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



	
