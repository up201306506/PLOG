
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
	solve_crossapix(Solucao,PistasColunas,PistasLinhas,TabelaRegioes).
	
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
solve_crossapix(Solucao, PistasLinhas, PistasColunas, TabelaRegioes)

/*

	Necessário:
		Definir 'Solucao' como uma matriz MxN onde M e N são os lengths das pistas
		Ver se os lengths das pistas equivalem aos da 'TabelaRegioes'
		Dominio dos valores: 0 ou 1
		Aplicar restrições:
			- Contagem dos grupos pintados numa linha igual à primeira pista
			- Contagem das casas pintadas numa linha igual à segunda pista
			- Repetir as duas restrições anteriores nas colunas
			- Elementos que pertençam à mesma região não podem ter valores diferentes
*/


.
