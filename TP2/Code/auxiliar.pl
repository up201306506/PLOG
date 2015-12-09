%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%
/*

*/
tabuleiro_buscar_valor(L, C, T, [P,R]) :-
			nth1(L, T, LI),
			nth1(C,LI,[P,R]).
			
%%%%%%%%%%%%%%%%%%%%%%
%%	Init			%%
%%%%%%%%%%%%%%%%%%%%%%
:- dynamic tabuleiro/1.
:- dynamic pistas_colunas/1.
:- dynamic pistas_linhas/1.
:- dynamic tabula_regioes/1.

get_Tab(T) :- tabuleiro(T).
get_PC(PC) :- pistas_colunas(PC).
get_PL(PL) :- pistas_linhas(PL).
get_Reg(RT) :- tabula_regioes(PL).


%%%%%%%%%%%%%%%%%%%%%%
%%	Testes			%%
%%%%%%%%%%%%%%%%%%%%%%
/*
 =============================================================================================
	Testar mostra_tabuleiro

	| ? - get_Tab(T),get_PC(PC),get_PL(PL),mostra_tabuleiro(PL,PC,T).	
*/	
	tabuleiro(
	[ 	 
		[ [0,1],[0,1],[1,2],[0,3],[0,3],[1,4],[0,5],[0,5],[1,6],[0,7] ],
		[ [0,1],[0,1],[1,2],[1,8],[0,9],[0,10],[0,5],[1,11],[0,12],[0,13] ],
		[ [0,14],[0,15],[0,16],[1,8],[1,17],[1,17],[0,5],[1,11],[0,12],[1,18] ],
		[ [0,19],[0,19],[0,16],[0,20],[1,21],[0,22],[1,23],[1,23],[1,18],[1,18] ],
		[ [0,24],[0,25],[0,20],[0,20],[1,21],[1,21],[1,23],[1,23],[0,26],[0,26] ],
		[ [0,27],[0,27],[0,20],[1,28],[0,29],[0,29],[1,30],[0,31],[1,32],[1,32] ],
		[ [1,33],[1,34],[1,34],[0,35],[1,36],[0,29],[1,37],[1,37],[1,37],[0,38] ],
		[ [1,33],[0,39],[0,39],[0,40],[0,41],[0,42],[1,37],[0,43],[0,43],[0,38] ],
		[ [0,44],[1,45],[1,47],[1,47],[0,48],[0,48],[1,50],[1,50],[0,43],[0,38] ],
		[ [0,44],[0,46],[0,46],[1,47],[0,49],[0,49],[0,51],[1,50],[1,52],[1,52] ]
		
	]).

	pistas_colunas(
		[[2,1],[2,2],[4,3],[5,3],[4,2],[4,2],[6,1],[7,3],[5,4],[4,3]]
	).
	pistas_linhas(
		[[3,3],[4,3],[5,3],[5,2],[4,1],[4,3],[7,3],[2,2],[5,2],[4,2]]
	).
/*
 =============================================================================================
	
*/	