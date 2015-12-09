%%%%%%%%%%%%%%%%%%%%%%
%%	Init			%%
%%%%%%%%%%%%%%%%%%%%%%
:- dynamic tabuleiro/1.
:- dynamic pistas_colunas/1.
:- dynamic pistas_linhas/1.
:- dynamic tabela_regioes/1.

get_Tab(T) :- tabuleiro(T).
get_PC(PC) :- pistas_colunas(PC).
get_PL(PL) :- pistas_linhas(PL).
get_Reg(RT) :- tabela_regioes(RT).


%%%%%%%%%%%%%%%%%%%%%%
%%	Testes			%%
%%%%%%%%%%%%%%%%%%%%%%
/*
 =============================================================================================
	Testar mostra_tabuleiro
	| ? - get_PC(PC),get_PL(PL),get_Reg(RT),teste_1_solucao(ST),tabuleiro_contruir_solucoes(ST,RT,T),mostra_tabuleiro(PL,PC,T).

	tabuleiro(
	[ 	 
		[ [0,01],[0,01],[1,02],[0,03],[0,03],[1,04],[0,05],[0,05],[1,06],[0,07] ],
		[ [0,01],[0,01],[1,02],[1,08],[0,09],[0,10],[0,05],[1,11],[0,12],[0,13] ],
		[ [0,14],[0,15],[0,16],[1,08],[1,17],[1,17],[0,05],[1,11],[0,12],[1,18] ],
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

	tabela_regioes(
	[
		[01,01,02,03,03,04,05,05,06,07],
		[01,01,02,08,09,10,05,11,12,13],
		[14,15,16,08,17,17,05,11,12,18],
		[19,19,16,20,21,22,23,23,18,18],
		[24,25,20,20,21,21,23,23,26,26],
		[27,27,20,28,29,29,30,31,32,32],
		[33,34,34,35,36,29,37,37,37,38],
		[33,39,39,40,41,42,37,43,43,38],
		[44,45,47,47,48,48,50,50,43,38],
		[44,46,46,47,49,49,51,50,52,52]
	]).
	
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
	
	| ? - tabuleiro_contruir_solucoes([[1,1,1],[1,0,1],[0,1,0]],[[1,1,2],[3,3,2],[4,4,4]],Tabuleiro).                          

 =============================================================================================
	
*/	