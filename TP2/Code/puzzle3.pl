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

pistas_linhas(
	[[1,1],[1,1],[2,1]]
).

pistas_colunas(
	[[1,1],[1,1],[2,1]]
).


tabela_regioes(
[
	[01,01,02],
	[01,01,02],
	[03,03,04]
]).