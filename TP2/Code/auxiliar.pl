%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%
/*

*/

tabuleiro_buscar_valor(L, C, T, [P,R]) :-
			tabuleiro(T),
			nth1(L, T, LI),
			nth1(C,LI,[P,R]).
			