%%%%%%%%%%%%%%%%%%%%%%
%%	tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%
mostra_peca([V|0]) :- write('     ').
mostra_peca([V|H]) :- write('['),
						write(V),
						write('|'),
						write(H),
						write(']').
						
mostra_linha([P|[]]) :- mostra_peca(P).
mostra_linha([P|R]) :- mostra_peca(P),
					mostra_linha(R).

mostra([L|[]]) :- mostra_linha(L).
mostra([L|R]) :- mostra_linha(L),
				nl,
				mostra(R).
				
				
mostra_tabuleiro([L|R]) :-  write('  1    2    3    4    5  '), nl,
							mostra([L|R]).
			
%%%%%%Exemplo - comando |?- mostra_tab_exemplo. em Prolog

tabuleiro([ [[0|0],[0|0],[0|0],[0|0]] , [[5|1],[7|1],[7|1],[7|1]] , [[0|0],[0|0],[0|0],[0|0]] , [[0|0],[0|0],[0|0],[0|0]] ]).
mostra_tab_exemplo :- tabuleiro(L), write('Tabuleiro:'), nl, mostra_tabuleiro(L).

%%%%%%%%%%%%%%%%%%%%%%
%%	mao jogador		%%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%Exemplo - comando |?- mostra_mao_exemplo. em Prolog

mao([ [2|1],[3|4],[2|5],[6|7],[8|1] ]).
mostra_mao_exemplo :- mao(L), write('Mao do Jogador:'), nl, mostra_linha(L).


[ 
	[	[6|1],[3|1],[5|2],[5|2],[4|1],[1|2],[3|2],[9|0]	],
	[	[3|2],[9|0],[0|2],[6|3],[4|3],[1|3],[4|2],[6|1]	],
	[	[3|2],[3|2],[0|2],[2|2],[0|3],[7|3],[5|2],[0|2]	],
	[	[7|1],[2|2],[4|2],[5|2],[1|3],[2|2],[0|1],[9|0]	],
	[	[9|0],[5|1],[2|1],[2|1],[6|2],[6|3],[7|2],[9|0]	],
	[	[9|0],[1|1],[9|0],[9|0],[7|1],[9|0],[9|0],[9|0]	],
	[	[9|0],[9|0],[9|0],[9|0],[4|1],[9|0],[9|0],[9|0]	]
]