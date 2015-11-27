

%%%%%%%%%%%%%%%%%%%%%%
%%	Limpar			%%
%%%%%%%%%%%%%%%%%%%%%%
/*
	Funções Importantes:	
		?-cls.
			Limpa o terminal e empurra para o topo novos elementos que forem mostrados.
*/
	

cls :- write('\e[2J').

%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%
/*

	?- mostra_tabuleiro(PC,PL,T)
		PC - Lista de pistas nas Colunas
		PL - Lista de pistas nas Linhas
		T - Tabela de valores



     +---+---+---+---+---+---+---+---+---+---+
     |R 1|R 1|R 1|R 1|R 1|R 1|R 1|R 1|R 1|R 1|
     |Q 2|Q12|Q 8|Q 2|Q 2|Q 2|Q 2|Q 2|Q 2|Q 2| 
     +++++++++++++++++++++++++++++++++++++++++
+---++---+---+---+---+---+---+---+---+---+---+
|R 1+|   |   |XXX|   |   |   |   |   |   |   |
|   +|   |   |XXX|   |   |   |   |   |   |   |
|Q 2+|   |   |XXX|   |   |   |   |   |   |   |
+---++---+---+---+   +---+---+---+   +---+---+
|R 1+|   |   |       |   |   |       |   |   |
|   +|   |   |       |   |   |       |   |   |
|Q 2+|   |   |       |   |   |       |   |   |
+---++---+---+---+---+---+---+   +   +---+---+
|R 1+|   |   |   |   |   |   |       |   |   |
|   +|   |   |   |   |   |   |       |   |   |
|Q 2+|   |   |   |   |   |   |       |   |   |
+---++---+---+---+---+---+---+---+---+---+---+
|R 1+|   |   |   |   |   |   |   |   |   |   |
|   +|   |   |   |   |   |   |   |   |   |   |
|Q 2+|   |   |   |   |   |   |   |   |   |   |
+---++---+---+---+---+---+---+---+---+---+---+
*/


mostra_tabuleiro(PC,PL,T) :-	length(PC,NC),
								length(PL,NL),
								%ver se NL e NC são iguas aos da tabela
								mostra_topo_tabela(PC,NC)
								.

mostra_topo_tabela(PC,NC) :- 	write('     +'),mostra_separador_topo1(NC),
						reverse(PC,RPC),
						write('     |'),mostra_pista_coluna_R(RPC,NC),
						write('     |'),mostra_pista_coluna_Q(RPC,NC),
						write('     +'),mostra_separador_topo2(NC).
						
mostra_separador_topo1(0):- nl.		
mostra_separador_topo1(NC) :-	write('---+'),
								N is NC-1,
								mostra_separador_topo1(N).

mostra_separador_topo2(0):- nl.	
mostra_separador_topo2(NC) :-	write('++++'),
								N is NC-1,
								mostra_separador_topo2(N).
mostra_pista_coluna_R(_, 0):- nl.		
mostra_pista_coluna_R(PC, NC) :-write('R '),
								nth1(NC,PC,[X,_]),
								write(X),
								write('|'),
								N is NC-1,
								mostra_pista_coluna_R(PC, N).

mostra_pista_coluna_Q(_, 0):- nl.		
mostra_pista_coluna_Q(PC, NC) :-write('Q '),
								nth1(NC,PC,[_,X]),
								write(X),
								write('|'),
								N is NC-1,
								mostra_pista_coluna_Q(PC, N).