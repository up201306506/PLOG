%%%%%%%%%%%%%%%%%%%%%%
%%	Limpar			%%
%%%%%%%%%%%%%%%%%%%%%%	
/* 
 =============
	cls.
		Limpa o terminal e empurra para o topo novos elementos que forem mostrados. 
 =============
*/
cls :- write('\e[2J').

%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%

/*
 =============
	mostra_tabuleiro(PC,PL,T)
		Mostra um tabuleiro com soluções.
		PC - Lista de pistas nas Colunas
		PL - Lista de pistas nas Linhas
		T - Tabela de valores
 =============
*/
mostra_tabuleiro(PC,PL,T) :-	length(PC,NC),
								length(PL,NL),
								%ver se NL e NC são iguas aos da tabela
								mostra_topo_tabela(PC,NC),
								write('+---++'), mostra_separador_topo1(NC),
								mostra_linhas_tabelas(PL,T,NL,NC,1).

mostra_topo_tabela(PC,NC) :- 	write('     +'),mostra_separador_topo1(NC),
						reverse(PC,RPC),
						write('     |'),mostra_pista_coluna_R(RPC,NC),
						write('     |'),mostra_pista_coluna_Q(RPC,NC),
						write('    ++'),mostra_separador_topo2(NC).
						
mostra_separador_topo1(0):- nl.		
mostra_separador_topo1(NC) :-	write('---+'),
								N is NC-1,
								mostra_separador_topo1(N).

mostra_separador_topo2(0):- nl.	
mostra_separador_topo2(NC) :-	write('---+'),
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
								
mostra_linhas_tabelas(_, _, NL, _,N) :- N > NL.
mostra_linhas_tabelas(PL,T,NL,NC,N) :-	mostra_pista_linha_R(N,PL),  
											mostra_linhas_valores(T,NC,N,1), 
											nl,
										write('|   ||'),
											mostra_linhas_valores(T,NC,N,1), 
											nl,
										mostra_pista_linha_Q(N,PL),  
											mostra_linhas_valores(T,NC,N,1), 
											nl,
										write('+---++'), 
											mostra_linhas_separador(T,NL,NC,N,1),
											nl,
										N2 is N+1,
										mostra_linhas_tabelas(PL,T,NL,NC,N2).
								
								
mostra_pista_linha_R(N,PL) :- 	write('|R '),
								nth1(N,PL,[X,_]),
								write(X),
								write('||').
								
mostra_pista_linha_Q(N,PL) :- 	write('|Q '),
								nth1(N,PL,[_,X]),
								write(X),
								write('||').

mostra_linhas_valores(_,NC,_,C):- C > NC.								
mostra_linhas_valores(T,NC,L,C):-		mostra_tabuleiro_buscar_valor(L, C, T, [P1,R1]),
										(
											P1 =:= 1
											->	write('XXX')
											;	write('   ')
										),
										C2 is C + 1,
										(
											C2 > NC
											->	write('|')
											;	mostra_tabuleiro_buscar_valor(L, C2, T, [_,R2]),
												(
													R1 \= R2
													->	write('|')
													;	write(' ')
												)
												
										),
										mostra_linhas_valores(T,NC,L,C2).
										
mostra_linhas_separador(_,_,NC,_,C) :- C > NC.										
mostra_linhas_separador(T,NL,NC,L,C) :-	
										
										L2 is L + 1,
										(
											L2 > NL
											->	write('---')
											;	mostra_tabuleiro_buscar_valor(L, C, T, [_,R1]),mostra_tabuleiro_buscar_valor(L2, C, T, [_,R2]),
												(
													R1 \= R2
													->	write('---')
													;	write('   ')
												)
												
										),
										C2 is C + 1,
										write('+'),
										mostra_linhas_separador(T,NL,NC,L,C2).

										
mostra_tabuleiro_buscar_valor(L, C, T, [P,R]) :-
			nth1(L, T, LI),
			nth1(C,LI,[P,R]).

										
