%%%%%%%%%%%%%%%%%%%%%%
%% Manipular Listas	%%
%%%%%%%%%%%%%%%%%%%%%%
	%	Funções Importantes:
	%
	%	:-list_element_at(E,L,N).
	%		Devolve o N-ésimo elemento da lista L em E.
	%
	%	:-list_delete_one(X,L1,L2).
	%		Remove o X-ésimo elemento da lista L1, resultando na lista L2.
	%
	%	:-matrix_setCell(L, C, TI, E, TF).
	%		Recebe uma matriz TI (com o propósito de receber tabuleiros), e uma coordenada coluna C + linha L, e 
	%		devolve a matriz TF igual à inicial excepto que o elemento na coordenada é trocado pelo elemento E.
	%
	%
	
	
list_element_at(X,[X|_],1).
list_element_at(X,[_|L],K) :- 
	list_element_at(X,L,K1), 
	K is K1 + 1.
			
list_delete_one(X,L1,L2) :- 	
	append(A1,[X|A2],L1),
	append(A1,A2,L2).

matrix_setCell(1, Col, [H|T], Piece, [H1|T]) :-
	matrix_setCellCol(Col,H,Piece,H1).
matrix_setCell(N,Col,[H|T],Piece,[H|T1]):-
	Prev is N-1,
	matrix_setCell(Prev, Col, T, Piece, T1).

matrix_setCellCol(1, [_|T], Piece, [Piece|T]).
matrix_setCellCol(N, [H|T], Piece, [H|T1]):-
	Prev is N-1,
	matrix_setCellCol(Prev, T, Piece, T1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Redimensionar o Tabuleiro	%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%	Funções Importantes:
	%
	%	:-tabuleiro_dimensiona.
	%		Encontra o tabuleiro actualmente definido pelo jogo e, com o uso às funções
	%			*tabuleiro_primeira_linha_vazia.		*tabuleiro_insere_linha_inicio.
	%			*tabuleiro_ultima_linha_vazia.			*tabuleiro_insere_linha_fim.
	%			*tabuleiro_primeira_coluna_vazia.		*tabuleiro_insere_coluna_inicio.
	%			*tabuleiro_ultima_coluna_vazia.			*tabuleiro_insere_coluna_fim.
	%		identifica se é necessário expandir o tamanho deste tabuleiro para permitir os jogadores pousarem
	%		peças na direcção ocupada. Um espaço vazio é o elemento [0|0].
	%		Cada chamada acrescenta duas linhas ou duas colunas à direcção em necessidade. 
	%		Não faz nada noutros casos.
	%		É necessário chamar a função duas vezes para garantir que peças pousadas nos cantos do tabuleiro 
	%		são lidadas adequadamente.
	%
	
	
tabuleiro_dimensiona :-
	tabuleiro(T),
	length(T, N),
	repeat,
		(\+tabuleiro_primeira_coluna_vazia(1, T, N) -> (tabuleiro_insere_coluna_inicio,tabuleiro_insere_coluna_inicio);
		\+tabuleiro_ultima_coluna_vazia(1, T, N) -> (tabuleiro_insere_coluna_fim,tabuleiro_insere_coluna_fim);
		\+tabuleiro_primeira_linha_vazia -> (tabuleiro_insere_linha_inicio,tabuleiro_insere_linha_inicio);
		\+tabuleiro_ultima_linha_vazia -> (tabuleiro_insere_linha_fim,tabuleiro_insere_linha_fim);
		true).

tabuleiro_primeira_linha_vazia :-
	tabuleiro(T),
	list_element_at(L,T,1),
	tabuleiro_linha_vazia(L).

tabuleiro_ultima_linha_vazia :-
	tabuleiro(T),
	length(T, N),
	list_element_at(L,T,N),
	tabuleiro_linha_vazia(L).

tabuleiro_linha_vazia([]).
tabuleiro_linha_vazia([H|T]) :-
	tabuleiro_primeiro_elemento_vazio([H|T]),
	tabuleiro_linha_vazia(T).

tabuleiro_primeira_coluna_vazia(N, _, Max) :-
	N > Max.
tabuleiro_primeira_coluna_vazia(N, T, Max) :-
	list_element_at(L, T, N),
	!,
	tabuleiro_primeiro_elemento_vazio(L),
	N1 is N+1,
	tabuleiro_primeira_coluna_vazia(N1, T, Max).

tabuleiro_ultima_coluna_vazia(N, _, Max) :-
	N > Max.
tabuleiro_ultima_coluna_vazia(N, T, Max) :-
	list_element_at(L, T, N),
	!,
	tabuleiro_ultimo_elemento_vazio(L),
	N1 is N+1,
	tabuleiro_ultima_coluna_vazia(N1, T, Max).

tabuleiro_primeiro_elemento_vazio([[_|A]|_]) :-
	A == 0.

tabuleiro_ultimo_elemento_vazio(L) :-
	reverse(L, NL),
	tabuleiro_primeiro_elemento_vazio(NL).

tabuleiro_insere_linha_fim :-
	tabuleiro(T),
	list_element_at(PL, T, 1),
	length(PL,C),
	acrescenta_vazio([], C, LF),
	!,
	append(T, [LF], TF),
	retract(tabuleiro(T)),
	assert(tabuleiro(TF)).

tabuleiro_insere_linha_inicio :-
	tabuleiro(T),
	list_element_at(PL, T, 1),
	length(PL,C),
	acrescenta_vazio([], C, LF),
	!,
	append([LF], T, TF),
	retract(tabuleiro(T)),
	assert(tabuleiro(TF)).

tabuleiro_insere_coluna_fim :-
	tabuleiro(T),
	length(T, Max),
	!,
	acrescenta_coluna_fim(T, 1, Max).

acrescenta_coluna_fim(_, N, Max) :-
	N > Max.
acrescenta_coluna_fim(T, N, Max) :-
	list_element_at(L, T, N),
	append(L, [[0|0]], LF),
	select(L, T, LF, TF),
	retract(tabuleiro(T)),
	assert(tabuleiro(TF)),
	N1 is N+1,
	acrescenta_coluna_fim(TF, N1, Max).
	
tabuleiro_insere_coluna_inicio :-
	tabuleiro(T),
	length(T, Max),
	!,
	acrescenta_coluna_inicio(T, 1, Max).

acrescenta_coluna_inicio(_, N, Max) :-
	N > Max.
acrescenta_coluna_inicio(T, N, Max) :-
	list_element_at(L, T, N),
	append([[0|0]], L, LF),
	select(L, T, LF, TF),
	retract(tabuleiro(T)),
	assert(tabuleiro(TF)),
	N1 is N+1,
	acrescenta_coluna_inicio(TF, N1, Max).
	
acrescenta_vazio(L, 1, LF) :-
	append([[0|0]], L, LF).

acrescenta_vazio(L, N, LF) :-
	append([[0|0]], L, LI),
	N1 is N-1,
	acrescenta_vazio(LI, N1, LF).

%%%%%%%%%%%%%%%%%%%%%%
%% Outras funções	%%
%%%%%%%%%%%%%%%%%%%%%%
	%	Funções Importantes:
	%
	%	:-readInt(Texto, I, Min, Max).
	%		Escreve um texto dado e pede ao jogadorum numero I que tem de se encontrar entre os 
	%		valores Min e Max, senão pede um novo.
	%
	%	:-incrementador(P, N, Max).
	%		Devolve em N, um a um, todos os valores entre P e Max, e depois falha.
	%
	

readInt(Texto, I, Min, Max) :-
	repeat,
	write(Texto), nl,
	read(I),
	I >= Min, I =< Max.

readInt_NoRepeat(Texto, I, Min, Max) :-
	write(Texto), nl,
	read(I),
	I >= Min, I =< Max.

incrementador(P, N, Max) :-
	P =< Max,
	N is P.
incrementador(P, N, Max) :-
	P =< Max,
	R is P+1,
	incrementador(R, N, Max).