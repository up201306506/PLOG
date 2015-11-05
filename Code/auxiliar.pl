%%%%%%%%%%%%%%%%%%%%%%
%% Manipular Listas	%%
%%%%%%%%%%%%%%%%%%%%%%
				
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
	

	
%%%%%%%%%%%%%%%%%%%%%%
%% Outras funções	%%
%%%%%%%%%%%%%%%%%%%%%%

readInt(Texto, I, Min, Max) :-
	repeat,
	write(Texto), nl,
	read(I),
	I >= Min, I =< Max.
