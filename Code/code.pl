
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(displays).
:- use_module(auxiliar).
:- use_module(cpu).

%:-now(time), setrand(time).

%%%%%%%%%%%%%%%%%%%%%%
%%	Init			%%
%%%%%%%%%%%%%%%%%%%%%%
	%
	%	Algumas funções finais/teste requerem que alguns factos sejam declarados como dinamicos antes de serem usados, 
	%	pelo que o fazemos logo aqui
	%


jogador(jogador1).
jogador(jogador2).

:- dynamic tabuleiro/1.
tabuleiro([ [[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
			[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]] ]).
			

:- dynamic mao/2.			
mao(jogador1, []).
mao(jogador2, []).

:- dynamic jogador_escolhido/1.
jogador_escolhido(jogador1).
	
:- dynamic estado/1.
estado(T, J) :- tabuleiro(T), jogador_escolhido(J).

:- dynamic baralho/1.
baralho([ [0|0], [0|1], [0|2], [0|3], [0|4], [0|5], [0|6], [0|7], [1|1], [1|2], 
			[1|3], [1|4], [1|5], [1|6], [1|7], [2|2], [2|3], [2|4], [2|5], [2|6],
			[2|7], [3|3], [3|4], [3|5], [3|6], [3|7], [4|4], [4|5], [4|6], [4|7],
			[5|5], [5|6], [5|7], [6|6], [6|7], [7|7] ]).



%%%%%%%%%%%%%%%%%%%%%%
%%	Main			%%
%%%%%%%%%%%%%%%%%%%%%%
	%
	%	Funções Importantes:
	%
	%	:- dominup.
	%		A forma correcta que deve ser chamada para o jogador iniciar o jogo.
	%
	%	:- jogar(Escolha).
	%		Predicado que é chamado pelos menus e que indica qual o modo de jogo escolhido à função main_loop,
	%		após iniciar o jogo adequadamente. O jogo começa com: 
	%			1- Dar aleatóriamente as peças do baralho aos jogadores, uma a uma.
	%			2- A procura da peça [7|7] nas mãos dos jogadores.
	%			3- Esse jogador é forçado a jogar essa peça no centro do tabuleiro.
	%			4- O jogador passa a vez e o outro joga normalmente daqui por diante.
	%		O passo 1 é feito por baralho_dar_as_pecas.
	%		Os passo 2 e 3 são feito por main_jogada_inicial.
	%
	%
	%	:- main_loop(Escolha).
	%		Quatro versões desta funções fazem a lógica do jogo de formas diferentes. Todos tem que iniciar a vez 
	%		do jogadores ou computadores quando adequado, e todos tem que ser capazes de terminar o jogo quando se
	%		atinge a condição de vitória (mão vazia).
	%		Modos:
	%				1 - Jogador vs Jogador
	%				2 - Jogador vs CPU fácil
	%				3 - Jogador vs CPU dificil
	%				4 - CPU dificil vs CPU dificil
	%		O computador difcil comporta-se como um fácil até ter menos que 1 peças.	
	%
	%	
	%	:- main_victoria(J).
	%		Anuncia a vitória do jogador J e acaba o jogo depois de mostrar o tabuleiro final.
	%		
	%	:- main_jogador_humano(J).	
	%	:- main_jogador_computador_facil(J).
	%	:- main_jogador_computador_dificil(J).
	%		Predicados que fazem displays/reads e verificam as jogadas dos jogadores ou computadores.
	%		O computador facil joga aleatoriamente de entre todas as jogadas válidas, o computador dificil 
	%		escreve a indicação de que está "-cuidadosamente-" a fazer a melhor decisão entre todas as escolhas.
	%		Se o oponente não poder jogar peças no próximo turno, não é trocada a indicação de qual o jogador
	%		que joga a seguir.
	%
	%


dominup :- menu_principal.

jogar(Escolha) :- 
	Escolha >= 0, Escolha < 4,
	baralho_reiniciar,
	mao_reiniciar(jogador1),
	mao_reiniciar(jogador2),
	tabuleiro_reiniciar,
	baralho_dar_as_pecas,
	!,
	main_jogada_inicial,
	!,
	repeat,
	main_loop(Escolha).
			
main_jogada_inicial :-
	mao_quem_tem_peca([7|7], JI),
	mao_remover_peca(JI, [7|7]),
	tabuleiro_jogar_peca([7|7], [5|6], [6|6]),
	jogador_escolhido(J),
	(JI = J -> jogador_trocar_vez(J); true).
	
main_loop(0) :- 
		jogador_escolhido(J),
		main_jogador_humano(J),
		!,
		(mao_vazia(J) ->  main_victoria(J); main_loop(0)).

main_loop(1) :-
	jogador_escolhido(J),
	(J = 'jogador1' -> main_jogador_humano(J); main_jogador_computador_facil(J)),
	!,
	(mao_vazia(J) ->  main_victoria(J); main_loop(1)).
	
main_loop(2) :-
	jogador_escolhido(J),
	mao(J, MJ), length(MJ, ML),
	(J = 'jogador1' -> main_jogador_humano(J)
					; 
					( ML < 10
						-> main_jogador_computador_dificil(J);
							main_jogador_computador_facil(J)
					)
	),
	!,
	(mao_vazia(J) ->  main_victoria(J); main_loop(2)).

main_loop(3) :-
	jogador_escolhido(J),
	mao(J, MJ), length(MJ, ML),
	( ML < 10
		-> main_jogador_computador_dificil(J);
			main_jogador_computador_facil(J)
	),
	!,
	(mao_vazia(J) ->  main_victoria(J); main_loop(3)).
	
	
main_victoria(J) :-
	cls,
	nl,nl,nl,
	write('                VICTORIA DO JOGADOR '), write(J),nl,nl,
	write('tabuleiro final:'), nl,
	tabuleiro(T),
	mostra_tabuleiro(T).
	
main_jogador_humano(J) :-
	%Escolher a Peca
		cls,
		tabuleiro(T),
		mostra_tabuleiro(T),
		mostra_mao_jogador(J),
			mao(J,M), length(M, ML),
		readInt('Qual a peca que quer jogar?', Input, 1, ML),
	%Escolher a Posição da cabeça
		!,
		cls,
		mostra_tabuleiro(T),
		mao_escolher_peca(J, Input, [V1|V2]),
		write('Peca escolhida: ['), write(V1), write('|'), write(V2), write(']. Valor da cabeca: '), write(V1) , nl,
			tabuleiro([TH|TR]), length([TH|TR], NL), length(TH,NC),
		write('Quais as coordenadas da cabeca da peca que quer jogar?'), nl,
		readInt('Coluna?', C1, 1, NC),
		readInt('Linha?', L1, 1, NL),
	%Escolher a Posição da cauda
		write('Peca escolhida: ['), write(V1), write('|'), write(V2), write(']. Valor da cauda: '), write(V2) , nl,
			tabuleiro([TH|TR]), length([TH|TR], NL), length(TH,NC),
		write('Quais as coordenadas da cauda da peca que quer jogar?'), nl,
		readInt('Coluna?', C2, 1, NC),
		readInt('Linha?', L2, 1, NL),
	%Verificar se e valido
		!,
		cls,
		write('A verificar se a jogada e valida...'), nl, sleep(1),
		(
		tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]);
		tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2]) 
		),
		write('A jogada e valida e foi efectuada'), nl, sleep(1),
	%alterar tabuleiro, tirar a peça ao jogador
		tabuleiro_jogar_peca([V1|V2], [C1|L1], [C2|L2]),
		mao_remover_peca(J, [V1|V2]),
	%Ver se o outro jogador pode jogar, trocar a vez se sim
		!,
		jogador(Joutro), Joutro \= J,
		(jogador_pode_jogar(Joutro) -> jogador_trocar_vez(J); true),	
	%expandir o tabuleiro se necessário
		tabuleiro_dimensiona, tabuleiro_dimensiona.
		
main_jogador_computador_facil(J) :-
		cls,
		tabuleiro(T),
		mostra_tabuleiro(T),
		mostra_mao_jogador(J),
	%Encontra a listade todas as jogadas possiveis, com prioridade a Climb
		write('O computador '), write(J), write(' esta a escolher uma peca para jogar...'), nl,
		cpu_uma_ao_calhas(J,G,T),
	%Escolher a Jogada que vai fazer
		length(G, JogadasL),
		random(0, JogadasL, N),
		list_element_at([[V1|V2],[C1|L1],[C2|L2]], G, N),
		nl,write('Peca escolhida: ['), write(V1), write('|'), write(V2), write('].'), nl,
		write('Posicao Escolhida: '), write(C1), write(':'), write(L1),write(', Cauda:'), write(C2), write(':'), write(L2), nl,
		sleep(3),
	%Verificar se e valido
		!,
		(
		 tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]);	
		 tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2])
		 ),
	%alterar tabuleiro, tirar a peça ao jogador
		tabuleiro_jogar_peca([V1|V2], [C1|L1], [C2|L2]),
		!,
		(mao_remover_peca(J, [V1|V2]);mao_remover_peca(J, [V2|V1])),
	%Ver se o outro jogador pode jogar, trocar a vez se sim
		jogador(Joutro), Joutro \= J,
		(jogador_pode_jogar(Joutro) -> jogador_trocar_vez(J); true),	
	%expandir o tabuleiro se necessário
		tabuleiro_dimensiona, tabuleiro_dimensiona.

main_jogador_computador_dificil(J) :-
		cls,
		tabuleiro(T),
		mostra_tabuleiro(T),
		mostra_mao_jogador(J),
	%Encontra a lista de todas as jogadas possiveis, com prioridade a Climb
		write('O computador '), write(J), write(' esta -cuidadosamente- a escolher uma peca para jogar...'), nl, sleep(1),
	%Escolher a Jogada que vai fazer
		qualidade_a_melhor_jogada(J, [V1|V2],[C1|L1],[C2|L2]),
		nl,write('Peca escolhida: ['), write(V1), write('|'), write(V2), write('].'), nl,
		write('Posicao Escolhida: '), write(C1), write(':'), write(L1),write(', Cauda:'), write(C2), write(':'), write(L2), nl,
		sleep(3),
	%Verificar se e valido
		!,
		(
		 tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]);	
		 tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2])
		 ),
	%alterar tabuleiro, tirar a peça ao jogador
		tabuleiro_jogar_peca([V1|V2], [C1|L1], [C2|L2]),
		!,
		(mao_remover_peca(J, [V1|V2]);mao_remover_peca(J, [V2|V1])),
	%Ver se o outro jogador pode jogar, trocar a vez se sim
		jogador(Joutro), Joutro \= J,
		(jogador_pode_jogar(Joutro) -> jogador_trocar_vez(J); true),	
	%expandir o tabuleiro se necessário
		tabuleiro_dimensiona, tabuleiro_dimensiona.
		

		
		
		
		
%%%%%%%%%%%%%%%%%%%%%%
%%	Jogador			%%
%%%%%%%%%%%%%%%%%%%%%%
	%	
	%	:- jogador_trocar_vez(J)
	%		Encontra o jogador que esteja marcado para fazer o próximo turno, e troca essa marca para o oponente
	%	
	%	:- jogador_pode_jogar(J).
	%	:- jogador_jogadas_disponiveis_climb(J).
	%	:- jogador_jogadas_disponiveis_expand(J).
	%		Percorre as posições do tabuleiro, por 2 linhas e por 2 colunas, e vê se é possivel o jogador colocar
	%		qualquer uma das peças que tem na mão.
	%		Nas funções auxiliares, as variaveis L e M representam a mão do jogador M e a quantidade de peças nela L.
	%	
	%	

jogador_trocar_vez(J) :- jogador(X),
						X \= J,
						retract(jogador_escolhido(J)),
						assert(jogador_escolhido(X)).

jogador_pode_jogar(J) :-
	(
	jogador_jogadas_disponiveis_climb(J);
	jogador_jogadas_disponiveis_expand(J)
	).
						

jogador_jogadas_disponiveis_climb(J) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		jogador_jogadas_disponiveis_climb_aux_linhas(L,M);
		jogador_jogadas_disponiveis_climb_aux_colunas(L,M)
	).

jogador_jogadas_disponiveis_climb_aux_linhas(L, M) :- 
	tabuleiro([Z|X]),
	length(Z, NC),
	length([Z|X], NL),
	NLL is NL-1,
	!,
	incrementador(1, N, L),
	list_element_at([V1|V2], M, N),
	incrementador(1, L1, NLL),
	L2 is L1+1,
	incrementador(1, C1, NC),
	C2 is C1,
	(tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]);
	tabuleiro_pode_jogar_peca_climb([V2|V1], [C1|L1], [C2|L2])).

jogador_jogadas_disponiveis_climb_aux_colunas(L, M) :- 
	tabuleiro([Z|X]),
	length(Z, NC),
	length([Z|X], NL),
	NCC is NC-1,
	!,
	incrementador(1, N, L),
	list_element_at([V1|V2], M, N),
	incrementador(1, L1, NL),
	L2 is L1,
	incrementador(1, C1, NCC),
	C2 is C1+1,
	(tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]);
	tabuleiro_pode_jogar_peca_climb([V2|V1], [C1|L1], [C2|L2])).



jogador_jogadas_disponiveis_expand(J) :-
	jogador(J),
    mao(J, M),
	length(M, L),
	( 
		jogador_jogadas_disponiveis_expand_aux_linhas(L,M);
		jogador_jogadas_disponiveis_expand_aux_colunas(L,M)
	).
	
jogador_jogadas_disponiveis_expand_aux_linhas(L, M) :- 
	tabuleiro([Z|X]),
	length(Z, NC),
	length([Z|X], NL),
	NLL is NL-1,
	!,
	incrementador(1, N, L),
	list_element_at([V1|V2], M, N),
	incrementador(1, L1, NLL),
	L2 is L1+1,
	incrementador(1, C1, NC),
	C2 is C1,
	(tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2]);
	tabuleiro_pode_jogar_peca_expand([V2|V1], [C1|L1], [C2|L2])).
	
jogador_jogadas_disponiveis_expand_aux_colunas(L, M) :- 
	tabuleiro([Z|X]),
	length(Z, NC),
	length([Z|X], NL),
	NCC is NC-1,
	!,
	incrementador(1, N, L),
	list_element_at([V1|V2], M, N),
	incrementador(1, L1, NL),
	L2 is L1,
	incrementador(1, C1, NCC),
	C2 is C1+1,
	(tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2]);
	tabuleiro_pode_jogar_peca_expand([V2|V1], [C1|L1], [C2|L2])).
						

%%%%%%%%%%%%%%%%%%%%%%
%%	Tabuleiro		%%
%%%%%%%%%%%%%%%%%%%%%%
	%	
	%	:- tabuleiro_set(TI, [C|L], V, A, TF).
	%		Recebe um tabuleiro TI, e muda os valores nas coordenadas C,L pelos valores V,A resultando no tabuleiro TF.
	%	
	%	:- tabuleiro_get(T,[C|L], V, A).
	%		Encontra no tabuleiro T as coordenadas C,L e devolve os valores [V|A].
	%		
	%	:- tabuleiro_distancia_coordenadas([C1|L1], [C2|L2], D)
	%		Dados dois conjuntos de coordenadas, CL1 e CL2, indica a distancia a que estão uma da outra.
	%		A distancia 1 significa que são adjacentes na horizontal ou vertical.
	%		
	%	:- tabuleiro_jogar_peca(P, CL1, CL2).
	%		Joga a peça P no tabuleiro nas coordenadas CL1 (cabeça) e CL2 (cauda). Assume que já foram feitas 
	%		verificações pelo que só ve se as coordenadas tem a mesma altura.
	%		
	%	:-tabuleiro_pode_jogar_peca_expand(P, CL1, CL2).
	%	:-tabuleiro_pode_jogar_peca_climb(P, CL1, CL2).
	%		As verificações à função anterior. Apenas indicam se pode ser jogado ou não.
	%		
	%	:- tabuleiro_reiniciar.
	%		Esvazia o tabuleiro de jogo e retorna-o as suas dimensões às originais.
	%		
	%		

tabuleiro_set(TI, [C|L], V, A, TF) :-
	matrix_setCell(L, C, TI, [V|A], TF).
	
	
tabuleiro_get(T,[C|L], V, A) :- 
	C > 0,
	L > 0,
	list_element_at(Y, T, L),
	list_element_at([V|A], Y, C).
	

tabuleiro_distancia_coordenadas([C1|L1], [C2|L2], D) :-
	DX is C1-C2,
	X is abs(DX),
	DY is L1-L2,
	Y is abs(DY),
	D is X+Y.
	
tabuleiro_jogar_peca([V1|V2], [C1|L1], [C2|L2]) :-
	tabuleiro(TI),
	tabuleiro_get(TI,[C1|L1], _, A1),
	tabuleiro_get(TI,[C2|L2], _, A2),
	A1 == A2,
	A is A1+1,
	tabuleiro_set(TI, [C1|L1], V1, A, TF),
	retract(tabuleiro(TI)),
	assert(tabuleiro(TF)),
	tabuleiro_set(TF, [C2|L2], V2, A, TFF),
	retract(tabuleiro(TF)),
	assert(tabuleiro(TFF)).
	

tabuleiro_pode_jogar_peca_expand([V1|V2], [C1|L1], [C2|L2]) :-
	tabuleiro(T),
	%buscar os valores no tabuleiro
		tabuleiro_get(T, [C1|L1], _, TA1),
		tabuleiro_get(T, [C2|L2], _, TA2),
		!,
	%as coordenadas devem estar a tocar-se
		tabuleiro_distancia_coordenadas([C1|L1], [C2|L2], D),
		D == 1,
	%as alturas devem ser iguais
		TA1 == TA2,
	%uma das peças circundantes devem ter o mesmo valor		
		A is TA1 + 1,
		(
			expand_aux(V1, [C1|L1], A, T);
			expand_aux(V2, [C2|L2], A, T)
		).
			
			expand_aux(V, [C|L], A, T) :-
				%à esquerda
					C2 is C-1,
					tabuleiro_get(T, [C2|L], V2, A2),
					A == A2,
					V == V2.
			expand_aux(V, [C|L], A, T) :-
				%à direita
					C2 is C+1,
					tabuleiro_get(T, [C2|L], V2, A2),
					A == A2,
					V == V2.
			expand_aux(V, [C|L], A, T) :-
				%abaixo
					L2 is L+1,
					tabuleiro_get(T, [C|L2], V2, A2),
					A == A2,
					V == V2.
			expand_aux(V, [C|L], A, T) :-
				%acima
					L2 is L-1,
					tabuleiro_get(T, [C|L2], V2, A2),
					A == A2,
					V == V2.


tabuleiro_pode_jogar_peca_climb([V1|V2], [C1|L1], [C2|L2]) :-
	tabuleiro(T),
	%buscar os valores no tabuleiro
		tabuleiro_get(T, [C1|L1], TV1, TA1),
		tabuleiro_get(T, [C2|L2], TV2, TA2),
		!,
	%as coordenadas devem estar a tocar-se
		tabuleiro_distancia_coordenadas([C1|L1], [C2|L2], D),
		D == 1,
	%as alturas devem ser iguais
		TA1 == TA2,
	%a altura não pode ser 0
		TA1 \= 0,
	%os valores no tabuleiro devem ser iguais ao da peça
		TV1 == V1,
		TV2 == V2.
	
tabuleiro_reiniciar :-
		tabuleiro(T),
		retract(tabuleiro(T)),
		assert(
			tabuleiro([ [[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]],
						[[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0],[0|0]] ])
		).

%%%%%%%%%%%%%%%%%%%%%%
%%	mao jogador		%%
%%%%%%%%%%%%%%%%%%%%%%
	%		
	%	:- mao_acrescentar_peca(P, J).
	%		Acrescenta a peça [P] à mão do jogador J. 
	%		
	%	:- mao_remover_peca(J, P).
	%		Remove a peça P da mão do jogador J, se existir.
	%		
	%		
	%	:- mao_escolher_peca(J, N, P).
	%		Devolve em P a N-ésima peça da mão do jogador J.	
	%		
	%	:- mao_vazia(J).
	%		Sucesso se o jogador não tiver peças na mão.			
	%		
	%	:- mao_quem_tem_peca(P, J).
	%		Procura a peça P nas mãos dos jogadores todos e devolve em J o jogador que a tiver.	
	%		
	%	:- mao_reiniciar(J).
	%		Esvazia a mão do jogador J.			
	%		

mao_acrescentar_peca(P, J) :- 	mao(J, V),
								append(P,V,N),
								retract(mao(J,V)),
								assert(mao(J, N)).
															
mao_remover_peca(J, P) :- 
	jogador(J),
	mao(J,MV),
	list_delete_one(P, MV, MN),
	retract(mao(J,MV)),
	assert(mao(J,MN)).
						
mao_escolher_peca(J, X, P) :-
	jogador(J),
	mao(J,M),
	list_element_at(P, M, X).

mao_vazia(J) :-
	jogador(J),
	mao(J, M),
	length(M,0).
						
								
mao_quem_tem_peca(P, J) :-
	mao(J,M),
	member(P,M).

mao_reiniciar(J) :-
		jogador(J),
		mao(J, M),
		retract(mao(J, M)),
		assert(mao(J, [])).		

%%%%%%%%%%%%%%%%%%%%%%
%%	Baralho			%%
%%%%%%%%%%%%%%%%%%%%%%
	%		
	%	:- baralho_vazio.
	%		Passa se o baralo estiver vazio.
	%		
	%	:- baralho_get_peca_from(P).
	%		Remove uma peça aleatória ao baralho e devolve-a em P.	
	%		
	%	:-baralho_dar_as_pecas.	
	%		Corre o predicado baralho_get_peca_from repetidamente de forma a dar todas as peças do baralho
	%		aos jogadores, alternadamete. Pára quando o baralho estiver vazio.
	%		
	%	:- baralho_reiniciar.
	%		Reinicia o baralho no seu estado original, com uma cópia de cada peça.
	%
	%

baralho_vazio :- 	baralho(B),
					mostra_linha(B),
					!,
					length(B,0).
					
baralho_get_peca_from(P) :- baralho(B),	
							length(B, L),
							LS is L + 1,
							random(0, LS, X),
							list_element_at(P, B, X).

baralho_reiniciar :-
			baralho(B),
			retract(baralho(B)),
			assert(
			baralho([ 
				[0|0], [0|1], [0|2], [0|3], [0|4], [0|5], [0|6], [0|7], [1|1], [1|2], 
				[1|3], [1|4], [1|5], [1|6], [1|7], [2|2], [2|3], [2|4], [2|5], [2|6],
				[2|7], [3|3], [3|4], [3|5], [3|6], [3|7], [4|4], [4|5], [4|6], [4|7],
				[5|5], [5|6], [5|7], [6|6], [6|7], [7|7] ])
			).
			
							
baralho_dar_as_pecas :-
				repeat,
				baralho(B),
				baralho_get_peca_from(P),
				list_delete_one(P,B,C),
				retract(baralho(B)),
				assert(baralho(C)),
				jogador_escolhido(J),
				mao_acrescentar_peca([P],J),
				jogador_trocar_vez(J),
				baralho(Y),
				length(Y, 0).