/**
 * Projeto: Esta em xeque?
 * 
 * Integrantes:
 * Jessica Bispo, 10410798
 * Lucas Pires de Camargo Sarai, 10418013
 * Vitor Alves Pereira, 10410862
 * 
 * Referências:
 * [ 1 ] https://pt.wikipedia.org/wiki/Notação_Forsyth
 * [ 2 ] https://pt.wikipedia.org/wiki/XadrezLeis_do_xadrez
 * 
 */

/* PARTE 1: LER A NOTAÇÃO DE FORSYTH E PROCESSAR COMO TABULEIRO *
 * ToDo: Por enquanto, só processa se as peças brancas estiverem com aspas simples ('T', 'B', etc); 
 * conversar com o Basile sobre os critérios para a entrada. 
 */

% Descrição: Interpreta a notação de forsyth em uma matriz que representa o tabuleiro
% Parâmetros: Lista, Lista
criar_tabuleiro([], []).
criar_tabuleiro([Linha|Resto], [LinhaInterpretada|TabuleiroInterpretado]) :-
    processar_linha(Linha, LinhaInterpretada),
    criar_tabuleiro(Resto, TabuleiroInterpretado).

% Descrição: Converte os números (1 a 8) em '.', mas mantém as listas como estão
% Parâmetros: Número (1 a 8) ou Caractere (Peças Pretas ou Brancas), Lista
processar_linha(N, Lista) :-
    number(N),
    gerar_vazias(N, Lista).

processar_linha(Linha, LinhaInterpretada) :-
    is_list(Linha),
    expandir_linha(Linha, LinhaInterpretada).

% Descrição: Realiza a interpretação das linhas que possuem espaços vazios e peças
% Detalhes: O método atom é utilizado para identificar os átomos na linha, que no caso são as peças pretas e brancas
% Parâmetros: Lista, Lista
expandir_linha([],[]).
expandir_linha([X|Xs], Resultado) :-
    number(X),
    gerar_vazias(X, Vazias),
    expandir_linha(Xs, Resto),
    append(Vazias, Resto, Resultado).

expandir_linha([X|Xs], [Simbolo|Resto]) :-
    atom(X),
    atom_string(X, S),
    string_chars(S, [Char]),
    atom_chars(Simbolo, [Char]),
    expandir_linha(Xs, Resto).

expandir_linha([X|Xs], [Simbolo|Resto]) :-
    var(X),
    term_to_atom(X, Atom),
    atom_chars(Atom, [Char]),
    atom_chars(Simbolo, [Char]),
    expandir_linha(Xs, Resto).

% Descrição: Função auxiliar para converter os números (1 a 8) em '.'
% Parâmetros: Número (0 a 8), Lista
gerar_vazias(0, []).
gerar_vazias(N, ['.'|Resto]) :-
    N > 0,
    N1 is N-1,
    gerar_vazias(N1, Resto).


/* PARTE 2: PROCESSAR TODAS AS PEÇAS PRETAS DO TABULEIRO, INCLUINDO PEÇA E COORDENADAS X E Y */

% Descrição: Valida se a peça é preta (diferente de letras maiúsculas ou '.')
% Parâmetros: Átomo
validar_peca_preta(Peca) :-
    atom(Peca),
    atom_chars(Peca, [Letra]),
    char_type(Letra, lower),
    Peca \= '.'.

% Descrição: Seleciona as peças pretas no tabuleiro, percorrendo linha a linha com índice Y
% Parâmetros: Lista de Lista
selecionar_pecas(Tabuleiro, Posicoes) :-
    selecionar_pecas(Tabuleiro, 0, Posicoes).

selecionar_pecas([], _, []).
selecionar_pecas([Linha|Resto], Y, Posicoes) :-
	coletar_linha(Linha, 0, Y, PosicoesLinha),
    Y1 is Y + 1,
    selecionar_pecas(Resto, Y1, PosicoesResto),
    append(PosicoesLinha, PosicoesResto, Posicoes).

% Descrição: Percorre uma linha, elemento por elemento
% Parâmetros: Tabuleiro, X, Y, Lista de Listas com as peças pretas e suas coodenadas x e y na forma [peça, x, y]
coletar_linha([], _, _, []).
coletar_linha([Peca|Resto], X, Y, [[Peca, X, Y]|Outros]) :-
    validar_peca_preta(Peca),
    X1 is X + 1,
    coletar_linha(Resto, X1, Y, Outros).

coletar_linha([Outro|Resto], X, Y, Resultado) :-
    \+ validar_peca_preta(Outro),
    X1 is X + 1,
    coletar_linha(Resto, X1, Y, Resultado).


/* PARTE 3: SIMULAR O MOVIMENTO DAS PEÇAS PRETAS */

% 
movimento_vertical_para_baixo(_, Y, Contador, Capacidade, _, false) :-
    (Y >= 7; Contador >= Capacidade), !.

movimento_vertical_para_baixo(X, Y, Contador, Capacidade, Tabuleiro, Resultado) :-
    Y1 is Y + 1,
    nth0(Y1, Tabuleiro, Linha),
    nth0(X, Linha, Peca),
    (
    	Peca == 'R' -> Resultado = true;
    	Peca \== '.' -> Resultado = false;
    	ContadorAux is Contador + 1,
        movimento_vertical_para_baixo(X, Y1, ContadorAux, Capacidade, Tabuleiro, Resultado)
    ).

% Descrição: Simula o movimento vertical indo do final até o topo do tabuleiro
% Parâmetros: Coordenada X, Coordenada Y, Contador de Movimentos, Capacidade de Movimentos, Tabuleiro, Resultado
movimento_vertical_para_cima(_, Y, Contador, Capacidade, _, false) :-
    (Y <= 0; Contador >= Capacidade), !.

movimento_vertical_para_cima(X, Y, Contador, Capacidade, Tabuleiro, Resultado) :-
    Y1 is Y - 1,
    nth0(Y1, Tabuleiro, Linha),
    nth0(X, Linha, Peca),
    (
    	Peca == 'R' -> Resultado = true;
    	Peca \== '.' -> Resultado = false;
    	ContadorAux is Contador + 1,
        movimento_vertical_para_cima(X, Y1, ContadorAux, Capacidade, Tabuleiro, Resultado)
    ).

% Descrição: Simula o movimento horizontal indo da esquerda à direita do tabuleiro
% Parâmetros: Coordenada X, Coordenada Y, Contador de Movimentos, Capacidade de Movimentos, Tabuleiro, Resultado
movimento_horizontal_para_a_direita(X, _, Contador, Capacidade, _, false) :-
    (X >= 7; Contador >= Capacidade), !.

movimento_horizontal_para_a_direita(X, Y, Contador, Capacidade, Tabuleiro, Resultado) :-
    X1 is X + 1,
    nth0(Y, Tabuleiro, Linha),
    nth0(X1, Linha, Peca),
    (
    	Peca == 'R' -> Resultado = true;
    	Peca \== '.' -> Resultado = false;
    	ContadorAux is Contador + 1,
        movimento_horizontal_para_a_direita(X1, Y, ContadorAux, Capacidade, Tabuleiro, Resultado)
    ).

% Descrição: Simula o movimento horizontal indo da direita à esquerda do tabuleiro
% Parâmetros: Coordenada X, Coordenada Y, Contador de Movimentos, Capacidade de Movimentos, Tabuleiro, Resultado
movimento_horizontal_para_a_esquerda(X, _, Contador, Capacidade, _, false) :-
    (X =< 0; Contador >= Capacidade), !.

movimento_horizontal_para_a_esquerda(X, Y, Contador, Capacidade, Tabuleiro, Resultado) :-
    X1 is X - 1,
    nth0(Y, Tabuleiro, Linha),
    nth0(X1, Linha, Peca),
    (
    	Peca == 'R' -> Resultado = true;
    	Peca \== '.' -> Resultado = false;
    	ContadorAux is Contador + 1,
        movimento_horizontal_para_a_esquerda(X1, Y, ContadorAux, Capacidade, Tabuleiro, Resultado)
    ).

% Descrição: Simula o movimento diagonal no sentido inferior direito do tabuleiro
% Parâmetros: Coordenada X, Coordenada Y, Contador de Movimentos, Capacidade de Movimentos, Tabuleiro, Resultado
movimento_diagonal_inferior_direita(X, Y, _, Capacidade, _, false) :-
    (X >= 7; Y >= 7; Capacidade =< 0), !.

movimento_diagonal_inferior_direita(X, Y, Tabuleiro, Capacidade, Resultado) :-
    X1 is X + 1,
    Y1 is Y + 1,
    nth0(Y1, Tabuleiro, Linha),
    nth0(X1, Linha, Peca),
    (
        Peca == 'R' -> Resultado = true;
        Peca \== '.' -> Resultado = false;
        Capacidade1 is Capacidade - 1,
        movimento_diagonal_inferior_direita(X1, Y1, Tabuleiro, Capacidade1, Resultado)
    ).

% Descrição: Simula o movimento diagonal no sentido inferior esquerdo do tabuleiro
% Parâmetros: Coordenada X, Coordenada Y, Contador de Movimentos, Capacidade de Movimentos, Tabuleiro, Resultado
movimento_diagonal_inferior_esquerda(X, Y, _, Capacidade, _, false) :-
    (X =< 0; Y >= 7; Capacidade =< 0), !.

movimento_diagonal_inferior_esquerda(X, Y, Tabuleiro, Capacidade, Resultado) :-
    X1 is X - 1,
    Y1 is Y + 1,
    nth0(Y1, Tabuleiro, Linha),
    nth0(X1, Linha, Peca),
    (
        Peca == 'R' -> Resultado = true;
        Peca \== '.' -> Resultado = false;
        Capacidade1 is Capacidade - 1,
        movimento_diagonal_inferior_esquerda(X1, Y1, Tabuleiro, Capacidade1, Resultado)
    ).

% Descrição: Simula o movimento diagonal no sentido superior direito do tabuleiro
% Parâmetros: Coordenada X, Coordenada Y, Contador de Movimentos, Capacidade de Movimentos, Tabuleiro, Resultado
movimento_diagonal_superior_direita(X, Y, _, Capacidade, _, false) :-
    (X >= 7; Y =< 0 ; Capacidade =< 0), !.

movimento_diagonal_superior_direita(X, Y, Tabuleiro, Capacidade, Resultado) :-
    X1 is X + 1,
    Y1 is Y - 1,
    nth0(Y1, Tabuleiro, Linha),
    nth0(X1, Linha, Peca),
    (
        Peca == 'R' -> Resultado = true;
        Peca \== '.' -> Resultado = false;
        Capacidade1 is Capacidade - 1,
        movimento_diagonal_superior_direita(X1, Y1, Tabuleiro, Capacidade1, Resultado)
    ).

% Descrição: Simula o movimento diagonal no sentido superior esquerdo do tabuleiro
% Parâmetros: Coordenada X, Coordenada Y, Contador de Movimentos, Capacidade de Movimentos, Tabuleiro, Resultado
movimento_diagonal_superior_esquerda(X, Y, _, Capacidade, _, false) :-
    (X =< 0; Y =< 0; Capacidade =< 0), !.

movimento_diagonal_superior_esquerda(X, Y, Tabuleiro, Capacidade, Resultado) :-
    X1 is X - 1,
    Y1 is Y - 1,
    nth0(Y1, Tabuleiro, Linha),
    nth0(X1, Linha, Peca),
    (
        Peca == 'R' -> Resultado = true;
        Peca \== '.' -> Resultado = false;
        Capacidade1 is Capacidade - 1,
        movimento_diagonal_superior_esquerda(X1, Y1, Tabuleiro, Capacidade1, Resultado)
    ).
