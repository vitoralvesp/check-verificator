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

% Descrição: Interpreta a notação de forsyth em uma matriz que representa o tabuleiro
% Parâmetros: Lista, Lista
criar_tabuleiro([], []).
criar_tabuleiro([Linha|Resto], [LinhaInterpretada|TabuleiroInterpretado]) :-
    converter_linha(Linha, LinhaInterpretada),
    criar_tabuleiro(Resto, TabuleiroInterpretado).

% Descrição: Converte os números (1 a 8) em '.', mas mantém as listas como estão
% Parâmetros: Número (1 a 8) ou Caractere (Peças Pretas ou Brancas), Lista
converter_linha(N, Lista) :-
    number(N),
    gerar_vazias(N, Lista).
converter_linha(Lista, Lista) :-
  	is_list(Lista).

% Descrição: Função auxiliar para converter os números (1 a 8) em '.'
% Parâmetros: Número (0 a 8), Lista
gerar_vazias(0, []).
gerar_vazias(N, ['.'|Resto]) :-
    N > 0,
    N1 is N-1,
    gerar_vazias(N1, Resto).