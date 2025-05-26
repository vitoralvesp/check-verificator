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
