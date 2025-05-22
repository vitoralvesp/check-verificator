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

/**
 * [[t,c,b,r,d,r,b,c,t],8,8,8,8,8,8,[T,C,B,D,R,B,C,T]]
 * 
 * 
 *  
 */

criar_tabuleiro([[]], [[]]).
criar_tabuleiro([A|X], tabuleiro) :-
    writeln(A),
    criar_tabuleiro(X, tabuleiro).    