{- TODO 

Peças = torre; cavalo; bispo; dama; rei; peão

Algoritmo:
- Procura peças pretas e retorna posição por linha
- Faz trajetos possíveis
- Verifica se alguma alcança rei branco
verificaCheque = verificaTorre || verificaCavalo || verificaBispo || verificaDama || verificaReiPreto || verificaPeão
	Se um for verdadeiro (se um já colocar em xeque), o programa retorna verdadeiro. Caso contrário (nenhum xeque), retorna falso

Separar tarefas:
	Lucas: Torre
	Vitor: Bispo
	Jéssica: Peão

Casos base: peão, torre e bispo

Dama = Bispo + Torre
Rei = peão + Bispo
Cavalo = 8 movimentos:
      L cima/direita
      L cima/esquerda
      L direita/cima
      L direita/baixo
      L baixo/direita
      L baixo/esquerda
      L esquerda/cima
      L esquerda/baixo 

 -}

-- Verifica se o Rei Branco está em Xeque
check_verificator :: [[Int]] -> Bool
check_verificator [[]] = False
check_verificator board = True

{- 
 - Leitura da Notação de Forsyth em uma Matriz
 - [String]: notação de forsyth
 - [[Char]]: matriz 8x8 no formato de um tabuleiro de xadrez
 -}
chess_board :: [String] -> [[Char]]
chess_board = map (concatMap forsyth_notation)
     where 
     forsyth_notation character
       | character >= '1' && character <= '8' = replicate (read [character]) '.'
       | otherwise = [character]
         
charParaInt :: Char -> Int
charParaInt c = read [c] :: Int

procura_peca :: Char -> [Char] -> Int
procura_peca peca_procurada lst= procura peca_procurada 0 lst


procura :: Char -> Int -> [Char] -> Int
procura _ _ [] = -1
procura peca_procurada pos [elemento]
        | elemento == peca_procurada = pos
        | otherwise = -1
procura peca_procurada pos (elemento:(prox_elemento:resto))
        | elemento == peca_procurada = pos
        | elemento >= '1' && elemento <= '8' = procura peca_procurada (pos+ (charParaInt elemento)) (prox_elemento:resto) 
        | otherwise = procura peca_procurada (pos+1) (prox_elemento:resto)

-- Verifica a linha da torre
verifica_torre_linha :: String -> Bool
verifica_torre_linha [] = False
verifica_torre_linha (a:xs) = verifica_torre_linha_aux 0 pos_torre posicao_rei (a:xs)
        where 
                posicao_rei = procura_peca 'R' (a:xs)
                pos_torre = procura_peca 't' (a:xs)

verifica_torre_linha_aux :: Int -> Int -> Int -> String -> Bool
verifica_torre_linha_aux _ _ _ [] = False
verifica_torre_linha_aux ac pos_torre pos_rei (a:xs)
        | pos_rei == -1 = False
        | (a >= '1' && a <= '8') && (((pos_rei + (charParaInt a)+1) == pos_torre) || ((pos_rei - (charParaInt a)+1) == pos_torre))= True
        | a >= '1' && a <= '8' = verifica_torre_linha_aux (ac + (charParaInt a)) pos_torre pos_rei xs
        | ((ac > pos_torre && ac < pos_rei) || (ac > pos_rei && ac < pos_torre))= False 
        | (a == 'R' && pos_rei > pos_torre) || (pos_rei == pos_torre+1) || (pos_rei == pos_torre-1) = True
        | otherwise = verifica_torre_linha_aux (ac+1) pos_torre pos_rei xs

-- Verifica a coluna da torre

{-
        FIXME: se houver peça na coluna da torre entre a torre e o rei, deve retornar falso, não verdadeiro.
-}
verifica_torre_coluna :: Int -> [String] -> Bool
verifica_torre_coluna _ [] = False
verifica_torre_coluna pos_torre (a:xs) = verifica_torre_coluna_prox_linhas pos_torre a || verifica_torre_coluna pos_torre xs

verifica_torre_coluna_prox_linhas :: Int -> String -> Bool
verifica_torre_coluna_prox_linhas pos_torre lst = verifica_torre_coluna_prox_linhas_aux 0 pos_torre lst

verifica_torre_coluna_prox_linhas_aux :: Int -> Int -> String -> Bool
verifica_torre_coluna_prox_linhas_aux _ _ [] = False  
verifica_torre_coluna_prox_linhas_aux ac pos_torre (a:xs)
    | ac == pos_torre = (a == 'R')
    | ac > pos_torre = False  
    | a >= '1' && a <= '8' = verifica_torre_coluna_prox_linhas_aux (ac + (charParaInt a)+1) pos_torre xs  
    | otherwise = verifica_torre_coluna_prox_linhas_aux (ac + 1) pos_torre xs
