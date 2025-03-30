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
  



