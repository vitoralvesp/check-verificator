{- TODO 
 - Ler a Notação de Forsyth
   - Formato da Entrada: [Peças Pretas, Peões das Peças Pretas, Linhas entre as Peças, Peões das Peças Brancas, Peças Brancas]
 - Realizar a leitura da notação e converter em uma matriz 8 x 8
 - Identificar a posição do rei branco na matriz
 - Verificar se alguma peça preta pode atacá-lo
 - Retornar True se alguma peça pode atacá-lo, ou False caso contrário
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
         


