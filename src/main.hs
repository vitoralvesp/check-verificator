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
check_verificator _[[]] = False
check_verificator chess_board = True

-- Leitura da Notação de Forsyth em uma Matriz
forsyth_mtrx :: [String] -> [[Int]]
forsyth_mtrx _[] = []
forsyth_mtrx forsyth_notation = [[1,2,3]]


