-- Interpreta a notação de forsyth em uma matriz que representa o tabuleiro de xadrez
criar_tabuleiro :: [String] -> [[Char]]
criar_tabuleiro = map (concatMap notacao_de_forsyth)
  where
    notacao_de_forsyth caractere
      | caractere >= '1' && caractere <= '8' = replicate (read [caractere]) '.'
      | otherwise = [caractere]

-- Seleciona todas as peças pretas e armazena as suas coordenadas
selecionar_pecas :: [[Char]] -> [(Char, (Int, Int))]
selecionar_pecas tabuleiro = selecionar_pecas_aux tabuleiro 0 []

-- Função auxiliar que percorre o tabuleiro linha por linha
selecionar_pecas_aux :: [[Char]] -> Int -> [(Char, (Int, Int))] -> [(Char, (Int, Int))]
selecionar_pecas_aux [] _ resultado = resultado
selecionar_pecas_aux (linha:restante) y resultado =
    selecionar_pecas_aux restante (y + 1) (resultado ++ selecionar_peca_linha linha y 0 [])

-- Função auxiliar que percorre uma linha do tabuleiro
selecionar_peca_linha :: [Char] -> Int -> Int -> [(Char, (Int, Int))] -> [(Char, (Int, Int))]
selecionar_peca_linha [] _ _ resultado = resultado
selecionar_peca_linha (peca:restante) y x resultado
    | peca >= 'a' && peca <= 'z' = selecionar_peca_linha restante y (x + 1) (resultado ++ [(peca, (x, y))])
    | otherwise = selecionar_peca_linha restante y (x + 1) resultado

-- Movimentos
movimento_vertical_para_baixo :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_vertical_para_baixo (x, y) capacidade_de_movimento contador_de_movimento tabuleiro
                  | y > capacidade_de_movimento = False
                  | tabuleiro !! y !! x == 'R' = True
                  | tabuleiro !! y !! x /= '.' && contador_de_movimento > 0 = False
                  | otherwise = movimento_vertical_para_baixo (x, y + 1) capacidade_de_movimento (contador_de_movimento+1) tabuleiro

movimento_vertical_para_cima :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_vertical_para_cima (x, y) capacidade_de_movimento contador_de_movimento tabuleiro
                  | y < 0 = False
                  | tabuleiro !! y !! x == 'R' = True
                  | tabuleiro !! y !! x /= '.' && contador_de_movimento > 0 = False
                  | otherwise = movimento_vertical_para_cima (x, y - 1) capacidade_de_movimento (contador_de_movimento+1) tabuleiro

movimento_horizontal_para_a_direita :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_horizontal_para_a_direita (x, y) capacidade_de_movimento contador_de_movimento tabuleiro
                  | x > capacidade_de_movimento = False
                  | tabuleiro !! y !! x == 'R' = True
                  | tabuleiro !! y !! x /= '.' && contador_de_movimento > 0 = False
                  | otherwise = movimento_horizontal_para_a_direita (x + 1, y) capacidade_de_movimento (contador_de_movimento+1) tabuleiro

movimento_horizontal_para_a_esquerda :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_horizontal_para_a_esquerda (x, y) capacidade_de_movimento contador_de_movimento tabuleiro
                  | x < 0 = False
                  | tabuleiro !! y !! x == 'R' = True
                  | tabuleiro !! y !! x /= '.' && contador_de_movimento > 0 = False
                  | otherwise = movimento_horizontal_para_a_esquerda (x - 1, y) capacidade_de_movimento (contador_de_movimento+1) tabuleiro

movimento_na_diagonal_principal_cima :: (Int, Int) -> [[Char]] -> Bool
movimento_na_diagonal_principal_cima (x, y) tabuleiro
                  | x < 0 || y < 0 = False
                  | tabuleiro !! y !! x == 'R' = True
                  | tabuleiro !! y !! x /= '.' = False
                  | otherwise = movimento_na_diagonal_principal_cima (x - 1, y - 1) tabuleiro

movimento_na_diagonal_principal_baixo :: (Int, Int) -> [[Char]] -> Bool
movimento_na_diagonal_principal_baixo (x, y) tabuleiro
                  | x > 7 || y > 7 = False
                  | tabuleiro !! y !! x == 'R' = True
                  | tabuleiro !! y !! x /= '.' = False
                  | otherwise = movimento_na_diagonal_principal_baixo (x + 1, y + 1) tabuleiro

movimento_na_diagonal_secundaria_cima :: (Int, Int) -> [[Char]] -> Bool
movimento_na_diagonal_secundaria_cima (x, y) tabuleiro
                  | x > 7 || y < 0 = False
                  | tabuleiro !! y !! x == 'R' = True
                  | tabuleiro !! y !! x /= '.' = False
                  | otherwise = movimento_na_diagonal_secundaria_cima (x + 1, y - 1) tabuleiro

movimento_na_diagonal_secundaria_baixo :: (Int, Int) -> [[Char]] -> Bool
movimento_na_diagonal_secundaria_baixo (x, y) tabuleiro
                  | x < 0 || y > 7 = False
                  | tabuleiro !! y !! x == 'R' = True
                  | tabuleiro !! y !! x /= '.' = False
                  | otherwise = movimento_na_diagonal_secundaria_baixo (x - 1, y + 1) tabuleiro

verifica_diagonal_esquerda_baixo :: (Int, Int) -> [[Char]] -> Bool
verifica_diagonal_esquerda_baixo (x, y) tabuleiro
      | x - 1 < 0 || y + 1 > 7 = False  
      | tabuleiro !! (y + 1) !! (x - 1) == 'R' = True  
      | otherwise = False

verifica_diagonal_direita_baixo :: (Int, Int) -> [[Char]] -> Bool
verifica_diagonal_direita_baixo (x, y) tabuleiro
      | x + 1 > 7 || y + 1 > 7 = False 
      | tabuleiro !! (y + 1) !! (x + 1) == 'R' = True 
      | otherwise = False

-- Peças
torre :: (Char, (Int, Int)) -> [[Char]] -> Bool
torre (peca, (x, y)) tabuleiro
      | movimento_vertical_para_baixo (x, y) 7 0 tabuleiro 
      || movimento_vertical_para_cima (x, y) 7 7 tabuleiro 
      || movimento_horizontal_para_a_direita (x, y) 7 0 tabuleiro 
      || movimento_horizontal_para_a_esquerda (x, y) 7 7 tabuleiro = True
      | otherwise = False

cavalo :: (Char, (Int, Int)) -> Bool
cavalo (peca, (x, y)) = False

bispo :: (Char, (Int, Int)) -> [[Char]] -> Bool
bispo (peca, (x, y)) tabuleiro
      | x == 0 && y == 0 = movimento_na_diagonal_principal_baixo (x, y) tabuleiro || 
                          movimento_na_diagonal_secundaria_baixo (x, y) tabuleiro
      | x == 0 = movimento_na_diagonal_principal_baixo (x, y) tabuleiro 
                 || movimento_na_diagonal_secundaria_cima (x, y) tabuleiro
      | y == 0 = movimento_na_diagonal_principal_baixo (x, y) tabuleiro 
                 || movimento_na_diagonal_secundaria_baixo (x, y) tabuleiro
      | otherwise = movimento_na_diagonal_principal_cima (x, y) tabuleiro 
                    || movimento_na_diagonal_principal_baixo (x, y) tabuleiro 
                    || movimento_na_diagonal_secundaria_cima (x, y) tabuleiro 
                    || movimento_na_diagonal_secundaria_baixo (x, y) tabuleiro

rainha :: (Char, (Int, Int)) -> Bool
rainha (peca, (x, y)) = False  

rei :: (Char, (Int, Int)) -> Bool
rei (peca, (x, y)) = False  

peao :: (Char, (Int, Int)) -> [[Char]] -> Bool
peao (peca, (x, y)) tabuleiro 
 | x == 0 && y == 7 = False  
 | x == 7 && y == 7 = False  
 | x == 0 = verifica_diagonal_direita_baixo (x, y) tabuleiro  
 | x == 7 = verifica_diagonal_esquerda_baixo (x, y) tabuleiro 
 | y == 7 = False
 | otherwise = verifica_diagonal_esquerda_baixo (x, y) tabuleiro || 
                verifica_diagonal_direita_baixo (x, y) tabuleiro

-- peao (peca, (x, y)) = False  

-- Verifica se o Rei Branco está em xeque
esta_em_xeque :: [String] -> Bool
esta_em_xeque notacao_de_forsyth = esta_em_xeque_aux (selecionar_pecas (criar_tabuleiro notacao_de_forsyth)) (criar_tabuleiro notacao_de_forsyth)

esta_em_xeque_aux :: [(Char, (Int, Int))] -> [[Char]] -> Bool
esta_em_xeque_aux [] _ = False
esta_em_xeque_aux ((peca, (x, y)):restante) tabuleiro
  | peca == 't' = torre (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'b' = bispo (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'p' = peao (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | otherwise = esta_em_xeque_aux restante tabuleiro


-- esta_em_xeque_aux :: [(Char, (Int, Int))] -> [[Char]] -> Bool
-- esta_em_xeque_aux [] _ = False
-- esta_em_xeque_aux ((peca, (x, y)):restante) tabuleiro
--   | peca == 't' = torre (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
--   | peca == 'c' = cavalo (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
--   | peca == 'b' = bispo (peca, (x, y)) tabuleiro|| esta_em_xeque_aux restante tabuleiro
--   | peca == 'd' = rainha (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
--   | peca == 'r' = rei (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
--   | peca == 'p' = peao (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
--   | otherwise = esta_em_xeque_aux restante tabuleiro

--
main :: IO()
main = do
  putStr "\nTabuleiro interpretado da Notação Forsyth: "
  print (criar_tabuleiro ["tcbdrbct","pRpppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"])
  
  putStr "\nSeleção das Peças: "
  print (selecionar_pecas ["tcbdrbct","pRpppppp","........","t......R","........","........","PPPPPPPP","TCBDRBCT"])
  
  putStr "\nEstá em Xeque? "
  print (esta_em_xeque ["tcbdrbct","pRpppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"])

 -- Teste específico para peão
  putStr "\nPeão dando xeque? "
  let tabuleiro_peao = criar_tabuleiro ["8","8","8","..p.....","...R....","8","8","8"]
  let peao_posicao = ('p', (2, 3))
  print (peao peao_posicao tabuleiro_peao)
  
  -- Outro teste para peão
  putStr "\nPeão não dando xeque? "
  let tabuleiro_sem_xeque = criar_tabuleiro ["8","8","8","..p.....","..R.....","8","8","8"]
  let peao_posicao2 = ('p', (2, 3))
  print (peao peao_posicao2 tabuleiro_sem_xeque)