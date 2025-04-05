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
movimento_vertical_para_baixo (x, y) contador_de_movimento capacidade_de_movimento_em_y tabuleiro
                  | (y >= 7) || (contador_de_movimento >= capacidade_de_movimento_em_y) = False
                  | tabuleiro !! (y + 1) !! x == 'R' = True
                  | tabuleiro !! (y + 1) !! x /= '.' = False
                  | otherwise = movimento_vertical_para_baixo (x, y + 1) (contador_de_movimento + 1) capacidade_de_movimento_em_y tabuleiro

movimento_vertical_para_cima :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_vertical_para_cima (x, y) contador_de_movimento capacidade_de_movimento_em_y tabuleiro
                  | (y <= 0) || (contador_de_movimento >= capacidade_de_movimento_em_y) = False
                  | tabuleiro !! (y - 1) !! x == 'R' = True
                  | tabuleiro !! (y - 1) !! x /= '.' = False
                  | otherwise = movimento_vertical_para_cima (x, y - 1) (contador_de_movimento + 1) capacidade_de_movimento_em_y tabuleiro

-- 0 1
movimento_horizontal_para_a_direita :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_horizontal_para_a_direita (x, y) contador_de_movimento capacidade_de_movimento_para_a_direita tabuleiro
                  | (x >= 7) || (contador_de_movimento >= capacidade_de_movimento_para_a_direita) = False
                  | tabuleiro !! y !! (x + 1) == 'R' = True
                  | tabuleiro !! y !! (x + 1) /= '.' = False
                  | otherwise = movimento_horizontal_para_a_direita (x + 1, y) (contador_de_movimento + 1) capacidade_de_movimento_para_a_direita tabuleiro

movimento_horizontal_para_a_esquerda :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_horizontal_para_a_esquerda (x, y) contador_de_movimento capacidade_de_movimento_para_a_esquerda tabuleiro
                  | (x <= 0) || (contador_de_movimento >= capacidade_de_movimento_para_a_esquerda) = False
                  | tabuleiro !! y !! (x - 1) == 'R' = True
                  | tabuleiro !! y !! (x - 1) /= '.' = False
                  | otherwise = movimento_horizontal_para_a_esquerda (x - 1, y) (contador_de_movimento + 1) capacidade_de_movimento_para_a_esquerda tabuleiro

-- se for zero, acessar como tabuleiro 0 0
-- caso contrario, acessar como tabuleiro y x
movimento_na_diagonal_principal_cima :: (Int, Int) -> Int -> Int -> Int -> Int -> [[Char]] -> Bool
movimento_na_diagonal_principal_cima (x, y) contador_de_movimento_para_a_direita contador_de_movimento_para_a_esquerda capacidade_de_movimento_para_a_direita capacidade_de_movimento_para_a_esquerda tabuleiro
                  | (x < 0) || (y < 0) 
                  || (contador_de_movimento_para_a_direita >= capacidade_de_movimento_para_a_direita) 
                  || (contador_de_movimento_para_a_esquerda >= capacidade_de_movimento_para_a_esquerda) = False
                  | (x == 0 && y == 0) && tabuleiro !! 0 !! 0 == 'R' = True
                  | (x == 0 && y == 0) && tabuleiro !! 0 !! 0 /= '.' = False
                  | (x > 0 && y > 0) && tabuleiro !! (y - 1) !! (x - 1) == 'R' = True
                  | (x > 0 && y > 0) && tabuleiro !! (y - 1) !! (x - 1) /= '.' = False
                  | otherwise = movimento_na_diagonal_principal_cima (x - 1, y - 1) (contador_de_movimento_para_a_direita + 1) (contador_de_movimento_para_a_esquerda + 1) capacidade_de_movimento_para_a_direita capacidade_de_movimento_para_a_esquerda tabuleiro

movimento_na_diagonal_principal_baixo :: (Int, Int) -> Int -> Int -> Int -> Int -> [[Char]] -> Bool
movimento_na_diagonal_principal_baixo (x, y) contador_de_movimento_para_a_direita contador_de_movimento_para_baixo capacidade_de_movimento_para_a_direita capacidade_de_movimento_para_baixo tabuleiro
                  | (x >= 7) || (y >= 7)
                  || (contador_de_movimento_para_a_direita >= capacidade_de_movimento_para_a_direita) 
                  || (contador_de_movimento_para_baixo >= capacidade_de_movimento_para_baixo) = False
                  | tabuleiro !! (y + 1) !! (x + 1) == 'R' = True
                  | tabuleiro !! (y + 1) !! (x + 1) /= '.' = False
                  | otherwise = movimento_na_diagonal_principal_baixo (x + 1, y + 1) (contador_de_movimento_para_a_direita + 1) (contador_de_movimento_para_baixo + 1) capacidade_de_movimento_para_a_direita capacidade_de_movimento_para_baixo tabuleiro

movimento_na_diagonal_secundaria_cima :: (Int, Int) -> Int -> Int -> Int -> Int -> [[Char]] -> Bool
movimento_na_diagonal_secundaria_cima (x, y) contador_de_movimento_para_a_direita contador_de_movimento_para_cima capacidade_de_movimento_para_a_direita capacidade_de_movimento_para_cima tabuleiro
                  | (x >= 7) || (y <= 0) 
                  || (contador_de_movimento_para_a_direita >= capacidade_de_movimento_para_a_direita)
                  || (contador_de_movimento_para_cima >= capacidade_de_movimento_para_cima) = False
                  | (y == 0) && tabuleiro !! 0 !! (x + 1) == 'R' = True
                  | (y == 0) && tabuleiro !! 0 !! (x + 1) /= '.' = False
                  | (y > 0) && tabuleiro !! (y - 1) !! (x + 1) == 'R' = True
                  | (y > 0) && tabuleiro !! (y - 1) !! (x + 1) /= '.' = False
                  | otherwise = movimento_na_diagonal_secundaria_cima (x + 1, y - 1) (contador_de_movimento_para_a_direita + 1) (contador_de_movimento_para_cima + 1) capacidade_de_movimento_para_a_direita capacidade_de_movimento_para_cima tabuleiro

movimento_na_diagonal_secundaria_baixo :: (Int, Int) -> Int -> Int -> Int -> Int -> [[Char]] -> Bool
movimento_na_diagonal_secundaria_baixo (x, y) contador_de_movimento_para_a_esquerda contador_de_movimento_para_baixo capacidade_de_movimento_para_a_esquerda capacidade_de_movimento_para_baixo tabuleiro
                  | (x < 0) || (y > 7)
                  || (contador_de_movimento_para_a_esquerda >= capacidade_de_movimento_para_a_esquerda)
                  || (contador_de_movimento_para_baixo >= capacidade_de_movimento_para_baixo) = False
                  | (x == 0) && tabuleiro !! (y + 1) !! 0 == 'R' = True
                  | (x == 0) && tabuleiro !! (y + 1) !! 0 /= '.' = False
                  | (x > 0) && tabuleiro !! (y + 1) !! (x - 1) == 'R' = True
                  | (x > 0) && tabuleiro !! (y + 1) !! (x - 1) /= '.' = False
                  | otherwise = movimento_na_diagonal_secundaria_baixo (x - 1, y + 1) (contador_de_movimento_para_a_esquerda + 1) (contador_de_movimento_para_baixo + 1) capacidade_de_movimento_para_a_esquerda capacidade_de_movimento_para_baixo tabuleiro

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
      | movimento_vertical_para_cima (x, y) 0 7 tabuleiro 
      || movimento_vertical_para_baixo (x, y) 0 7 tabuleiro 
      || movimento_horizontal_para_a_direita (x, y) 0 7 tabuleiro 
      || movimento_horizontal_para_a_esquerda (x, y) 0 7 tabuleiro = True
      | otherwise = False

cavalo :: (Char, (Int, Int)) -> Bool
cavalo (peca, (x, y)) = False

bispo :: (Char, (Int, Int)) -> [[Char]] -> Bool
bispo (peca, (x, y)) tabuleiro
      | movimento_na_diagonal_principal_cima (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_secundaria_cima (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_principal_baixo (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_secundaria_baixo (x, y) 0 0 7 7 tabuleiro = True
      | otherwise = False

rainha :: (Char, (Int, Int)) -> [[Char]] -> Bool
rainha (peca, (x, y)) tabuleiro
      | movimento_vertical_para_cima (x, y) 0 7 tabuleiro 
      || movimento_vertical_para_baixo (x, y) 0 7 tabuleiro 
      || movimento_horizontal_para_a_direita (x, y) 0 7 tabuleiro 
      || movimento_horizontal_para_a_esquerda (x, y) 0 7 tabuleiro
      || movimento_na_diagonal_principal_cima (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_secundaria_cima (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_principal_baixo (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_secundaria_baixo (x, y) 0 0 7 7 tabuleiro = True
      | otherwise = False

rei :: (Char, (Int, Int)) -> [[Char]] -> Bool
rei (peca, (x, y)) tabuleiro
    | movimento_vertical_para_cima (x, y) 0 1 tabuleiro 
    || movimento_vertical_para_baixo (x, y) 0 1 tabuleiro 
    || movimento_horizontal_para_a_direita (x, y) 0 1 tabuleiro 
    || movimento_horizontal_para_a_esquerda (x, y) 0 1 tabuleiro
    || movimento_na_diagonal_principal_cima (x, y) 0 0 1 1 tabuleiro
    || movimento_na_diagonal_secundaria_cima (x, y) 0 0 1 1 tabuleiro
    || movimento_na_diagonal_principal_baixo (x, y) 0 0 1 1 tabuleiro
    || movimento_na_diagonal_secundaria_baixo (x, y) 0 0 1 1 tabuleiro = True
    | otherwise = False

peao :: (Char, (Int, Int)) -> [[Char]] -> Bool
peao (peca, (x, y)) tabuleiro 
     | (x == 0 && y == 7) || (x == 7 && y == 7) || (y == 7) = False  
     | x == 0 = verifica_diagonal_direita_baixo (x, y) tabuleiro  
     | x == 7 = verifica_diagonal_esquerda_baixo (x, y) tabuleiro 
     | otherwise = verifica_diagonal_esquerda_baixo (x, y) tabuleiro 
                   || verifica_diagonal_direita_baixo (x, y) tabuleiro

-- Verifica se o Rei Branco está em xeque
esta_em_xeque :: [String] -> Bool
esta_em_xeque notacao_de_forsyth = esta_em_xeque_aux (selecionar_pecas (criar_tabuleiro notacao_de_forsyth)) (criar_tabuleiro notacao_de_forsyth)

esta_em_xeque_aux :: [(Char, (Int, Int))] -> [[Char]] -> Bool
esta_em_xeque_aux [] _ = False
esta_em_xeque_aux ((peca, (x, y)):restante) tabuleiro
  | peca == 't' = torre (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'b' = bispo (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'd' = rainha (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'r' = rei (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'p' = peao (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | otherwise = esta_em_xeque_aux restante tabuleiro

--
main :: IO()
main = do

  putStr "\nTabuleiro interpretado da Notacao Forsyth:\n"
  -- print (criar_tabuleiro ["1111r111","pppRpppp","8","8","8","8","PPPPPPP","TCBDRBCT"])

  -- tcbdrbct, pppppppp

  mapM_ print ["........","....R...","..r.....","........","........","........","PPPPPPPP","TCBDRBCT"]
  
  putStr "\nSelecao das Pecas: "
  print (selecionar_pecas ["........","........","..r.....","........","........","........","PPPPPPPP","TCBDRBCT"])
  
  putStr "\nEsta em Xeque? "
  print (esta_em_xeque ["8","4R3","2r5","8","8","8","PPPPPPPP","TCBDRBCT"])
