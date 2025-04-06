-- Interpreta a notacao de forsyth em uma lista de listas (matriz) que representa o tabuleiro de xadrez
-- [String]: notacao de forsyth
-- [[Char]]: retorna uma lista de listas de caracteres (matriz) como o tabuleiro
criar_tabuleiro :: [String] -> [[Char]]
criar_tabuleiro = map (concatMap notacao_de_forsyth)
  where
    notacao_de_forsyth caractere
      | caractere >= '1' && caractere <= '8' = replicate (read [caractere]) '.'
      | otherwise = [caractere]

-- Seleciona todas as peças pretas e armazena as suas coordenadas x e y
-- [[Char]]: tabuleiro
-- [(Char, (Int, Int))]: retorna uma lista de tuplas para representar uma peca preta e suas coordenadas no tabuleiro
selecionar_pecas :: [[Char]] -> [(Char, (Int, Int))]
selecionar_pecas tabuleiro = selecionar_pecas_aux tabuleiro 0 []

-- Funcao auxiliar de selecionar_pecas que percorre o tabuleiro linha por linha
-- [[Char]]: tabuleiro
-- Int: coordenada y
-- [(Char, (Int, Int))]: lista de tuplas que recebera as coordenadas de y
-- [(Char, (Int, Int))]: retorna uma lista de tuplas para representar uma peca preta e suas coordenadas no tabuleiro
selecionar_pecas_aux :: [[Char]] -> Int -> [(Char, (Int, Int))] -> [(Char, (Int, Int))]
selecionar_pecas_aux [] _ resultado = resultado
selecionar_pecas_aux (linha:restante) y resultado =
    selecionar_pecas_aux restante (y + 1) (resultado ++ selecionar_peca_linha linha y 0 [])

-- Funcao auxiliar de selecionar_pecas_aux que percorre uma linha do tabuleiro
-- [Char]: linha do tabuleiro
-- Int: coordenada y
-- Int: coordenada x
-- [(Char, (Int, Int))]: lista de tuplas que recebera as coordenadas de x
-- [(Char, (Int, Int))]: retorna uma lista de tuplas para representar uma peca preta e suas coordenadas no tabuleiro
selecionar_peca_linha :: [Char] -> Int -> Int -> [(Char, (Int, Int))] -> [(Char, (Int, Int))]
selecionar_peca_linha [] _ _ resultado = resultado
selecionar_peca_linha (peca:restante) y x resultado
    | peca >= 'a' && peca <= 'z' = selecionar_peca_linha restante y (x + 1) (resultado ++ [(peca, (x, y))])
    | otherwise = selecionar_peca_linha restante y (x + 1) resultado

-- Funcao para simular o movimento vertical percorrendo de cima para baixo no tabuleiro
-- (Int, Int): coordenadas x e y de uma peca preta
-- Int: contador de movimento para realizar o controle da quantidade de movimentos possiveis para baixo
-- Int: capacacidade de movimento em y para limitar a quantidade de casas no sentido vertical que uma peca pode avancar
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado e False, caso contrario
movimento_vertical_para_baixo :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_vertical_para_baixo (x, y) contador_de_movimento capacidade_de_movimento_em_y tabuleiro
                  | (y >= 7) || (contador_de_movimento >= capacidade_de_movimento_em_y) = False
                  | tabuleiro !! (y + 1) !! x == 'R' = True
                  | tabuleiro !! (y + 1) !! x /= '.' = False
                  | otherwise = movimento_vertical_para_baixo (x, y + 1) (contador_de_movimento + 1) capacidade_de_movimento_em_y tabuleiro

                  
-- Funcao para simular o movimento vertical percorrendo de baixo para cima no tabuleiro
-- (Int, Int): coordenadas x e y de uma peca preta
-- Int: contador de movimento para realizar o controle da quantidade de movimentos possiveis para cima
-- Int: capacacidade de movimento em y para limitar a quantidade de casas no sentido vertical que uma peca pode avancar
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado e False, caso contrario
movimento_vertical_para_cima :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_vertical_para_cima (x, y) contador_de_movimento capacidade_de_movimento_em_y tabuleiro
                  | (y <= 0) || (contador_de_movimento >= capacidade_de_movimento_em_y) = False
                  | tabuleiro !! (y - 1) !! x == 'R' = True
                  | tabuleiro !! (y - 1) !! x /= '.' = False
                  | otherwise = movimento_vertical_para_cima (x, y - 1) (contador_de_movimento + 1) capacidade_de_movimento_em_y tabuleiro


-- Funcao para simular o movimento horizontal percorrendo da esquerda para a direita no tabuleiro
-- (Int, Int): coordenadas x e y de uma peca preta
-- Int: contador de movimentos para realizar o controle da quantidade de movimentos possiveis para a direita
-- Int: capacidade de movimento em x para limitar a quantidade de casas no sentido horizontal que uma peca pode avancar
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado e False, caso contrario
movimento_horizontal_para_a_direita :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_horizontal_para_a_direita (x, y) contador_de_movimento capacidade_de_movimento_para_a_direita tabuleiro
                  | (x >= 7) || (contador_de_movimento >= capacidade_de_movimento_para_a_direita) = False
                  | tabuleiro !! y !! (x + 1) == 'R' = True
                  | tabuleiro !! y !! (x + 1) /= '.' = False
                  | otherwise = movimento_horizontal_para_a_direita (x + 1, y) (contador_de_movimento + 1) capacidade_de_movimento_para_a_direita tabuleiro


-- Funcao para simular o movimento horizontal percorrendo da esquerda para a direira no tabuleiro
-- (Int, Int): coordenadas x e y de uma peca preta
-- Int: contador de movimento para realizar o controle da quantidade de movimentos possiveis na horizontal
-- Int: capacacidade de movimentos para a direita para limitar a quantidade de casas no sentido horizontal que uma peca pode avancar
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
movimento_horizontal_para_a_esquerda :: (Int, Int) -> Int -> Int -> [[Char]] -> Bool
movimento_horizontal_para_a_esquerda (x, y) contador_de_movimento capacidade_de_movimento_para_a_esquerda tabuleiro
                  | (x <= 0) || (contador_de_movimento >= capacidade_de_movimento_para_a_esquerda) = False
                  | tabuleiro !! y !! (x - 1) == 'R' = True
                  | tabuleiro !! y !! (x - 1) /= '.' = False
                  | otherwise = movimento_horizontal_para_a_esquerda (x - 1, y) (contador_de_movimento + 1) capacidade_de_movimento_para_a_esquerda tabuleiro

-- Funcao para simular o movimento diagonal percorrendo de baixo para cima na diagonal principal do tabuleiro
-- (Int, Int): coordenadas x e y de uma peca preta
-- Int: contador de movimento para a direita para realizar o controle da quantidade de movimentos possiveis na diagonal principal
-- Int: contador de movimento para a esquerda para realizar o controle da quantidade de movimentos possiveis na diagonal principal
-- Int: capacacidade de movimentos para a direita para limitar a quantidade de casas na diagonal principal que uma peca pode avancar
-- Int: capacacidade de movimentos para a esquerda para limitar a quantidade de casas na diagonal principal que uma peca pode avancar
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
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


-- Funcao para simular o movimento diagonal percorrendo de cima para baixo na diagonal principal do tabuleiro
-- (Int, Int): coordenadas x e y de uma peca preta
-- Int: contador de movimento para a direita para realizar o controle da quantidade de movimentos possiveis na diagonal principal
-- Int: contador de movimento para a esquerda para realizar o controle da quantidade de movimentos possiveis na diagonal principal
-- Int: capacacidade de movimentos para a direita para limitar a quantidade de casas na diagonal principal que uma peca pode avancar
-- Int: capacacidade de movimentos para a esquerda para limitar a quantidade de casas na diagonal principal que uma peca pode avancar
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
movimento_na_diagonal_principal_baixo :: (Int, Int) -> Int -> Int -> Int -> Int -> [[Char]] -> Bool
movimento_na_diagonal_principal_baixo (x, y) contador_de_movimento_para_a_direita contador_de_movimento_para_baixo capacidade_de_movimento_para_a_direita capacidade_de_movimento_para_baixo tabuleiro
                  | (x >= 7) || (y >= 7)
                  || (contador_de_movimento_para_a_direita >= capacidade_de_movimento_para_a_direita) 
                  || (contador_de_movimento_para_baixo >= capacidade_de_movimento_para_baixo) = False
                  | tabuleiro !! (y + 1) !! (x + 1) == 'R' = True
                  | tabuleiro !! (y + 1) !! (x + 1) /= '.' = False
                  | otherwise = movimento_na_diagonal_principal_baixo (x + 1, y + 1) (contador_de_movimento_para_a_direita + 1) (contador_de_movimento_para_baixo + 1) capacidade_de_movimento_para_a_direita capacidade_de_movimento_para_baixo tabuleiro


-- Funcao para simular o movimento diagonal percorrendo de baixo para cima na diagonal secundaria do tabuleiro
-- (Int, Int): coordenadas x e y de uma peca preta
-- Int: contador de movimento para a direita para realizar o controle da quantidade de movimentos possiveis na diagonal principal
-- Int: contador de movimento para a esquerda para realizar o controle da quantidade de movimentos possiveis na diagonal principal
-- Int: capacacidade de movimentos para a direita para limitar a quantidade de casas na diagonal principal que uma peca pode avancar
-- Int: capacacidade de movimentos para a esquerda para limitar a quantidade de casas na diagonal principal que uma peca pode avancar
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
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


-- Funcao para simular o movimento diagonal percorrendo de cima para baixo na diagonal secundaria do tabuleiro
-- (Int, Int): coordenadas x e y de uma peca preta
-- Int: contador de movimento para a direita para realizar o controle da quantidade de movimentos possiveis na diagonal principal
-- Int: contador de movimento para a esquerda para realizar o controle da quantidade de movimentos possiveis na diagonal principal
-- Int: capacacidade de movimentos para a direita para limitar a quantidade de casas na diagonal principal que uma peca pode avancar
-- Int: capacacidade de movimentos para a esquerda para limitar a quantidade de casas na diagonal principal que uma peca pode avancar
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
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

-- Funcao para simular o movimento na diagonal secundaria de cima para baixo em uma casa
-- (Int, Int): coordenadas x e y
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
verifica_diagonal_esquerda_baixo :: (Int, Int) -> [[Char]] -> Bool
verifica_diagonal_esquerda_baixo (x, y) tabuleiro
                  | x - 1 < 0 || y + 1 > 7 = False  
                  | tabuleiro !! (y + 1) !! (x - 1) == 'R' = True  
                  | otherwise = False


-- Funcao para simular o movimento na diagonal principal de cima para baixo em uma casa
-- (Int, Int): coordenadas x e y
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
verifica_diagonal_direita_baixo :: (Int, Int) -> [[Char]] -> Bool
verifica_diagonal_direita_baixo (x, y) tabuleiro
                  | x + 1 > 7 || y + 1 > 7 = False 
                  | tabuleiro !! (y + 1) !! (x + 1) == 'R' = True 
                  | otherwise = False

-- Funcao para simular o movimento em L do cavalo de baixo para cima
-- Int: contador de movimentos possiveis para cima
-- (Int, Int): coordenadas x e y
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
movimento_L_cima :: Int -> (Int,Int) -> [[Char]] -> Bool
movimento_L_cima contador_y (x_atual, y_atual) tabuleiro
    | contador_y < 0 = False
    | contador_y == y_atual-2 = movimento_horizontal_para_a_direita (x_atual,contador_y) 0 1 tabuleiro || movimento_horizontal_para_a_esquerda (x_atual,contador_y) 0 1 tabuleiro
    | otherwise = movimento_L_cima (contador_y-1) (x_atual, y_atual) tabuleiro


-- Funcao para simular o movimento em L do cavalo de cima para baixo
-- Int: contador de movimentos possiveis para baixo
-- (Int, Int): coordenadas x e y
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
movimento_L_baixo :: Int -> (Int,Int) -> [[Char]] -> Bool
movimento_L_baixo contador_y (x_atual, y_atual) tabuleiro
    | contador_y > 7 = False
    | contador_y == y_atual+2 = movimento_horizontal_para_a_direita (x_atual,contador_y) 0 1 tabuleiro || movimento_horizontal_para_a_direita (x_atual,contador_y) 0 1 tabuleiro
    | otherwise = movimento_L_baixo (contador_y+1) (x_atual, y_atual) tabuleiro


-- Funcao para simular o movimento em L do cavalo da direita para a esquerda
-- Int: contador de movimentos possiveis para a esquerda
-- (Int, Int): coordenadas x e y
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
movimento_L_esq:: Int -> (Int,Int)-> [[Char]] -> Bool
movimento_L_esq contador_x (x_atual, y_atual) tabuleiro
    | contador_x < 0 = False
    | contador_x == x_atual-2 = movimento_vertical_para_cima (contador_x,y_atual) 0 1 tabuleiro || movimento_vertical_para_baixo(contador_x,y_atual) 0 1 tabuleiro
    | otherwise = movimento_L_esq (contador_x-1) (x_atual, y_atual) tabuleiro


-- Funcao para simular o movimento em L do cavalo da esquerda para a direita
-- Int: contador de movimentos possiveis para a direita
-- (Int, Int): coordenadas x e y
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
movimento_L_dir:: Int ->(Int,Int)-> [[Char]] -> Bool
movimento_L_dir contador_x (x_atual, y_atual) tabuleiro
    | contador_x > 7 = False
    | contador_x == x_atual+2 = movimento_vertical_para_cima (contador_x,y_atual) 0 1 tabuleiro || movimento_vertical_para_baixo(contador_x,y_atual) 0 1 tabuleiro
    | otherwise = movimento_L_dir (contador_x+1) (x_atual, y_atual) tabuleiro

-- Funcao para simular os movimentos da torre
-- (Char, (Int, Int)): coordenadas de uma torre no tabuleiro
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
torre :: (Char, (Int, Int)) -> [[Char]] -> Bool
torre (peca, (x, y)) tabuleiro
      | movimento_vertical_para_cima (x, y) 0 7 tabuleiro 
      || movimento_vertical_para_baixo (x, y) 0 7 tabuleiro 
      || movimento_horizontal_para_a_direita (x, y) 0 7 tabuleiro 
      || movimento_horizontal_para_a_esquerda (x, y) 0 7 tabuleiro = True
      | otherwise = False


-- Funcao para simular os movimentos do cavalo
-- (Int, Int): coordenadas de um cavalo no tabuleiro
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
cavalo :: (Int,Int) -> [[Char]] -> Bool
cavalo (x_atual,y_atual) tabuleiro = movimento_L_cima y_atual (x_atual,y_atual) tabuleiro || movimento_L_baixo y_atual (x_atual,y_atual) tabuleiro || movimento_L_esq x_atual (x_atual,y_atual) tabuleiro || movimento_L_dir x_atual (x_atual,y_atual) tabuleiro 


-- Funcao para simular os movimentos do bispo
-- (Char, (Int, Int)): coordenadas de um bispo no tabuleiro
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
bispo :: (Char, (Int, Int)) -> [[Char]] -> Bool
bispo (peca, (x, y)) tabuleiro
      | movimento_na_diagonal_principal_cima (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_secundaria_cima (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_principal_baixo (x, y) 0 0 7 7 tabuleiro
      || movimento_na_diagonal_secundaria_baixo (x, y) 0 0 7 7 tabuleiro = True
      | otherwise = False


-- Funcao para simular os movimentos da rainha
-- (Char, (Int, Int)): coordenadas de uma rainha no tabuleiro
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
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


-- Funcao para simular os movimentos do rei
-- (Char, (Int, Int)): coordenadas de um rei no tabuleiro
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
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


-- Funcao para simular os movimentos do peao
-- (Char, (Int, Int)): coordenadas de um peao no tabuleiro
-- [[Char]]: tabuleiro
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
peao :: (Char, (Int, Int)) -> [[Char]] -> Bool
peao (peca, (x, y)) tabuleiro 
     | (x == 0 && y == 7) || (x == 7 && y == 7) || (y == 7) = False  
     | x == 0 = verifica_diagonal_direita_baixo (x, y) tabuleiro  
     | x == 7 = verifica_diagonal_esquerda_baixo (x, y) tabuleiro 
     | otherwise = verifica_diagonal_esquerda_baixo (x, y) tabuleiro 
                   || verifica_diagonal_direita_baixo (x, y) tabuleiro

-- Funcao para verificar se o Rei Branco está em xeque
-- [String]: notacao de forsyth
-- Bool: retorna True se o Rei Branco foi encontrado, ou False caso contrario
esta_em_xeque :: [String] -> Bool
esta_em_xeque notacao_de_forsyth = esta_em_xeque_aux (selecionar_pecas (criar_tabuleiro notacao_de_forsyth)) (criar_tabuleiro notacao_de_forsyth)

esta_em_xeque_aux :: [(Char, (Int, Int))] -> [[Char]] -> Bool
esta_em_xeque_aux [] _ = False
esta_em_xeque_aux ((peca, (x, y)):restante) tabuleiro
  | peca == 't' = torre (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'c' = cavalo (x,y) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'b' = bispo (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'd' = rainha (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'r' = rei (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | peca == 'p' = peao (peca, (x, y)) tabuleiro || esta_em_xeque_aux restante tabuleiro
  | otherwise = esta_em_xeque_aux restante tabuleiro