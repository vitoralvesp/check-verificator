{- TODO 
 - Ler a Notação de Forsyth:
   - Formato da Entrada: [Peças Pretas, Peões das Peças Pretas, Linhas entre as Peças, Peões das Peças Brancas, Peças Brancas]
 - Realizar a leitura da notação e converter em uma matriz
 - Identificar a posição do rei branco na matriz
 - Verificar se alguma peça preta pode atacá-lo
 - Retornar True ou False para os casos
 -}

{-
check_verificator :: [String] -> Bool
check_verificator [] = False
check_verificator lst = True
-}

matrix :: Int -> Int -> [String] -> [[Char]]
matrix x y
     | [x][y] = 

main :: IO()
main = do
       putStr "Executando..."
