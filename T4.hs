-- ===========================================
-- Seção 1 – Pattern Matching
-- ===========================================

-- 1. Função 'inicioLista' que retorna o primeiro elemento de uma lista
inicioLista :: [a] -> a
inicioLista (x:_) = x

-- 2. Função 'segundoElemento' que retorna o segundo elemento de uma lista
segundoElemento :: [a] -> a
segundoElemento (_:x:_) = x

-- 3. Função 'ultimoElemento' que retorna o último elemento de uma lista
ultimoElemento :: [a] -> a
ultimoElemento [x] = x
ultimoElemento (_:xs) = ultimoElemento xs

-- 4. Função 'soUm' que retorna True se a lista tiver exatamente um elemento
soUm :: [a] -> Bool
soUm [_] = True
soUm _ = False

-- 5. Função 'total' que recebe uma tupla de três inteiros e retorna a soma do primeiro e terceiro
total :: (Int, Int, Int) -> Int
total (x, _, z) = x + z

-- ===========================================
-- Seção 2 – Funções Anônimas (Lambdas) e Seções
-- ===========================================

-- 6. Use 'map' com uma função lambda para somar 1 a cada elemento de uma lista
somarUm :: [Int] -> [Int]
somarUm xs = map (\x -> x + 1) xs

-- 7. Use 'filter' com uma função lambda para filtrar números negativos de uma lista
filtrarNegativos :: [Int] -> [Int]
filtrarNegativos xs = filter (\x -> x < 0) xs

-- 8. Use uma seção para dobrar os números de uma lista com 'map (*2)'
dobrarNumeros :: [Int] -> [Int]
dobrarNumeros xs = map (*2) xs

-- 9. Use 'map' com uma seção '(+3)' para somar 3 a cada número de uma lista
somarTres :: [Int] -> [Int]
somarTres xs = map (+3) xs

-- 10. Implemente uma função que recebe uma seção e um inteiro e retorna a aplicação da seção sobre o inteiro
aplicarSecao :: (Int -> Int) -> Int -> Int
aplicarSecao secao n = secao n


-- ===========================================
-- Exemplos de uso e testes
-- ===========================================

-- Função auxiliar para testar as funções
testarFuncoes :: IO ()
testarFuncoes = do
    putStrLn "=== Testando Pattern Matching ==="
    putStrLn $ "inicioLista [1,2,3,4]: " ++ show (inicioLista [1,2,3,4])
    putStrLn $ "segundoElemento [1,2,3,4]: " ++ show (segundoElemento [1,2,3,4])
    putStrLn $ "ultimoElemento [1,2,3,4]: " ++ show (ultimoElemento [1,2,3,4])
    putStrLn $ "soUm [5]: " ++ show (soUm [5])
    putStrLn $ "soUm [1,2]: " ++ show (soUm [1,2])
    putStrLn $ "total (10,20,30): " ++ show (total (10,20,30))
    
    putStrLn "\n=== Testando Lambdas e Seções ==="
    putStrLn $ "somarUm [1,2,3,4]: " ++ show (somarUm [1,2,3,4])
    putStrLn $ "filtrarNegativos [-2,-1,0,1,2]: " ++ show (filtrarNegativos [-2,-1,0,1,2])
    putStrLn $ "dobrarNumeros [1,2,3,4]: " ++ show (dobrarNumeros [1,2,3,4])
    putStrLn $ "somarTres [1,2,3,4]: " ++ show (somarTres [1,2,3,4])
    putStrLn $ "aplicarSecao (*5) 3: " ++ show (aplicarSecao (*5) 3)
    putStrLn $ "aplicarSecao (+10) 7: " ++ show (aplicarSecao (+10) 7)