module Main (main) where

import Lib 

main :: IO ()
main = do
    putStrLn "--- Запуск LamaGraph Isomorphism ---"

    putStrLn "\n[1/2] Чтение первого графа (graph1.json)..."
    maybeGraph1 <- parseGraphFromFile "graph1.json"

    putStrLn "[2/2] Чтение второго графа (graph2.json)..."
    maybeGraph2 <- parseGraphFromFile "graph2.json"

    case (maybeGraph1, maybeGraph2) of
        (Just g1, Just g2) -> do
            putStrLn "\nОба графа успешно загружены в память!"
            
            putStrLn "\n=== Проверка пре-фильтра (Инварианты) ==="
            print $ checkInvariants g1 g2

            putStrLn "\n=== Точный поиск изоморфизма (Бэктрекинг) ==="
            print $ isIsomorphic g1 g2

        (Nothing, _) -> putStrLn "\nОшибка: Не удалось прочитать graph1.json"
        (_, Nothing) -> putStrLn "\nОшибка: Не удалось прочитать graph2.json"
