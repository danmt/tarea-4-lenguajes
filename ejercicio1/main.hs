cargar :: IO [[Int]]
cargar = do
  putStrLn "\nIntroduce el archivo a leer."
  nombreArchivo <- getLine
  archivo <- readFile nombreArchivo
  putStrLn "\nContenido del archivo:"
  putStrLn $ archivo
  putStrLn "\nResultado:"
  putStrLn . show . sumarColumnas $ obtenerMatriz archivo
  cargar

sumarColumnas :: [[Int]] -> [Int]
sumarColumnas filas = foldl1 (zipWith (+)) filas

obtenerMatriz :: String -> [[Int]]
obtenerMatriz archivo = obtenerMinimo $ map (map readInt) (map words (lines archivo))
  where
    readInt = read :: String -> Int
    obtenerMinimo filas = map (take (minimum $ map length filas)) filas


main::IO [[Int]]
main = cargar
