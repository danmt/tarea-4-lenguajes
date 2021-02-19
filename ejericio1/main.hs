cargar :: IO [[Int]]
cargar = do
  putStrLn "Introduce el archivo a leer."
  nombreArchivo <- getLine
  archivo <- readFile nombreArchivo
  putStrLn "Contenido del archivo:"
  putStrLn $ archivo 
  putStrLn "Archivo deserializado:"
  let listaInt = deserializar archivo
  putStrLn . show $ listaInt
  putStrLn "\nResultado:"
  putStrLn . show $ sumarListas $ map (take (minimum $ map length listaInt)) listaInt
  cargar
  where
    sumarListas lista = foldl1 (zipWith (+)) lista
    
deserializar :: String -> [[Int]]
deserializar archivo = map (map readInt) (map words (lines archivo))
  where
    readInt = read :: String -> Int

main::IO [[Int]]
main = cargar
