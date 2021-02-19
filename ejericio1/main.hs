cargar :: IO [[Int]]
cargar = do
  putStrLn "Introduce el archivo a leer."
  nombreArchivo <- getLine
  archivo <- readFile nombreArchivo
  putStrLn "Contenido del archivo:"
  putStrLn $ archivo 
  putStrLn "Archivo deserializado:"
  putStrLn . show $ deserializar archivo
  cargar

deserializar :: String -> [[Int]]
deserializar archivo = map (map readInt) (map words (lines archivo))
  where
    readInt = read :: String -> Int

main::IO [[Int]]
main = cargar