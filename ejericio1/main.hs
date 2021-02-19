cargar :: IO String
cargar = do
  putStrLn "Introduce el archivo a leer."
  nombreArchivo <- getLine
  archivo <- readFile nombreArchivo
  putStrLn archivo
  return archivo

main::IO String
main = cargar