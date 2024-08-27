{-# LANGUAGE OverloadedStrings #-}

module Utils (
    toContinue, 
    printTextScreen, 
    drawTextScreen
) where

-- Continua o fluxo de execução
toContinue :: IO ()
toContinue = do
    putStrLn "\n📚 Pressione ENTER para continuar. 📚"
    line <- getLine :: IO String
    return()

-- Exibe textos na tela
printTextScreen :: [String] -> IO ()
printTextScreen texts = do
  mapM_ putStrLn texts
  toContinue

-- Concatena listas de strings na tela
drawTextScreen :: [String] -> [String]
drawTextScreen texts = texts
