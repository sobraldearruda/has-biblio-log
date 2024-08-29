{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database (connectDB, createTable)
import Menu (menu)

main :: IO ()
main = do
    connect <- connectDB

    -- Cria a tabela se ainda não existir
    createTables connect

    --putStrLn "\n📚 HasBiblioLog 📚"
    putStrLn "\n HasBiblioLog "
    menu connect
