{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database (connectDB)
import Menu (menu)

main :: IO ()
main = do
    connect <- connectDB
    putStrLn "\n📚 HasBiblioLog 📚"
    menu connect
