{-# LANGUAGE OverloadedStrings #-}

module Borrowing (
    postBorrowing,
    putBorrowing,
    getOneBorrowing,
    getAllBorrowings,
    deleteBorrowing
) where

import Database (insertItem, getItems, updateItem, deleteItem)
import Database.PostgreSQL.Simple (Connection)
import System.IO (hFlush, stdout)

postBorrowing :: Connection -> IO ()
postBorrowing connect = do
    putStr "Digite o nome do item: "
    hFlush stdout
    name <- getLine
    insertItem connect name

putBorrowing :: Connection -> IO ()
putBorrowing connect = do
    putStr "Digite o ID do item a ser atualizado: "
    hFlush stdout
    idStr <- getLine
    let itemId = read idStr :: Int
    putStr "Digite o novo nome do item: "
    hFlush stdout
    newName <- getLine
    updateItem connect itemId newName

getOneBorrowing :: Connection -> IO ()
getOneBorrowing connect = do
    items <- getItems connect
    putStrLn "Itens no banco de dados:"
    mapM_ putStrLn items

getAllBorrowings :: Connection -> IO ()
getAllBorrowings connect = do
    items <- getItems connect
    putStrLn "Itens no banco de dados:"
    mapM_ putStrLn items

deleteBorrowing :: Connection -> IO ()
deleteBorrowing connect = do
    putStr "Digite o ID do item a ser deletado: "
    hFlush stdout
    idStr <- getLine
    let itemId = read idStr :: Int
    deleteItem connect itemId
