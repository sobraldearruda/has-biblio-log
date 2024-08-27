{-# LANGUAGE OverloadedStrings #-}

module Archive (
    postBook,
    putBook,
    getOneBook,
    getAllBooks,
    deleteBook
) where

import Database (insertItem, getItems, updateItem, deleteItem)
import Database.PostgreSQL.Simple (Connection)
import System.IO (hFlush, stdout)

postBook :: Connection -> IO ()
postBook connect = do
    putStr "Digite o nome do item: "
    hFlush stdout
    name <- getLine
    insertItem connect name

putBook :: Connection -> IO ()
putBook connect = do
    putStr "Digite o ID do item a ser atualizado: "
    hFlush stdout
    idStr <- getLine
    let itemId = read idStr :: Int
    putStr "Digite o novo nome do item: "
    hFlush stdout
    newName <- getLine
    updateItem connect itemId newName

getOneBook :: Connection -> IO ()
getOneBook connect = do
    items <- getItems connect
    putStrLn "Itens no banco de dados:"
    mapM_ putStrLn items

getAllBooks :: Connection -> IO ()
getAllBooks connect = do
    items <- getItems connect
    putStrLn "Itens no banco de dados:"
    mapM_ putStrLn items

deleteBook :: Connection -> IO ()
deleteBook connect = do
    putStr "Digite o ID do item a ser deletado: "
    hFlush stdout
    idStr <- getLine
    let itemId = read idStr :: Int
    deleteItem connect itemId
