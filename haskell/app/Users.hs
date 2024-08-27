{-# LANGUAGE OverloadedStrings #-}

module Users (
    postUser,
    putUser,
    getOneUser,
    getAllUsers,
    deleteUser
) where

import Database (insertItem, getItems, updateItem, deleteItem)
import Database.PostgreSQL.Simple (Connection)
import System.IO (hFlush, stdout)

postUser :: Connection -> IO ()
postUser connect = do
    putStr "Digite o nome do item: "
    hFlush stdout
    name <- getLine
    insertItem connect name

putUser :: Connection -> IO ()
putUser connect = do
    putStr "Digite o ID do item a ser atualizado: "
    hFlush stdout
    idStr <- getLine
    let itemId = read idStr :: Int
    putStr "Digite o novo nome do item: "
    hFlush stdout
    newName <- getLine
    updateItem connect itemId newName

getOneUser :: Connection -> IO ()
getOneUser connect = do
    items <- getItems connect
    putStrLn "Itens no banco de dados:"
    mapM_ putStrLn items

getAllUsers :: Connection -> IO ()
getAllUsers connect = do
    items <- getItems connect
    putStrLn "Itens no banco de dados:"
    mapM_ putStrLn items

deleteUser :: Connection -> IO ()
deleteUser connect = do
    putStr "Digite o ID do item a ser deletado: "
    hFlush stdout
    idStr <- getLine
    let itemId = read idStr :: Int
    deleteItem connect itemId
