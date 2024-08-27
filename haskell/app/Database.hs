{-# LANGUAGE OverloadedStrings #-}

module Database (
    connectDB,
    createTable,
    insertItem,
    getItems,
    updateItem,
    deleteItem
) where

import Database.PostgreSQL.Simple
import Control.Exception (bracket, catch, SomeException)

-- Função para criar uma conexão com o banco de dados
connectDB :: IO Connection
connectDB = connect defaultConnectInfo
    { connectDatabase = "postgres"
    , connectUser = "postgres"
    , connectPassword = "hasbibliolog"
    , connectHost = "localhost"
    , connectPort = 5432
    }

-- Função para criar a tabela
createTable :: Connection -> IO ()
createTable connect = do
    execute_ connect "CREATE TABLE IF NOT EXISTS items (id SERIAL PRIMARY KEY, name TEXT NOT NULL)"
    putStrLn "Tabela 'items' criada com sucesso."

-- Função para inserir um item
insertItem :: Connection -> String -> IO ()
insertItem connect name = do
    execute connect "INSERT INTO items (name) VALUES (?)" (Only name)
    putStrLn $ "Item inserido: " ++ name

-- Função para obter todos os itens
getItems :: Connection -> IO [String]
getItems connect = do
    rows <- query_ connect "SELECT name FROM items" :: IO [Only String]
    return $ map fromOnly rows

-- Função para atualizar um item
updateItem :: Connection -> Int -> String -> IO ()
updateItem connect itemId newName = do
    execute connect "UPDATE items SET name = ? WHERE id = ?" (newName, itemId)
    putStrLn $ "Item com ID " ++ show itemId ++ " atualizado para: " ++ newName

-- Função para deletar um item
deleteItem :: Connection -> Int -> IO ()
deleteItem connect itemId = do
    execute connect "DELETE FROM items WHERE id = ?" (Only itemId)
    putStrLn $ "Item com ID " ++ show itemId ++ " deletado."
