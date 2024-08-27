{-# LANGUAGE OverloadedStrings #-}

module Menu (
    menu,
    menuUsers,
    menuArchive,
    menuBorrowing,
    backToMenu
) where

import Database (insertItem, getItems, updateItem, deleteItem)
import Database.PostgreSQL.Simple (Connection)
import System.IO (hFlush, stdout)
import Users (postUser, putUser, getOneUser, getAllUsers, deleteUser)
import Archive (postBook, putBook, getOneBook, getAllBooks, deleteBook)
import Borrowing (postBorrowing, putBorrowing, getOneBorrowing, getAllBorrowings, deleteBorrowing)

-- Exibe o menu principal
menu :: Connection -> IO ()
menu connect = do
    putStrLn "\nMENU:"
    putStrLn "1. USUÁRIOS"
    putStrLn "2. ACERVO BIBLIOGRÁFICO"
    putStrLn "3. EMPRÉSTIMOS DE LIVROS"
    putStrLn "4. SAIR"
    putStr "\nDigite sua escolha: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> menuUsers connect
        "2" -> menuArchive connect
        "3" -> menuBorrowing connect
        "4" -> do
            putStrLn "\n📚 HasBiblioLog 📚"
            return ()
        _   -> putStrLn "\nOpção inválida. Tente novamente."

-- Exibe o submenu de opções para funções com usuários
menuUsers :: Connection -> IO ()
menuUsers connect = do
    putStrLn "\n1. CADASTRAR USUÁRIO"
    putStrLn "2. ATUALIZAR USUÁRIO"
    putStrLn "3. VISUALIZAR USUÁRIO"
    putStrLn "4. LISTAR USUÁRIOS"
    putStrLn "5. REMOVER USUÁRIO"
    putStrLn "6. RETORNAR AO MENU"
    putStr "\nDigite sua escolha: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> postUser connect
        "2" -> putUser connect
        "3" -> getOneUser connect
        "4" -> getAllUsers connect
        "5" -> deleteUser connect
        "6" -> backToMenu connect
        "7" -> do
            putStrLn "\n📚 HasBiblioLog 📚"
            return ()
        _   -> putStrLn "\nOpção inválida. Tente novamente."

-- Exibe o submenu de opções para o acervo bibliográfico
menuArchive :: Connection -> IO ()
menuArchive connect = do
    putStrLn "\n1. CADASTRAR LIVRO"
    putStrLn "2. ATUALIZAR LIVRO"
    putStrLn "3. VISUALIZAR LIVRO"
    putStrLn "4. LISTAR LIVROS"
    putStrLn "5. REMOVER LIVRO"
    putStrLn "6. RETORNAR AO MENU"
    putStr "\nDigite sua escolha: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> postBook connect
        "2" -> putBook connect
        "3" -> getOneBook connect
        "4" -> getAllBooks connect
        "5" -> deleteBook connect
        "6" -> backToMenu connect
        "7" -> do
            putStrLn "\n📚 HasBiblioLog 📚"
            return ()
        _   -> putStrLn "\nOpção inválida. Tente novamente."

-- Exibe o submenu de opções para empréstimos de livros
menuBorrowing :: Connection -> IO ()
menuBorrowing connect = do
    putStrLn "\n1. CADASTRAR EMPRÉSTIMO"
    putStrLn "2. ATUALIZAR EMPRÉSTIMO"
    putStrLn "3. VISUALIZAR EMPRÉSTIMO"
    putStrLn "4. LISTAR EMPRÉSTIMOS"
    putStrLn "5. REMOVER EMPRÉSTIMO"
    putStrLn "6. RETORNAR AO MENU"
    putStr "\nDigite sua escolha: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> postBorrowing connect
        "2" -> putBorrowing connect
        "3" -> getOneBorrowing connect
        "4" -> getAllBorrowings connect
        "5" -> deleteBorrowing connect
        "6" -> backToMenu connect
        "7" -> do
            putStrLn "\n📚 HasBiblioLog 📚"
            return ()
        _   -> putStrLn "\nOpção inválida. Tente novamente."

-- Retorna ao menu de opções
backToMenu :: Connection -> IO ()
backToMenu connect = do
    menu connect
