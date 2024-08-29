{-# LANGUAGE OverloadedStrings #-}

module Database (
    connectDB,
    createTables,
    insertGeneralUser,
    insertAdminUser,
    insertAcervo,
    insertAutor,
    insertEmprestimo,
    getGeneralUsers,
    getAdminUsers,
    getAcervo,
    getAutores,
    getEmprestimos,
    updateGeneralUser,
    updateAdminUser,
    updateAcervo,
    updateAutor,
    updateEmprestimo,
    deleteGeneralUser,
    deleteAdminUser,
    deleteAcervo,
    deleteAutor,
    deleteBorrowing
) where

import Database.PostgreSQL.Simple
import Control.Exception (catch, SomeException)
import Control.Monad (void)

-- Função para criar uma conexão com o banco de dados
connectDB :: IO Connection
connectDB = connect defaultConnectInfo
    { connectDatabase = "postgres"
    , connectUser = "postgres"
    , connectPassword = "hasbibliolog"
    , connectHost = "localhost"
    , connectPort = 5432
    }

-- Função para criar as tabelas necessárias
createTables :: Connection -> IO ()
createTables connect = do
    void $ execute_ connect "CREATE TABLE IF NOT EXISTS generalUsers \
                 \(matricula SERIAL PRIMARY KEY, \
                 \primeiroNome VARCHAR(50) NOT NULL, \
                 \ultimoNome VARCHAR(50) NOT NULL, \
                 \dataNascimento DATE NOT NULL, \
                 \email VARCHAR(100) NOT NULL UNIQUE)"
    void $ execute_ connect "CREATE TABLE IF NOT EXISTS adminUsers \
                 \(matricula SERIAL PRIMARY KEY, \
                 \primeiroNome VARCHAR(50) NOT NULL, \
                 \ultimoNome VARCHAR(50) NOT NULL, \
                 \dataNascimento DATE NOT NULL, \
                 \email VARCHAR(100) NOT NULL UNIQUE)"
    void $ execute_ connect "CREATE TABLE IF NOT EXISTS autores \
                 \(id SERIAL PRIMARY KEY, \
                 \nomeCompleto VARCHAR(100) NOT NULL)"
    void $ execute_ connect "CREATE TABLE IF NOT EXISTS acervo \
                 \(id SERIAL PRIMARY KEY, \
                 \nomeCompletoLivro INT REFERENCES autores(id), \
                 \codigoPublicacao VARCHAR(20) NOT NULL, \
                 \anoPublicacao INT NOT NULL, \
                 \editora VARCHAR(50) NOT NULL, \
                 \paisPublicacao VARCHAR(50) NOT NULL, \
                 \generoLiterario VARCHAR(50) NOT NULL)"
    void $ execute_ connect "CREATE TABLE IF NOT EXISTS emprestimo \
                 \(idEmprestimo SERIAL PRIMARY KEY, \
                 \usuario INT REFERENCES generalUsers(matricula), \
                 \livro INT REFERENCES acervo(id), \
                 \dataEmprestimo DATE NOT NULL, \
                 \dataDevolucao DATE NOT NULL, \
                 \status VARCHAR(10) CHECK (status IN ('OK', 'ATRASADO', 'RENOVADO')) NOT NULL)"

    putStrLn "Tabelas criadas com sucesso."

-- Funções para inserir dados
insertGeneralUser :: Connection -> String -> String -> String -> String -> IO ()
insertGeneralUser connect primeiroNome ultimoNome dataNascimento email = do
    void $ execute connect "INSERT INTO generalUsers (primeiroNome, ultimoNome, dataNascimento, email) VALUES (?, ?, ?, ?)" (primeiroNome, ultimoNome, dataNascimento, email)
    putStrLn "Usuário geral inserido com sucesso."

insertAdminUser :: Connection -> String -> String -> String -> String -> IO ()
insertAdminUser connect primeiroNome ultimoNome dataNascimento email = do
    void $ execute connect "INSERT INTO adminUsers (primeiroNome, ultimoNome, dataNascimento, email) VALUES (?, ?, ?, ?)" (primeiroNome, ultimoNome, dataNascimento, email)
    putStrLn "Usuário administrador inserido com sucesso."

insertAutor :: Connection -> String -> IO ()
insertAutor connect nomeCompleto = do
    void $ execute connect "INSERT INTO autores (nomeCompleto) VALUES (?)" (Only nomeCompleto)
    putStrLn "Autor inserido com sucesso."

insertAcervo :: Connection -> Int -> String -> Int -> String -> String -> String -> IO ()
insertAcervo connect nomeCompletoLivro codigoPublicacao anoPublicacao editora paisPublicacao generoLiterario = do
    void $ execute connect "INSERT INTO acervo (nomeCompletoLivro, codigoPublicacao, anoPublicacao, editora, paisPublicacao, generoLiterario) VALUES (?, ?, ?, ?, ?, ?)" (nomeCompletoLivro, codigoPublicacao, anoPublicacao, editora, paisPublicacao, generoLiterario)
    putStrLn "Item do acervo inserido com sucesso."

insertEmprestimo :: Connection -> Int -> Int -> String -> String -> String -> IO ()
insertEmprestimo connect usuario livro dataEmprestimo dataDevolucao status = do
    void $ execute connect "INSERT INTO emprestimo (usuario, livro, dataEmprestimo, dataDevolucao, status) VALUES (?, ?, ?, ?, ?)" (usuario, livro, dataEmprestimo, dataDevolucao, status)
    putStrLn "Empréstimo registrado com sucesso."

-- Funções para obter dados
getGeneralUsers :: Connection -> IO [(String, String, String, Int, String)]
getGeneralUsers connect = query_ connect "SELECT primeiroNome, ultimoNome, dataNascimento, matricula, email FROM generalUsers"

getAdminUsers :: Connection -> IO [(String, String, String, Int, String)]
getAdminUsers connect = query_ connect "SELECT primeiroNome, ultimoNome, dataNascimento, matricula, email FROM adminUsers"

getAutores :: Connection -> IO [(Int, String)]
getAutores connect = query_ connect "SELECT id, nomeCompleto FROM autores"

getAcervo :: Connection -> IO [(Int, Int, String, Int, String, String, String)]
getAcervo connect = query_ connect "SELECT id, nomeCompletoLivro, codigoPublicacao, anoPublicacao, editora, paisPublicacao, generoLiterario FROM acervo"

getEmprestimos :: Connection -> IO [(Int, Int, Int, String, String, String)]
getEmprestimos connect = query_ connect "SELECT idEmprestimo, usuario, livro, dataEmprestimo, dataDevolucao, status FROM emprestimo"

-- Funções para atualizar dados
updateGeneralUser :: Connection -> Int -> String -> String -> String -> String -> IO ()
updateGeneralUser connect matricula primeiroNome ultimoNome dataNascimento email = do
    void $ execute connect "UPDATE generalUsers SET primeiroNome = ?, ultimoNome = ?, dataNascimento = ?, email = ? WHERE matricula = ?" (primeiroNome, ultimoNome, dataNascimento, email, matricula)
    putStrLn $ "Usuário geral com matrícula " ++ show matricula ++ " atualizado."

updateAdminUser :: Connection -> Int -> String -> String -> String -> String -> IO ()
updateAdminUser connect matricula primeiroNome ultimoNome dataNascimento email = do
    void $ execute connect "UPDATE adminUsers SET primeiroNome = ?, ultimoNome = ?, dataNascimento = ?, email = ? WHERE matricula = ?" (primeiroNome, ultimoNome, dataNascimento, email, matricula)
    putStrLn $ "Usuário administrador com matrícula " ++ show matricula ++ " atualizado."

updateAutor :: Connection -> Int -> String -> IO ()
updateAutor connect id nomeCompleto = do
    void $ execute connect "UPDATE autores SET nomeCompleto = ? WHERE id = ?" (nomeCompleto, id)
    putStrLn $ "Autor com ID " ++ show id ++ " atualizado."

updateAcervo :: Connection -> Int -> Int -> String -> Int -> String -> String -> String -> IO ()
updateAcervo connect id nomeCompletoLivro codigoPublicacao anoPublicacao editora paisPublicacao generoLiterario = do
    void $ execute connect "UPDATE acervo SET nomeCompletoLivro = ?, codigoPublicacao = ?, anoPublicacao = ?, editora = ?, paisPublicacao = ?, generoLiterario = ? WHERE id = ?" (nomeCompletoLivro, codigoPublicacao, anoPublicacao, editora, paisPublicacao, generoLiterario, id)
    putStrLn $ "Item do acervo com ID " ++ show id ++ " atualizado."

updateEmprestimo :: Connection -> Int -> Int -> Int -> String -> String -> String -> IO ()
updateEmprestimo connect idEmprestimo usuario livro dataEmprestimo dataDevolucao status = do
    void $ execute connect "UPDATE emprestimo SET usuario = ?, livro = ?, dataEmprestimo = ?, dataDevolucao = ?, status = ? WHERE idEmprestimo = ?" (usuario, livro, dataEmprestimo, dataDevolucao, status, idEmprestimo)
    putStrLn $ "Empréstimo com ID " ++ show idEmprestimo ++ " atualizado."

-- Funções para deletar dados
deleteGeneralUser :: Connection -> Int -> IO ()
deleteGeneralUser connect matricula = do
    void $ execute connect "DELETE FROM generalUsers WHERE matricula = ?" (Only matricula)
    putStrLn $ "Usuário geral com matrícula " ++ show matricula ++ " deletado."

deleteAdminUser :: Connection -> Int -> IO ()
deleteAdminUser connect matricula = do
    void $ execute connect "DELETE FROM adminUsers WHERE matricula = ?" (Only matricula)
    putStrLn $ "Usuário administrador com matrícula " ++ show matricula ++ " deletado."

deleteAutor :: Connection -> Int -> IO ()
deleteAutor connect id = do
    void $ execute connect "DELETE FROM autores WHERE id = ?" (Only id)
    putStrLn $ "Autor com ID " ++ show id ++ " deletado."

deleteAcervo :: Connection -> Int -> IO ()
deleteAcervo connect id = do
    void $ execute connect "DELETE FROM acervo WHERE id = ?" (Only id)
    putStrLn $ "Item do acervo com ID " ++ show id ++ " deletado."

deleteBorrowing :: Connection -> Int -> IO ()
deleteBorrowing connect idEmprestimo = do
    void $ execute connect "DELETE FROM emprestimo WHERE idEmprestimo = ?" (Only idEmprestimo)
    putStrLn $ "Empréstimo com ID " ++ show idEmprestimo ++ " deletado."
