{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graph
import GraphLoader (loadFrom)

import Control.Monad (join)

import Data.Text (Text)
import qualified Data.Text as T

import System.IO ( hFlush, stdout )


-- Dado um caminho, gera um texto amigável a ser mostrado para o usuário
pathToText :: Path -> Text
pathToText [] = "Path not found"
pathToText path =
  T.intercalate " -> " mPathWs
  where
    mPathWs = map word path


repl :: Graph -> IO ()
repl grafo = do
  origem <- prompt "Origem: "
  let mOrigem = findWord grafo $ T.pack origem
  destino <- prompt "Destino: "
  let mDestino =  findWord grafo $ T.pack destino
  putStrLn $ maybe
      "Caminho não encontrado"
      (T.unpack . pathToText)
      $ join $ findPath grafo <$> mOrigem <*> mDestino
  repl grafo

prompt :: String -> IO String
prompt what = do
    putStr what
    hFlush stdout
    getLine

main :: IO()
main = do
  fname <- prompt "Digite o nome do arquivo dicionario: "
  grafo <- loadFrom fname
  repl grafo

-- Exemplos:
--   aura ausente
--   asa assalto
--   tomate coelho
