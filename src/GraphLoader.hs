module GraphLoader (loadFrom)  where

import Graph

import Data.Maybe (mapMaybe)
import Data.List (sort)
import Data.Text.Metrics (levenshtein)
import System.Directory (doesFileExist)

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Carrega um conjunto de palavras de um dicionário e gera um grafo
-- onde palavras com distância 1 (levenstein) tem uma aresta entre
-- elas
loadPristineSetOfWords :: FilePath -> IO Graph
loadPristineSetOfWords fp = do
  g0 <- startUp fp
  return $ Graph (initGraph g0)
  where
    startUp fname = do
      fContents <- readFile fname -- uma palavra por linha
      return $ V.fromList $ zipWith l2t [0..] (sort $ lines fContents)
      where
        l2t i w = Vertex i (T.toUpper $ T.pack w) S.empty
    -- Como calcular a distância de levenshtein é caro, só calcula se
    -- tiver chance de ser 1
    conexo (Vertex _ p1 _) (Vertex _ p2 _) =
      abs (T.length p1 - T.length p2) < 2 && levenshtein p1 p2 == 1
    -- Dá para acelerar o processo se ordenarmos as palavras por
    -- tamanho e parar quando a diferença entre os tamanhos for > 1
    -- (em vez de ficar testando as diferenças de tamanho a cada
    -- comparação)
    initGraph g =
      V.map (\v@(Vertex i p _) -> Vertex i p (edges v)) g
      where
        edges v = S.fromList $ V.toList (V.map idf (V.filter (conexo v) g))

-- Carrega um grafo de palavras a partir de um arquivo pré-calculado
loadPreCalculatedGraph :: FilePath -> IO Graph
loadPreCalculatedGraph fname = do
  fContents <- readFile fname
  let fLines = lines fContents
      g =  Graph $ V.fromList $ zipWith (buildV g) [0..] (tail fLines)
  return g
  where
    buildV g i ln =
      case map T.pack (words ln) of
        (x:xs) ->
          Vertex i x (S.fromList $ mapMaybe (fmap idf . findWord g) xs)
        [] -> error "Should not be here"

-- Salva um grafo para um arquivo de cache para acelerar execuções
-- subsequentes do mesmo dicionário
saveCache :: FilePath -> Graph -> IO ()
saveCache fp g =
  writeFile fp dump
  where
    vToStr v = unwords $ T.unpack (word v) : map (T.unpack . word) (S.toList $ neighbors g v)
    dump = unlines $ show (size g) : map vToStr (V.toList $ toVector g)

-- Carrega um arquivo de dicionário e gera o grafo das palavras. Se
-- houver um arquivo de cache com o grafo pré-calculado o
-- carrega. Caso contrário, calcula o grafo e salva o cache antes de
-- devolver o grafo.
loadFrom :: FilePath -> IO Graph
loadFrom fp = do
  putStrLn $ "Reading from: " <> fp
  isCached <- doesFileExist cachefp
  putStr $ "Cached file (" <> cachefp <> ") "
  if isCached
  then do
    putStrLn "found."
    loadPreCalculatedGraph cachefp
  else do
    putStrLn "not found."
    putStrLn "Pre-processing file. This will take a while..."
    g <- loadPristineSetOfWords fp
    putStrLn "Saving cache."
    saveCache cachefp g
    return g
  where
    cachefp = fp <> ".cache"
