{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}

module Graph (
               Vertex(..)
             , neighbors
             , Graph(..)
             , size
             , toVector
             , findWord
             , Path
             , findPath
             ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Set (Set)
import qualified Data.Set as S

import Data.Vector (Vector)
import qualified Data.Vector as V

-- Cada palavra é representada como um vértice de um grafo
data Vertex = Vertex {
  idf             :: !Int, -- ID da palavra
  word            :: !Text,
  neighborIndices :: Set Int -- Cjto de IDs das palavras vizinhas
}

-- Grafo é representado como um vetor de vértices (para indexação
-- O(1)). Os vértices são armazenados no vetor em ordem (veja a
-- instância de Ord Vertex abaixo).
newtype Graph = Graph (Vector Vertex)

-- Um caminho nada mais é do que uma lista de vertices
type Path = [Vertex]


-- Define uma instância de Eq para Vertex onde dois Vertex são iguais se:
-- os seus IDs são iguais, exceto se qualquer um for == -1
-- ou se as palavras que contém são iguais
instance Eq Vertex where
  v0 == v1
    | idf v0 /= -1 && idf v1 /= -1 = idf v0 == idf v1
    | otherwise = word v0 == word v1 

-- A ordem de dois vértices é determinada pela ordem lexicográfica
-- das palavras que contêm
instance Ord Vertex where
  (Vertex _ w0 _ ) <= (Vertex _ w1 _) = w0 <= w1

-- Devolve o vetor contendo os vértices de um grafo
toVector :: Graph -> Vector Vertex
toVector (Graph v) = v

-- Devolve o vértice de ID i
(!) :: Graph -> Int -> Vertex
g ! i = toVector g V.! i
-- Devolve a lista de vizinhos de um vértice
neighbors :: Graph -> Vertex -> Set Vertex
neighbors g v = S.map (g !) (neighborIndices v) 

-- Devolve o número de vértices em um grafo
size :: Graph -> Int
size g = V.length $ toVector g

-- Dado um grafo e uma palavra, devolve o índice do nó do grafo que
-- contém tal palavra, ou -1 caso não esteja presente.
-- Assume que as palavras estão armazenadas em ordem lexicográfica.
wordIntBinSearch :: Graph -> Text -> Int
wordIntBinSearch g w = binSearch 0 (size g)
  where
    binSearch:: Int -> Int -> Int
    binSearch l r
     | l == r = -1
     | w == wmid = mid
     | w < wmid = binSearch l mid
     | otherwise = binSearch (mid + 1) r
     where 
       mid = (l + r) `div` 2
       wmid = word (g ! mid)
-- Dado um grafo e uma palavra, devolve Maybe Vertex, caso exista,
-- que contém a palavra procurada utilizando busca binária
wordBinSearch ::  Graph -> Text -> Maybe Vertex
wordBinSearch g w
  | i == -1 = Nothing
  | otherwise = Just $ g ! i
    where
      i = wordIntBinSearch g w

-- Busca uma palavra no grafo passado como parâmetro
findWord :: Graph -> Text -> Maybe Vertex
findWord g w = wordBinSearch g (T.toUpper w)


-- Dado um grafo e duas palavras, encontra um caminho entre elas onde
-- cada passo tem distância de no máximo 1
findPath :: Graph -> Vertex -> Vertex -> Maybe Path
findPath = bfs

-- Efetua uma busca em largura no grafo. Dadas duas palavras (origem e
-- destino) devolve um caminho entre as palavras caso ele exista.
bfs :: Graph -> Vertex -> Vertex -> Maybe Path
bfs g v0 v1 =
  map (g !) <$> mpath
  where
    s = idf v0
    d = idf v1
    mpath = bfs' [(s, [])] (S.singleton s)
    -- Fila (vertice, backtrack) -> Ver. Vistos
    bfs' :: [(Int, [Int])] -> Set Int -> Maybe [Int]
    bfs' [] _ = Nothing
    bfs' ((v, path):qs) !seen
      | d == v = Just $ reverse (d:path)
      | otherwise =
        bfs' nqueue nseen
        where
          fneigs = neighborIndices $ g ! v
          neigs = fneigs S.\\ seen
          nqueue = qs ++ map (, v:path) (S.toList neigs)
          nseen = S.union seen neigs
