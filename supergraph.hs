{-# LANGUAGE NoMonomorphismRestriction,
			 DeriveDataTypeable #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import Data.Typeable

import qualified Data.Map as Map

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Debug.Trace

import Data.List (nub, sort)
import Data.Maybe (fromJust)

-- Main routine:

main = mainWith mainDiagram

mainDiagram :: FilePath -> IO (Diagram B R2)
mainDiagram inputFile = do
	contents <- readFile inputFile
	let weightedEdges = (\(Right x) -> x) $ parse supergraph "" contents
	let sizeMap = foldl updateWithEdge Map.empty weightedEdges
	let leftClusters = getClusters L $ weightedEdges
	let rightClusters = getClusters R $ weightedEdges
	let allClusters = leftClusters ++ rightClusters
	let clusterScaling = fromIntegral $ clusterFactor * (averageClusterSize allClusters weightedEdges)
	let leftClusterStacking = clusterStacking clusterScaling L leftClusters sizeMap
	let rightClusterStacking = clusterStacking clusterScaling R rightClusters sizeMap
	let bipartiteClusterGraph = hcat [leftClusterStacking, hSpace 20, rightClusterStacking]
	return $ addEdges clusterScaling allClusters weightedEdges bipartiteClusterGraph

-- Modifyable parameters:

colors = cycle [green, purple, red, blue, orange]

clusterFactor = 4

-- Diagramming:

data Cluster = Cluster { positioning :: LeftRight, clusterId :: Int }
				deriving (Eq, Show, Ord, Typeable)

data LeftRight = L | R
				deriving (Eq, Show, Ord, Typeable)

instance IsName Cluster

averageClusterSize :: [Cluster] -> [(Int, Int, Int)] -> Int
averageClusterSize clusters edges = sumEdges `div` numClusters
	where
		sumEdges = sum . map (\(x, y, z) -> z) $ edges
		numClusters = length clusters

getClusters :: LeftRight -> [(Int, Int, Int)] -> [Cluster]
getClusters p = map (Cluster p) . sort . nub . map f
	where
		f (i, j, w) = case p of
			L -> i
			R -> j

updateWithEdge :: Map.Map Cluster Int -> (Int, Int, Int) -> Map.Map Cluster Int
updateWithEdge sizeMap (i, j, w) = Map.insert clusterI (sizeI + w) . Map.insert clusterJ (sizeJ + w) $ sizeMap
	where
		clusterI = Cluster L i
		clusterJ = Cluster R j
		maybeToInt Nothing = 0
		maybeToInt (Just x) = x
		sizeI = maybeToInt . Map.lookup clusterI $ sizeMap
		sizeJ = maybeToInt . Map.lookup clusterJ $ sizeMap

--clusterBox :: Int -> LeftRight -> Map.Map Cluster Int -> Cluster -> Diagram B R2
clusterBox clusterScaling p sizeMap cluster = hcat $ f [text (show . clusterId $ cluster) # fontSize (Local 0.6) # fc black
										<> square 1 # lw none # scaleX 1.5,
										square 1 # fc (colors !! clusterId cluster) # lw none # named cluster # scaleY size]
	where
		f = case p of
			L -> id
			R -> reverse
		size = (fromIntegral . fromJust . Map.lookup cluster $ sizeMap) / clusterScaling

--clusterStacking :: Int -> LeftRight -> [Cluster] -> Map.Map Cluster Int -> Diagram B R2
clusterStacking clusterScaling p clusters sizeMap = vcat . map (clusterBox clusterScaling p sizeMap) $ clusters

hSpace :: Double -> Diagram B R2
hSpace n = square 1 # scaleX n # lw none

--addEdges :: Int -> [Cluster] -> [(Int, Int, Int)] -> Diagram B R2 -> Diagram B R2
addEdges clusterScaling clusters edges graph = graph <> mconcat (evalState connections initialState)
	where
		initialState = Map.fromList $ do
			cluster <- clusters
			return . getCorner cluster . fromJust . lookupName cluster $ graph
		getCorner (Cluster p i) box = (Cluster p i, case p of
			L -> l .+^ (up ^+^ right)
			R -> l .+^ (up ^+^ left))
			where
				l = location box
				up = fromJust $ rayTraceV l unitY box
				left = fromJust $ rayTraceV l unit_X box
				right = fromJust $ rayTraceV l unitX box
		connections = sequence . map connection $ edges
		connection (i, j, w) = do
			Just pi <- gets (Map.lookup (Cluster L i))
			Just pj <- gets (Map.lookup (Cluster R j))
			let displacement = r2 (0, - (fromIntegral w) / clusterScaling)
			let displaced_pi = pi .+^ displacement
			let displaced_pj = pj .+^ displacement
			modify (Map.insert (Cluster L i) displaced_pi)
			modify (Map.insert (Cluster R j) displaced_pj)
			let connectorTrail = fromVertices [pi, displaced_pi, displaced_pj, pj]
			return . translate (pi .-. origin) . opacity 0.6 . lw none . fc (colors !! i) . strokeLoop . closeLine $ connectorTrail

-- Parsing:

linesOf :: Parser a -> Parser [a]
linesOf p = many $ do
	x <- p
	newline
	return x

supergraph :: Parser [(Int, Int, Int)]
supergraph = do
	edges <- linesOf csvEdge
	eof
	return edges

csvEdge :: Parser (Int, Int, Int)
csvEdge = do
	string "link"
	space
	node1 <- many1 digit
	space
	node2 <- many1 digit
	space
	weight <- many1 digit
	return (read node1 :: Int, read node2 :: Int, read weight :: Int)
