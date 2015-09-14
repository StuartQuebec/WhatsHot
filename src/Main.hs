module Main where

import File
import qualified Data.ByteString as B
import qualified Data.Map as Map

import Data.List
import Moo.GeneticAlgorithm.Binary
import Moo.GeneticAlgorithm.Constraints
import Moo.GeneticAlgorithm.Multiobjective
import Data.Attoparsec.ByteString
popsize = 10
generations = 10
genomeSize = 10
numberArticles = 5

-- REWRITE the type PageCount on top level with Data.Map

wikiCluster :: [Outlinks] -> [PageCount] -> MultiObjectiveProblem ([Bool] -> Double)
wikiCluster links c = [(Maximizing,\x -> linksPerNode links (decode x)),(Maximizing, \x -> visitsPerNode c (decode x))]

decode :: [Bool] -> [Int]
decode genome = map (decodeBinary (1,numberArticles)) (splitEvery  (bitsNeeded (1,numberArticles)) genome)

getLinkedNodes' :: [Outlinks] -> Int -> [Int]
getLinkedNodes' links id = case Map.lookup id (Map.fromList links) of
                            Just a -> a
                            Nothing -> []

getLinkedNodes :: [Outlinks] -> [Int] -> Int -> Int
getLinkedNodes links genome id = length (intersect (getLinkedNodes' links id) genome)

linksPerNode :: [Outlinks] -> [Int] -> Double
linksPerNode links genome = fromIntegral (foldr (+) 0 (map (getLinkedNodes links genome) genome))
                                /fromIntegral (length genome)

getPageCount :: [PageCount] -> Int -> Int
getPageCount c id = case Map.lookup id (Map.fromList c) of
                        Just a -> a
                        Nothing -> 0

visitsPerNode :: [PageCount] -> [Int] -> Double
visitsPerNode c genome = fromIntegral (foldr (+) 0 (map (getPageCount c) genome))
                    /fromIntegral (length genome)

initialize = getRandomBinaryGenomes popsize (genomeSize * (bitsNeeded (1,numberArticles)))

step :: [Outlinks] -> [PageCount] -> StepGA Rand Bool
step l p = stepNSGA2bt (wikiCluster l p) (onePointCrossover 0.1) (pointMutate 0.01)

loadNames :: IO [B.ByteString]
loadNames = do
    namesFile <- B.readFile "names"
    case (parseOnly namesParser namesFile) of
           Right a -> return a
           Left a -> error "Error wih names file" >> return []

loadLinks :: IO [Outlinks]
loadLinks = do
    linksFile <- B.readFile "links"
    case (parseOnly linksParser linksFile) of
           Right a -> return a
           Left a -> error "Error wih links file" >> return []

loadCounts :: IO [(B.ByteString,Int)]
loadCounts = do
    countsFile <- B.readFile "counts"
    case (parseOnly pageCountParser countsFile) of
           Right a -> return a
           Left a -> error "Error wih counts file" >> return []

main = do
  links <- loadLinks
  names <- loadNames
  counts <- loadCounts

  result <- runGA initialize (loop (Generations generations)
            (step links (idPageCountList counts names)))
  let solutions = map takeGenome 
                    $ Data.List.takeWhile ((<= 10.0) . takeObjectiveValue) result
  print $ map decode solutions
  let ovs = map takeObjectiveValues $ evalAllObjectives
            (wikiCluster links (idPageCountList counts names)) solutions
  flip mapM_ ovs $ \[x1,x2] ->
      putStrLn $ show x1 ++ "\t" ++ show x2

  print "Have a nice day"