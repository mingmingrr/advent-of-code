{-# LANGUAGE CPP #-}

module Year2016.Day11 where

import Util
import Safe
import Text.Regex.PCRE
import Data.Function
import Data.List
import Control.Lens
import System.FilePath

type Key = [[Int]]
type Floor = ([String], [String])
newtype Facility = Facility (Key, Int, [Floor])

instance Eq Facility where
  Facility (x,n,_) == Facility (y,m,_) = n == m && x == y
instance Ord Facility where
  compare (Facility (x,n,_)) (Facility (y,m,_)) = compare (n,x) (m,y)

facility :: Int -> [Floor] -> Facility
facility n xs = Facility (mkKey (xs >>= (^.. each)), n, xs) where
  mkKey xs = map (sort . map (`elemIndexJust` xs')) xs where
    xs' = sortOn (\x -> (map (x `elem`) xs, x)) . nub $ concat xs

neigh :: Facility -> [(Int, Facility)]
neigh (Facility (_, elev1, floors)) =
  [ (1, facility elev2 floors')
  | let floor1@(gen1, chip1) = floors !! elev1
  , elev2 <- [elev1-1, elev1+1]
  , Just floor2@(gen2, chip2) <- [floors ^? ix elev2]
  , (floor1'@(gen1', chip1'), floor2'@(gen2', chip2')) <-
    [ ((gen1', chip1), (gen2 ++ move, chip2))
    | (move, gen1') <- take1 gen1 ++ take2 gen1 ] ++ -- move gens
    [ ((gen1, chip1'), (gen2, chip2 ++ move))
    | (move, chip1') <- take1 chip1 ++ take2 chip1 ] ++ -- move chips
    [ ((gen1 \\ move, chip1 \\ move), (gen2 ++ move, chip2 ++ move))
    | move <- map pure (gen1 `intersect` chip1) ] -- move both
  , null gen1' || null (chip1' \\ gen1') , null gen2' || null (chip2' \\ gen2')
  , let floors' = ix elev1 .~ floor1' $ ix elev2 .~ floor2' $ floors
  ] where take1 xs = [ ([b], a<>c) | (a,b,c) <- zipper xs ]
          take2 xs = [ ([b,e], a<>d<>f) | (a,b,c) <- zipper xs ,(d,e,f) <- zipper c ]

part1, part2 :: String
part1 = ""
part2 = "An elerium generator. An elerium-compatible microchip." ++
  "A dilithium generator. A dilithium-compatible microchip."

main = readFile (replaceExtension __FILE__ ".in") >>= \input -> print $ head [ cost
  | let floors = map (((,) `on` sort . map (take 4 . last))
          <$> (=~ "(\\w+) generator") <*> (=~ "(\\w+)-compatible"))
          . lines $ part2 ++ input
  , (cost, Facility (_,_,floors) : _) <- dijkstra neigh (facility 0 floors)
  , and [ null xs && null ys | (xs, ys) <- init floors ] ]

