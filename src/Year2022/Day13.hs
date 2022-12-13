{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Year2022.Day13 where

import Data.List
import System.Process
import System.FilePath
import System.IO
import System.IO.Temp
import GHC.Exts

data Tree = Tip Int | Tree [Tree] deriving (Show, Eq)

instance Num Tree where
  fromInteger = Tip . fromInteger
instance IsList Tree where
  type Item Tree = Tree
  fromList = Tree . fromList

instance Ord Tree where
  compare (Tip x) (Tip y)     = compare x y
  compare (Tree xs) (Tree ys) = compare xs ys
  compare x@Tip{} (Tree ys)   = compare [x] ys
  compare (Tree xs) y@Tip{}   = compare xs [y]

part1, part2 :: ([String], String)
part1 = (["Data.List", "Data.List.Split", "Util"],
  "sum . map succ . findIndices (uncurry' (<)) . chunksOf 2")
part2 = (["Data.List", "Safe"],
  "((*) <$> ($ [[2]]) <*> ($ [[6]])) . " ++
  "(succ .) . flip elemIndexJust . sort . ([[[2]],[[6]]] ++)")

main = do
  input <- readFile (replaceExtension __FILE__ ".in")
  let (imports, code) = part2
  withTempFile "." "temp.hs" $ \file handle -> do
    mapM_ (hPutStrLn handle) $
      map ("import " ++) ("Year2022.Day13 hiding (main)" : imports) ++
      [ "main = print . " ++ code ++ " $ trees" , "trees :: [Tree]" , "trees = ["
      , intercalate ",\n" . map (' ':) . filter (not . null) $ lines input , " ]" ]
    hFlush handle >> hClose handle
    callProcess "runghc" ["-XOverloadedLists",
      "-i" ++ takeDirectory (takeDirectory __FILE__), file]

