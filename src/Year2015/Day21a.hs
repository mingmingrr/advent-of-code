import Data.List
import Data.Ord

data Equipment = Equipment { cost :: Int, damage :: Int, armor :: Int }
  deriving (Show)

instance Semigroup Equipment where
  Equipment c d a <> Equipment c' d' a' = Equipment (c + c') (d + d') (a + a')

instance Monoid Equipment where
  mempty = Equipment 0 0 0

equipments = sortBy (comparing cost)
           $ (\a b c -> a <> b <> c)
         <$> [ Equipment  8 4 0
             , Equipment 10 5 0
             , Equipment 25 6 0
             , Equipment 40 7 0
             , Equipment 74 8 0
             ]
         <*> [ Equipment  13 0 1
             , Equipment  31 0 2
             , Equipment  53 0 3
             , Equipment  75 0 4
             , Equipment 102 0 5
             , mempty
             ]
         <*> ( map (\[a, b] -> a <> b)
             . filter ((==2) . length)
             . subsequences
             $ [ Equipment  25 1 0
               , Equipment  50 2 0
               , Equipment 100 3 0
               , Equipment  20 0 1
               , Equipment  40 0 2
               , Equipment  80 0 3
               , mempty
               , mempty
               ]
             )

winning boss@[h, d, a] self@(Equipment _ d' a') =
  let t = 99 `div` max 1 (d - a')
      t' = (h - 1) `div` max 1 (d' - a)
   in t >= t'

main = interact $ show . cost . head
                . flip filter equipments
                . winning
                . map (read . last . words)
                . lines

