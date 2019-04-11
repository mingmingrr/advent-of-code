import Data.List
import Control.Monad

hamm = (sum .) . zipWith ((fromEnum .) . (/=))

main = do
  stuff <- lines <$> readFile "2.in"
  putStrLn . head $ do
    a <- stuff
    b <- stuff
    guard $ hamm a b == 1
    return $ map fst . filter snd . zip a $ zipWith (==) a b

