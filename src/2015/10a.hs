import Data.List

look = concatMap ((++) <$> show . length <*> pure . head) . group
main = getLine >>= print . length . (!! 40) . iterate look
