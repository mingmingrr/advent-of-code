nice as
  | null . filter (uncurry (==)) $ zip as (tail as) = False
  | (< 3) . length $ filter (`elem` "aeiou") as = False
  | not . null . filter (`elem` ["ab", "cd", "pq", "xy"])
    . map (uncurry $ (. return) . (:)) $ zip as (tail as) = False
  | otherwise = True

main = getContents >>= print . length . filter nice . lines
