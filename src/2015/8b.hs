quote '"' = "\\\""
quote '\\' = "\\\\"
quote x = [x]

difference = (-) <$> (+2) . length . concatMap quote <*> length

main = interact $ show . sum . map difference . lines

