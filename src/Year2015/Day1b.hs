parse '(' = 1
parse ')' = (-1)
parse _ = 0

main = getContents >>= print . (+1) . length . takeWhile (>=0) . scanl1 (+) . map parse
