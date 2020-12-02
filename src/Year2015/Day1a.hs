parse '(' = 1
parse ')' = (-1)
parse _ = 0

main = getContents >>= print . sum . map parse
