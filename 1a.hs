main = readFile "1.in" >>= print . sum . map (read . filter (/= '+')) . lines
