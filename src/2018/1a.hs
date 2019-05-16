main = getContents >>= print . sum . map (read . filter (/= '+')) . lines
