main = getLine >>= print . sum . map (maybe 0 id . (`lookup` [('(', 1), (')', -1)]))
