import Data.Hash.MD5

match salt num = (=="00000") . take 5 . md5s . Str $ salt ++ show num

main = getContents
   >>= print
     . head
     . flip filter [1..]
     . match
     . head . lines

-- use -O2 lmao
