import Data.Hash.MD5

match salt num = (=="000000") . take 6 . md5s . Str $ salt ++ show num

main = getContents
   >>= print
     . head
     . flip filter [1..]
     . match
     . head . lines

-- use -O2 lmao
