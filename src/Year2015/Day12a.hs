import Data.Maybe
import Data.Aeson
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as BS

numbers (Number n) = [n]
numbers (Array a) = foldMap numbers a
numbers (Object o) = concatMap numbers $ Map.elems o
numbers _ = []

main = interact $ show . sum . numbers . fromJust .  decode . BS.pack
