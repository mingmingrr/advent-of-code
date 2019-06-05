import Text.Parsec
import Control.Monad.Reader
import Data.Word
import Data.Bits
import Data.Map (Map, (!))
import qualified Data.Map as Map

type Registers = Map String Word16

unary :: Map String (Word16 -> Word16)
unary = Map.fromList
  [ ("NOT", complement)
  ]

binary :: Map String (Word16 -> Word16 -> Word16)
binary = Map.fromList
  [ ("AND", (.&.))
  , ("OR", (.|.))
  , ("LSHIFT", (. fromIntegral) . shiftL)
  , ("RSHIFT", (. fromIntegral) . shiftR)
  ]

lexeme = (<* spaces)
number = (read :: String -> Word16) <$> many1 digit
register = many1 letter >>= lift . asks . flip (!)
source = number <|> register
operator = many1 upper

unaryOp = do
  op <- lexeme operator
  x <- source
  return $ (unary ! op) x

binaryOp = do
  x <- lexeme source
  op <- lexeme operator
  y <- source
  return $ (binary ! op) x y

operation = try binaryOp <|> try unaryOp <|> source

instruction :: ParsecT String u (Reader Registers) (String, Word16)
instruction = do
  val <- lexeme operation
  lexeme $ string "->"
  reg <- many1 letter
  return (reg, val)

main = do
  instructions <- lines <$> getContents
  let doParse = fmap (either (error . show) id)
              . runParserT instruction () ""
      regReader = mapM doParse instructions
      registers = Map.fromListWith (const)
                $ runReader regReader registers
      registers' = Map.fromListWith (flip const)
                 . (("b", registers ! "a") : )
                 $ runReader regReader registers'
  print $ registers' ! "a"

