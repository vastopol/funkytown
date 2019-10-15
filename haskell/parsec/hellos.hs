-- parsec wikipedia example

import Text.Parsec      -- has general parsing utility functions
import Text.Parsec.Char -- contains specific basic combinators

-- type Parser = Stream s m Char => ParsecT s u m String
-- parser :: Parser
parser = string "hello"

main :: IO ()
main = print (parse parser "<test>" "hello world")

