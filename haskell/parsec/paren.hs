-- hackage parsec example
-- language of ballanced parens

import Text.Parsec

parenSet  :: Parsec String () Char
parenSet = char '(' >> many parenSet >> char ')'

parens = (many parenSet >> eof) <|> eof

main:: IO ()
main =
    print (parse parens "" "()")
    -- print (parse parens "" "()(())")
    -- print (parse parens "" "(")

