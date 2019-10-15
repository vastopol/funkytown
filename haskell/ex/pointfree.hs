
-- Examples from:
-- "Point-Free or Die: Tacit Programming in Haskell and Beyond" by Amar Shah
-- incomplete code...

import Data.Function (on)

----------------------------------------

-- Combinators

blackbird f g = (f .) . g
(...) = (.) . (.)

----------------------------------------

-- Point Free Functions

-- helpers
sqr x = x ** 2
sqrt x = x ** 0.5
abs x = if x >= 0 then x else -x

sum = foldr (+) 0

aggregate1 f xs = sum (map f xs)
aggregate2 = (sum .) . map
aggregate = sum ... map

-- distances

distance o i = o . aggregate i
distance1 = (. aggregate) . (.)
distance2 = (... aggregate)

euclidian1 = sqrt . aggregate sqr
euclidian2 = distance sqrt sqr

manhattan1 = aggregate abs
manhattan2 = distance id abs

-- mastermind game

exactMatches1 = ((length . filter id) .) . zipWith (==)
exactMatches = length . filter id ... zipWith (==)

colorMatches1 = (. countColors) . ((sum .) . zipWith min) . countColors
colorMatches2 ps qs = sum (zipWith min (countColors ps) (countColors qs))
colorMatches3 ps qs = sum ((zipWith min `on` countColors) ps ps)
colorMatches = sum .. zipWith min `on` countColors



