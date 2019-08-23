-- Tribonacci Sequence

main :: IO()
main = do
    putStrLn "Enter Nth number"
    input <- getLine
    let num = (read input :: Int)
    let res = trib(num)
    print res

trib :: Int -> Int
trib 0 = 0
trib 1 = 1
trib 2 = 1
trib n = trib(n-1) + trib(n-2) + trib(n-3)

