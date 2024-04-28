import System.IO ()

for :: Int -> Int -> IO ()
for n max
    | n > max = putStrLn "END"
    | isPrime n = do
        appendFile "prime.txt" (show n ++ "\n")
        for (n + 1) max
    | otherwise = do
        for (n + 1) max

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (isNotDivisible n) [2..end]
    where 
        end = floor $ sqrt (fromIntegral n)

isNotDivisible :: Int -> Int -> Bool
isNotDivisible x y = x `mod` y /= 0

main :: IO ()
main = do
    writeFile "prime.txt" ""
    putStrLn "上限値を設定してください"
    n <- readLn
    for 2 n