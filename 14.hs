solve :: Int -> Int
solve 1 = 1
solve n = if (even n) then 1 + solve (n `div` 2)
                      else 1 + solve (3 * n + 1)

get :: Int -> (Int, Int)
get 0 = (0, 0)
get n = let x = solve n
            y = get $ n - 1
            in if (fst y > x) then y else (x, n) 

ans :: Int
ans = snd $ get 999999

main = do
    putStrLn (show ans)
