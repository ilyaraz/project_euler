import List

primes' (x:xs) = x : primes' [y | y <- xs, y `mod` x /= 0]
primes = primes' [2..]

getb a b c = if (a * b > c) then 0
             else 1 + (getb (a * b) b c)

bt num d cur = if ((num > 15000))
               then []
               else if (cur >= 10) then if (d >= 23) then [num] else []
               else foldl (++) [] (map (\x -> bt (num * primes!!cur^x) (d * (x + 1)) (cur + 1))[0 .. (getb num (primes!!cur) 15000)])

process n i = if (n `mod` i /= 0) then (n, 1)
              else let x = process (n `div` i) i in
                   (fst x, snd x + 1)

divs n i = if (i * i > n) then if (n > 1) then 2 else 1
           else let x = process n i in
                (snd x) * divs (fst x) (i + 1)

ans = minimum [x | (x, y) <- [(x, divs x 2) | x <- sort (foldl (++) [] [[x * (x + 1) `div` 2, x * (x - 1) `div` 2] | x <- bt 1 1 0])], y >= 500]
