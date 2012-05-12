dsum n i = if (n < i * i) then 0
           else if (n == i * i) then i
           else if (n `mod` i /= 0) then dsum n (i + 1)
           else i + n `div` i + dsum n (i + 1)

getD n = 1 + dsum n 2

isAmicable n = let x = getD n in if (x == n) then False else getD x == n

ans = sum $ filter isAmicable [2..9999]
