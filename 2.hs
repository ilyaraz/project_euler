fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

filter1 (x:xs) = if (x <= 4000000) then (x : (filter1 xs)) else []

ans = sum [x | x <- (filter1 fibs), even x]
