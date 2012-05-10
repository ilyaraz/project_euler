isprime n i = if (i * i > n)
              then True
              else if (n `mod` i == 0)
                   then False
                   else isprime n (i + 1)

ans = [x | x <- [2..], isprime x 2] !! 10000
