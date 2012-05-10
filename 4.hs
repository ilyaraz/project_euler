reverse1 0 res = res
reverse1 n res = reverse1 (n `div` 10) (res * 10 + n `mod` 10)

ispal n = n == reverse1 n 0
ans = maximum [x | x <- [i*j | i <- [101..999], j <- [101..999]], ispal x]
