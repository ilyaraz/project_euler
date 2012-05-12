sumdigits 0 = 0
sumdigits n = n `mod` 10 + sumdigits (n `div` 10)

ans = sumdigits $ 2^1000
