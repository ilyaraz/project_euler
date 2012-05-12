factorial 0 = 1
factorial n = n * factorial (n - 1)

sumdigits 0 = 0
sumdigits n = n `mod` 10 + sumdigits (n `div` 10)

ans = sumdigits $ factorial 100
