factorial 0 = 1
factorial n = n * factorial (n - 1)

sqr x = x * x

ans = (factorial 40) `div` sqr (factorial 20)
