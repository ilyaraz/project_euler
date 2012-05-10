sqr x = x * x

ans = sqr (sum [1..100]) - sum [sqr x | x <- [1..100]]
