ans = head [a * b * (1000 - a - b) | a <- [1..1000], b <- [1..1000], a + b < 1000, a * a + b * b == (1000 - a - b) * (1000 - a - b)]
