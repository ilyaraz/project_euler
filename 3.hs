factor n i = if (i * i > n)
             then if (n == 1)
                  then []
                  else [n]
             else if (n `mod` i == 0)
                  then i : (factor (n `div` i) i)
                  else factor n (i + 1)

ans = maximum (factor 600851475143 2)
