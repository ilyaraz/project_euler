import Data.Set (Set)
import qualified Data.Set as Set

n = 2000000

init1 = foldr Set.insert Set.empty [2..n]

process p x = foldr Set.delete p [x, 2 * x .. n]

primes p = if (Set.size p == 0)
           then []
           else let x = Set.findMin p in x : primes (process (Set.deleteMin p) x)

ans = sum (primes init1)
