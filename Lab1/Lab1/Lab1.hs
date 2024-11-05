{- Lab 1
   Date: 4th November 2024
   Authors: SAMAIN Luc | GATIMEL Loan
   Lab group: 5
 -}
--------------------------------------------
import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k+1 


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k
   | k < 0 = error "power: negative argument"
power1 n 0  = 1
power1 n k = product([n | x <- [1..k]])


-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k
   | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k 
   | even k  = power2 (n * n) (k `div` 2) 
power2 n k 
   | odd k = n * power2 n (k - 1)


-- D -------------------------
{- 
<Describe your test cases here>
   1) Get an error by passing a negative k value (not included in the powerTest function)
   2) Edges cases n = 0 and k = 0, should output 0 and 1 respectively
   3) Different values:
      3.a) n = 2 and k = 3 should output 8
      3.b) n = 7 and k = 2 should output 49
      3.c) n = 12 and k = 43 should output 25397652694505813866070015990659936347412758528

 -}

--
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power2 n k == power1 n k) && (power1 n k == power n k)

--
powerTest :: Bool
powerTest = and [prop_powers n k | n <-[0, 42, 2, 7, 12], k <- [42, 0, 3, 2, 43]]

--
prop_powers's :: Integer -> Integer -> Bool
prop_powers's n k = (power2 n k' == power1 n k') && (power1 n k' == power n k')
   where k' = abs(k)