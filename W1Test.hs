{-# LANGUAGE TemplateHaskell #-}

module W1Test where

import W1
import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

args = stdArgs {maxSize = 100}

main = $forAllProperties (quickCheckWithResult args)

prop_t2_double :: Integer -> Bool
prop_t2_double x = double x `div` 2 == x

prop_t3_quadruple :: Integer -> Bool
prop_t3_quadruple x = quadruple x `div` 4 == x
                        
feq a b = abs (a-b) < 0.01
                      
prop_t4_poly2 = do
  x0 <- elements [1..4] :: Gen Double
  x1 <- elements [1..4] :: Gen Double
  a <- elements [1..4] :: Gen Double
  let b = -a*(x0+x1)
      c = a*x0*x1
      str = concat ["poly2 ",show a," ",show b," ",show c," "]
      t x y = printTestCase (str++show x++" == "++show y) $ poly2 a b c x `feq` y
  t x0 0
    .&&. t x1 0
    .&&. t ((x0+x1)/2) (-b^2/(4*a)+c)
    
prop_t5_entten_even x = entten (2*x) == "entten"
prop_t5_entten_odd x = entten (2*x+1) == "tentten"

div35 :: Gen Integer
div35 = fmap (*15) arbitrary

div5 :: Gen Integer
div5 = fmap (\x -> 5*(3*x+1)) arbitrary

div3 :: Gen Integer
div3 = fmap (\x -> 3*(5*x+1)) arbitrary

div0 :: Gen Integer
div0 = fmap (\x -> 15*x+1) arbitrary

prop_t6_fizzbuzz_3_5 = 
  forAll div35 $ \i ->
  fizzbuzz i == "FizzBuzz"
prop_t6_fizzbuzz_3 = 
  forAll div3 $ \i ->
  fizzbuzz i == "Fizz"
prop_t6_fizzbuzz_5 = 
  forAll div5 $ \i ->
  fizzbuzz i == "Buzz"
prop_t6_fizzbuzz_empty = 
  forAll div0 $ \i ->
  fizzbuzz i == ""

prop_t7_isZero_0 = isZero 0 == True
prop_t7_isZero_positive (Positive n) = isZero n == False
prop_t7_isZero_negative (Positive n) = isZero (-n) == False

prop_t8_sumTo = 
  forAll (elements [1..100]) $ \n ->
  sumTo n == sum [1..n]

prop_t9_power =
  forAll (elements [1..10]) $ \n ->
  forAll (elements [1..10]) $ \k ->
  power n k == n^k
  
prop_t10_ilog2 (Positive n) =
  ilog2 n == floor (logBase 2 $ fromIntegral n)
  
prop_t11_binomial =
  forAll (elements [0..10]) $ \n ->
  forAll (elements [0..n]) $ \k ->
  binomial n k == f n `div` (f k * f (n-k))
  where f n = product [1..n]
  
prop_t12_tribonacci =
  forAll (elements [1..15]) $ \n ->
  tribonacci n == t n
  where t 1 = 1
        t 2 = 1
        t 3 = 2
        t n = t (n-1) + t (n-2) + t (n-3)
        
prop_t13_myGcd =
  forAll (elements [1..max]) $ \x ->
  forAll (elements [1..max]) $ \y ->
  myGcd x y == gcd x y
  where max = 10000
        
odds = filter odd [-5..100]
evens = filter even [-5..100]
        
prop_t14_hassuCompare_even =
  forAll (elements evens) $ \x ->
  forAll (elements evens) $ \y ->
  hassuCompare x y == compare x y

prop_t14_hassuCompare_odd =
  forAll (elements odds) $ \x ->
  forAll (elements odds) $ \y ->
  hassuCompare x y == compare x y

prop_t14_hassuCompare_mixed =
  forAll (elements evens) $ \x ->
  forAll (elements odds) $ \y ->
  hassuCompare x y == LT
  &&
  hassuCompare y x == GT

prop_t15_hassuMinimi_even =
  forAll (elements evens) $ \x ->
  forAll (elements evens) $ \y ->
  hassuMinimi x y == min x y

prop_t15_hassuMinimi_odd =
  forAll (elements odds) $ \x ->
  forAll (elements odds) $ \y ->
  hassuMinimi x y == min x y

prop_t15_hassuMinimi_mixed =
  forAll (elements evens) $ \x ->
  forAll (elements odds) $ \y ->
  hassuMinimi x y == x
  &&
  hassuMinimi y x == x

split delim xs = 
  case rest of [] -> [a]
               (_:rest') -> a : split delim rest'
  where (a,rest) = break (==delim) xs
        
prop_t16_pyramidi =
  forAll (elements [0..40]) $ \n ->
  f (pyramidi n) == [0..n] ++ [n-1,n-2..0]
  where f xs = map read $ split ',' xs

primes = nubBy (\x y -> mod x y == 0) [2..]
        
prop_t17_smallestDivisor_prime = do
  forAll (elements $ take 12 primes) $ \p ->
    p == smallestDivisor p

prop_t17_smallestDivisor_comp = do
  k <- (elements . take 10 $ primes)
  p <- (elements . take 20 . drop 10 $ primes)
  let n = k*p
  printTestCase (show n) $
    k == smallestDivisor n

prop_t18_isPrime =
  forAll (elements [0..max]) $ \n ->
  isPrime n == elem n primes'
  where max = 20
        primes' = takeWhile (<=max) primes
        
prop_t19_nextPrime =
  forAll (elements [0..max]) $ \n ->
  nextPrime n == head (dropWhile (<n) primes)
  where max = 100
