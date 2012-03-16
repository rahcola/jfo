module W1 where

-- Tehtävä 1: määrittele muuttujat one, two, ja three. Kaikkien tyyppi
-- on Int ja arvot ovat 1, 2 ja 3. Tälle tehtävälle ei ole testejä.
one :: Int
one = 1

two :: Int
two = 2

three :: Int
three = 3

-- Tehtävä 2: määrittele funktio double, joka on tyyppiä Integer->Integer.
-- double ottaa yhden argumentin ja palauttaa sen kaksinkertaisena.

double :: Integer -> Integer
double x = 2 * x

-- Tehtävä 3: määrittele funktio quadruple, joka käyttää funktiota
-- double nelinkertaistamaan annetun luvun.

quadruple :: Integer -> Integer
quadruple x = double . double $ x

-- Tehtävä 4: määrittele tehtävä poly2, joka ottaa neljä
-- Double-tyyppistä argumenttia, a, b,c ja x. Funktion arvo on 2.
-- asteen polynomin, jonka kertoimet ovat a, b ja c arvo pisteessä x.

poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a * x^2 + b * x + c 

-- Tehtävä 5: toteuta alle funktio entten, joka palauttaa parillisille
-- argumenteille merkkijonon "entten" ja parittomille "tentten"
--
-- Ps. Kannattaa katsoa mitä funktio even tekee

entten :: Integer -> String
entten x = if even x then "entten" else "tentten"

-- Tehtävä 6: toteuta funktio fizzbuzz joka palauttaa 3:lla
-- jaollisille luvuille "Fizz", 5:llä jaollisille "Buzz" ja
-- kummallakin 3:lla ja 5:llä jaollisille "FizzBuzz". Muille luvuille
-- funktio palauttaa tyhjän merkkijonon.
--
-- Jakojäännöksen voit laskea funktiolla mod

fizzbuzz :: Integer -> [Char]
fizzbuzz x
  | mod x 5 == 0 && mod x 3 == 0 = "FizzBuzz"
  | mod x 5 == 0 = "Buzz"
  | mod x 3 == 0 = "Fizz"
  | otherwise = ""

-- Tehtävä 7: toteuta funktio isZero, joka palauttaa True jos
-- parametri on 0 ja False muuten. KÄYTÄ HAHMONSOVITUSTA!
--
-- Muistathan, totuusarvot Haskellissa ovat tyyppiä Bool.

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

-- Tehtävä 8: toteuta rekursiivinen funktio sumTo n, joka laskee
-- summan 1+2+...+n

sumTo :: Integer -> Integer
sumTo n
  | n < 1 = undefined
  | n == 1 = 1
  | otherwise = n + sumTo (n - 1)

-- Tehtävä 9: toteuta rekursiivinen funktio power n k, joka laskee n^k.

power :: Integer -> Integer -> Integer
power 0 _ = 0
power n k
  | k > 0 = n * power n (k - 1)
  | k < 0 = undefined
  | otherwise = 1

-- Tehtävä 10: toteuta rekursiivinen funktio ilog2 n, joka laskee
-- montako kertaa kokonaisluvun voi jakaa kahdella ennen kuin siitä
-- tulee 1. Käytä kokonaislukujen jakolaskua eli funktiota div

ilog2 :: Integer -> Integer
ilog2 n 
  | n < 1 = undefined
  | otherwise = ilog2' n 0
      where ilog2' 1 times = times
            ilog2' n times = ilog2' (div n 2) (times + 1)

-- Tehtävä 11: toteuta rekursiivinen funktio binomial, joka laskee
-- binomikertoimen. Binomikertoimen määrittelee seuraava
-- rekursioyhtälö:
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, kun k>0

-- Vihje! Hahmonsovitus auttaa tekemään tämän ytimekkäästi. Muista
-- että yhtälöitä sovitetaan määrittelyjärjestyksessä!

binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k
  | k > 0 = (binomial (n - 1) k) + (binomial (n - 1) (k - 1))
  | otherwise = undefined

-- Tehtävä 12: toteuta funktio tribonacci, joka laskee n:nnen
-- tribonaccin lukujonon luvun. Tribonaccin lukujono määritellään
-- seuraavasti:
--   T(1) = 1
--   T(2) = 1
--   T(3) = 2
--   T(n+1) = T(n)+T(n-1)+T(n-2)
--
-- Tee toteutuksestasi tehokas (lineaariaikainen), eli käytä vastaavaa
-- rekursiivista apufunktiota kuin luentokalvojen
-- fibonacci-esimerkissä

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n
  | n < 1 = undefined
  | otherwise = (tribonacci (n - 1)) + (tribonacci (n - 2)) + (tribonacci (n - 3))

-- Tehtävä 13: Eukleideen algoritmi on tapa laskea kahden luvun suurin
-- yhteinen tekijä. Lue algoritmin kuvaus wikipediasta ja toteuta se
-- funktioksi myGcd :: Integer -> Integer -> Integer

myGcd :: Integer -> Integer -> Integer
myGcd a 0 = a
myGcd a b = myGcd b (mod a b)

-- Tehtävä 14: Haskellin standardikirjasto määrittelee tyypin
-- Ordering, jonka arvot ovat LT, GT ja EQ. Voit kokeilla miten
-- Ordering toimii esimerkiksi seuraavilla lausekkeilla:
--
--   compare 3 4
--   compare 4 3
--   compare 0 0
--   compare "Hei" "Moi"
--   compare True False
--
-- Nyt tehtävänäsi on toteuttaa funktio hassuCompare :: Int -> Int ->
-- Ordering, joka järjestää kokonaislukuja seuraavasti:
--
-- 1. Kaikki parilliset luvut ovat parittomia lukuja pienempiä
-- 2. Muuten lukujen järjestys on normaali

hassuCompare :: Int -> Int -> Ordering
hassuCompare a b
  | even a && odd b = LT
  | odd a && even b = GT
  | otherwise = compare a b

-- Tehtävä 15: Toteuta funktio hassuMinimi :: Int -> Int -> Int, joka
-- palauttaa hassuComparen mielestä pienimmän argumenteistaan. Käytä
-- hahmontunnistusta Ordering-arvojen analysoimiseen.
--
-- Huomio: hahmontunnistusta varten joudut joko käyttämään
-- apufunktiota tai case-of-lauseketta (ei vielä esitelty kurssilla,
-- mutta löytyy internet-materiaaleista).
--
-- Huomio: käytä hassuCompare-funktiota

hassuMinimi :: Int -> Int -> Int
hassuMinimi a b = min' (hassuCompare a b)
  where min' LT = a 
        min' _ = b

-- Tehtävä 16: toteuta funktio pyramidi, joka tuottaa tällaisia
-- merkkijonoja:
--
-- pyramidi 0: "0"
-- pyramidi 1: "0,1,0"
-- pyramidi 2: "0,1,2,1,0"
-- pyramidi 3: "0,1,2,3,2,1,0"
--
-- Vihjeitä:
-- * merkkijonoja liitetään yhteen operaattorilla ++
-- * funktio show muuntaa luvun merkkijonoksi
-- * tarvitset rekursiiviisen apufunktion

pyramidi :: Integer -> String
pyramidi 0 = "0"
pyramidi n = up n ++ "," ++ down (n - 1)
  where up 0 = "0"
        up n = up (n - 1) ++ "," ++ show n
        down 0 = "0"
        down n = show n ++ "," ++ down (n - 1)

-- Tehtävä 17: toteuta funktio smallestDivisor n, joka palauttaa
-- pienimmän luvun k>1 s.e. n on jaollinen k:lla.
--
-- Kannattaa muistaa funktio mod
--
-- Huom! smallestDivisor 1 ja smallestDivisor 0 ovat helposti
-- loputtomia rekursioita. Muista tämä seuraavassa tehtävässä.

smallestDivisor :: Integer -> Integer
smallestDivisor 0 = undefined
smallestDivisor 1 = undefined
smallestDivisor n = smallestDivisor' n 2
  where smallestDivisor' n k
          | mod n k == 0 = k
          | otherwise = smallestDivisor' n (k + 1)

-- Tehtävä 18: toteuta funktio isPrime, joka tarkistaa onko annettu
-- luku alkuluku käyttämällä funktiota smallestDivisor.
--
-- Alkuluku on luku joka ei ole jaollinen muilla luvuilla kuin
-- itsellään ja 1:llä. 0 ja 1 eivät ole alkulukuja.

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = smallestDivisor n == n

-- Tehtävä 19: toteuta funktio nextPrime, joka palauttaa annettua
-- lukua seuraavan alkuluvun. Jos luku on alkuluku, palautetaan se
-- itse. Ts. nextPrime n ==> k s.e. k >= n ja k on alkuluku.
--
-- Tässä kannattaa luonnollisesti käyttää apuna funktiota isPrime.

nextPrime :: Integer -> Integer
nextPrime n
  | isPrime n = n
  | otherwise = nextPrime (n + 1)

