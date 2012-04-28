module W6Test where

import W6

import Data.List
import Data.Maybe
import Data.Char
import Data.Either
import Data.Ord
import Control.Monad
import Control.Monad.State
import System.Random

import Test.QuickCheck hiding (Result,reason,classify,Failure)
import Test.QuickCheck.Test hiding (Result,Failure)
import Test.QuickCheck.Property hiding (Result,classify,MkResult)
import Test.QuickCheck.Monadic
import Control.Exception (try,evaluate,SomeException)

infixl 5 ===
actual === expected = 
  printTestCase ("Expected " ++ show expected ++ ", got " ++ show actual) $ actual == expected

args = stdArgs {maxSize = 50, maxSuccess = 200}
main = testExs tests -- $forAllProperties (quickCheckWithResult args)

x = property False

tests = [[prop_t1_ok, prop_t1_fail]
        ,[property $ prop_t2]
        ,[property $ prop_t3]
        ,[prop_t4_ok, prop_t4_fail]
        ,[prop_t5_ok, prop_t5_fail]
        ,[prop_t6_1, prop_t6_2, prop_t6_3]
        ,[property $ prop_t7]
        ,[property $ prop_t8]
        ,[property $ prop_t9]
        ,[property $ prop_t10]
        ,[property $ prop_t11_1, property $ prop_t11_2]
        ,[property $ prop_t12_1]
        ,[prop_t13_1, prop_t13_2, prop_t13_3, prop_t13_4]
        ,[prop_t13_1, prop_t13_2, prop_t13_3, prop_t13_4]
        ,[prop_t15_1, prop_t15_2]
        ,[prop_t16]
        ,[prop_t17_1, prop_t17_2, property $ prop_t17_3]
        ,[prop_t18]
        ,[prop_t19_2]
        ,[prop_t19_2]
        ]

testEx str ts = do
  putStrLn ("Testing "++str)
  res <- mapM (quickCheckWithResult args) ts
  if all isSuccess res
    then putStrLn "PASS" >> return True
    else putStrLn "FAIL" >> return False
         
testExs tests = do
  sucs <- forM (zip [1..] tests) $ \(i,ts) -> testEx (show i) ts
  let success = length . filter id $ sucs
      total = length tests
  putStrLn $ "TOTAL: "++show success++" / "++show total

-- -- -- -- -- -- -- --

word = do fst <- choose ('A','Z')
          rest <- listOf (choose ('a','z'))
          return $ fst:rest

huono = do a <- choose ('A','Z')
           b <- word
           c <- elements "0123456789"
           d <- word
           return $ a:b++c:d

prop_t1_ok = do
  etu <- word
  suku <- word
  let str = etu++" "++suku
  printTestCase ("lueNimet "++show str) $
    lueNimet str === Just (etu,suku)
    
m_t1_fail s =
  printTestCase ("lueNimet "++show s) $ lueNimet s === Nothing
    
prop_t1_fail =
  do etu <- word
     suku <- word
     h <- huono
     conjoin [m_t1_fail (etu++suku),
              m_t1_fail (map toLower etu ++ " " ++ suku),
              m_t1_fail (etu ++ " " ++ map toLower suku),
              m_t1_fail (etu ++ h ++ " " ++ suku),
              m_t1_fail (etu ++ " " ++ suku ++ h)]
  

prop_t2 :: [Maybe Int] -> Property
prop_t2 ms = chainList ms === sequence ms

prop_t3 is = case all (>=0) is of True -> sumPos is === Just (sum is)
                                  False -> sumPos is === Nothing

prop_t4_ok = do
  as <- listOf1 arbitrary :: Gen [Int]
  i <- choose (0,length as)
  let ml = Just as
      mi = Just i
  printTestCase ("myTake ("++show mi++") ("++show ml++")") $
    myTake mi ml === Just (take i as)
    
prop_t4_fail = do
  as <- listOf1 arbitrary :: Gen [Int]
  i <- choose (length as+1,length as+5)
  let ml = Just as
      mi = Just i
  conjoin [printTestCase ("myTake ("++show mi++") ("++show ml++")") $
           myTake mi ml === Nothing,
           printTestCase ("myTake Nothing ("++show ml++")") $
           myTake Nothing ml === Nothing,
           printTestCase ("myTake ("++show mi++") Nothing") $
           myTake mi (Nothing :: Maybe String) === Nothing]

prop_t5_ok = do
  as <- listOf1 arbitrary :: Gen [Integer]
  is <- listOf (choose (0,length as - 1))
  printTestCase ("selectSum "++show as++" "++show is) $
    selectSum as is === Just (sum $ map (as!!) is)
    
prop_t5_fail = do
  as <- arbitrary :: Gen [Int]
  is1 <- listOf (choose (0,length as - 1))
  is2 <- listOf (choose (0,length as - 1))
  b <- elements [-1,length as]
  let is = is1++b:is2
  printTestCase ("selectSum "++show as++" "++show is) $
    selectSum as is === Nothing
    
b n k = case (n,k) of (_,0) -> 1
                      (0,_) -> 0
                      (n,k) -> b (n-1) (k-1) + b (n-1) k
    
prop_t6_1 = do
  n <- choose (0,7)
  k <- choose (0,n)
  let Logger _ res = binom n k
  printTestCase ("Kutsun binom "++show n++" "++show k++" palautusarvo") $
    res === b n k
    
prop_t6_2 = do
  n <- choose (0,7)
  k <- choose (0,n)
  let Logger log _ = binom n k
  printTestCase ("Kutsun binom "++show n++" "++show k++" loki") $
    conjoin [printTestCase "lokin ei pitäisi olla tyhjä" $
             not $ null log,
             printTestCase "lokin viimeinen viesti" $
             last log === ("B("++show n++","++show k++")"),
             printTestCase "lokin ensimmäinen viesti" $
             head log === ("B("++show (n-k)++",0)")]    
                                  
    
    
prop_t6_3 = 
  conjoin [t 2 2 ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)"],
           t 2 7 ["B(0,5)","B(0,6)","B(1,6)","B(0,6)","B(0,7)","B(1,7)","B(2,7)"],
           t 3 3 ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(0,1)","B(0,2)","B(1,2)","B(0,2)","B(0,3)","B(1,3)","B(2,3)","B(3,3)"],
           t 4 3 ["B(1,0)","B(0,0)","B(0,1)","B(1,1)","B(2,1)","B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(3,2)","B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(0,1)","B(0,2)","B(1,2)","B(0,2)","B(0,3)","B(1,3)","B(2,3)","B(3,3)","B(4,3)"]]
  where t n k log = printTestCase ("binom "++show n++" "++show k) $ let Logger l _ = binom n k in l===log
           
prop_t7 i = printTestCase ("runState paivitys "++show i) $
            runState paivitys i === ((),2*i+1)
            
prop_t8 bs = printTestCase ("runState (lengthAndCount True "++show bs++") 0") $
               runState (lengthAndCount True bs) 0 === (length bs, length (filter id bs))
               
prop_t9 = do
  is <- fmap nub $ listOf1 (choose ('a','z') :: Gen Char)
  fs <- vectorOf (length is) (choose (1,2048))
  let assocs = zip is fs
  x <- elements is
  y <- choose  ('0','z') `suchThat` \y -> not (elem y is)
  let Just cx = lookup x assocs
      ((),rx) = runState (count x) assocs
      ((),ry) = runState (count y) assocs
      s x = "runState (count "++show x++") "++show assocs
  return $ conjoin [printTestCase (s y) $
                    sort ry === sort ((y,1):assocs),
                    printTestCase (s x) $
                    sort rx === sort ((x,cx+1):delete (x,cx) assocs)]
    
prop_t10 =
  forAllShrink (listOf (choose (0,10 :: Integer))) shrink $ \is ->
  let (r,_) = runState (occurrences is) []
      ck i = ascs [r !! j | (j,x) <- zip [0..] is, x==i]
      ascs xs = xs == [1..length xs]
  in all ck (nub is)
  
prop_t11_1 :: Maybe Bool -> Maybe Int -> Maybe Int -> Property
prop_t11_1 b t e = ifM b t e === case b of Just True -> t
                                           Just False -> e
                                           Nothing -> Nothing
                                          
prop_t11_2 = do
  b <- arbitrary
  t <- arbitrary :: Gen Integer
  e <- arbitrary :: Gen Integer
  printTestCase ("runState (ifM (return "++show b++") (return "++show t++") (return "++show e++")) 0") $
    runState (ifM (return b) (return t) (return e)) 0 === (if b then t else e,0)
    
prop_t12_1 :: [Int] -> [Int] -> Property
prop_t12_1 as bs =
  printTestCase ("mapM2 (\\x y -> if x == y then Nothing else Just (x-y)) "++show as++" "++show bs) $
    mapM2 (\x y -> if x == y then Nothing else Just (x-y)) as bs === res
  where z = zipWith (-) as bs
        res = if all (/=0) z then Just z else Nothing
  
prop_t13_1 = do  
  let cs = [[1],[0,2],[1,3],[2,4],[3,5],[4]]
  i <- choose (1,length cs - 1)
  let st = [0..i-1]
  printTestCase ("runState (dfs "++show cs++" "++show i++") "++show st) $
    let ((),res) = runState (dfs cs i) st
    in sort res === [0..5]
  
prop_t13_2 = do
  let cs = [[1,4],[0,2],[1,3],[2,4],[3,0]]
  i <- choose (1,length cs - 1)
  printTestCase ("runState (dfs "++show cs++" "++show i++") []") $
    let ((),res) = runState (dfs cs i) []
    in sort res === [0..4]
  
prop_t13_3 = do
  siz <- choose (2,5)
  let cs = map (\i -> delete i [0..siz-1]) [0..siz-1]
  a <- choose (0,siz-1)
  b <- choose (0,siz-1)
  printTestCase ("routeExists "++show cs++" "++show a++" "++show b) $
    routeExists cs a b === True
  
shuffle xs = do
  is <- vector (length xs) :: Gen [Int]
  return $ map snd . sortBy (comparing fst) $ zip is xs
  
  
genGraph' :: [Int] -> [Int] -> [(Int,Int)] -> Gen [(Int,Int)]
genGraph' is [] es = return es
genGraph' is todo es = do
  u <- elements $ todo
  v <- elements $ is \\ todo
  genGraph' is (delete u todo) ((u,v):(v,u):es)
  
genGraph :: [Int] -> Gen [(Int,Int)]
genGraph is = do
  base <- genGraph' is (tail is) []
  [a,b,c] <- vectorOf 3 (elements is)
  return $ (a,b):(b,c):base
  
mkGraph es = map neighs [0..n]
  where n = maximum (map fst es ++ map snd es)
        neighs i = nub $ sort $ map snd $ filter (\(x,_) -> x==i) es
              
prop_t13_4 = do
  siz <- choose (5,7)
  k <- choose (2,siz-2)
  left <- genGraph [0..k]
  right <- genGraph [k+1..siz-1]
  i <- choose (0,siz-1)
  j <- choose (0,siz-1)
  let cities = mkGraph (left++right)
  printTestCase (show left++"\n"++show right++"\n"++"routeExists "++show cities++" "++show i++" "++show j) $
    routeExists cities i j === ((i<=k) == (j<=k))

m is = maximum (scanl1 (+) is)

prop_t15_1 = do
  let n = 6
  is <- vectorOf n (choose (0,10))
  i <- choose (0,n-2)
  j <- choose (i+1,n-1)
  let a = is!!i
      b = is!!j
      ret = orderedPairs is
  printTestCase ("orderedPairs "++show is) $
    if a<b
    then printTestCase ("Parin "++show (a,b)++" pitäisi olla listassa.") $ (a,b) `elem` ret
    else printTestCase ("Parin "++show (a,b)++" ei pitäisi olla listassa.") . not $ (a,b) `elem` ret

prop_t15_2 = do
  let n = 7
  let is0 = [0..n]
  x <- choose (0,n)
  let is = drop x is0 ++ take x is0
      exp = [(i,j) | i<-[0..x-2], j<-[i+1..x-1]]
            ++
            [(i,j) | i<-[x..n-1], j<-[i+1..n]]
  printTestCase ("orderedPairs "++show is) $
    sort (orderedPairs is) === sort exp
    
sums [] = [0]
sums (x:xs) = sums xs ++ map (x+) (sums xs)
  
prop_t16 = do
  siz <- choose (0,5)
  is <- vectorOf siz (choose (0,10))
  printTestCase ("summat "++show is) $ sort (summat is) === sort (sums is)

prop_t17_1 = 
  forAllShrink (listOf1 (choose (-10,10))) shrink $ \is ->
  let k = m is + 1
  in printTestCase ("sumBounded "++show k++" "++show is) $
     sumBounded k is === Just (sum is)
  
prop_t17_2 = 
  forAll (listOf1 (choose (-10,10))) $ \is ->
  let k = m is - 1
  in printTestCase ("sumBounded "++show k++" "++show is) $
     sumBounded k is === Nothing
     
prop_t17_3 is = 
  sumNotTwice is === sum (nub is)
  
prop_t18 = 
  let op :: Int -> Result Int
      op i = if i>3 then fail "big" else return (i+1)
      s = "let op i = if (i>3) then fail \"big\" else return (i+1) in "
  in conjoin [printTestCase (s++" MkResult 1 >>= op") $
              (MkResult 1 >>= op) === MkResult 2,
              printTestCase (s++" MkResult 4 >>= op") $
              (MkResult 4 >>= op) === Failure "big",
              printTestCase (s++" Fail \"foo\" >>= op") $
              (Failure "foo" >>= op) === Failure "foo",
              printTestCase (s++" NoResult >>= op") $
              (NoResult >>= op) === NoResult]
     
prop_t19_2 =
  arbitrary >>= \o ->
  shrinking shrink o $ \ops ->
  let m (Left i) = modifySL (+i)
      m (Right s) = msgSL s
      s (Left i) = "modifySL (+"++show i++")"
      s (Right m) = "msgSL "++show m
      op = mapM_ m ops
      desc = "runSL ("++intercalate " >> " (map s ops)++") 0"
      (incs,msgs) = partitionEithers ops
      state = sum incs
  in printTestCase desc $ runSL op 0 === ((),state,msgs)
   