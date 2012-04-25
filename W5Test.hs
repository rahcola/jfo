module W5Test where

import W5

import Data.List
import Data.Either
import Data.Maybe
import Control.Monad
import System.Random

import Test.QuickCheck hiding (Result,reason,classify,Failure)
import Test.QuickCheck.Test hiding (Failure)
import Test.QuickCheck.Property hiding (classify,MkResult)
import Test.QuickCheck.Monadic
import Control.Exception (try,evaluate,SomeException)

infixl 5 ===
actual === expected = 
  printTestCase ("Expected " ++ show expected ++ ", got " ++ show actual) $ actual == expected

args = stdArgs {maxSize = 50, maxSuccess = 100}
main = testExs tests -- $forAllProperties (quickCheckWithResult args)

x = property False

tests = [[prop_t1]
        ,[prop_t2]
        ,[property prop_t3_eq, property prop_t3_neq, property prop_t3_len]
        ,[prop_t4]
        ,[prop_t5]
        ,[prop_t6]
        ,[property prop_t7_eq, property prop_t7_neq]
        ,[property prop_t8_bops, property prop_t8_sops, property prop_t8_fI]
        ,[property prop_t9_1, property prop_t9_2]
        ,[prop_t10_1, prop_t10_2]
        ,[property prop_t11_eq, property prop_t11_neq]
        ,[property prop_t12_list, property prop_t12_maybe]
        ,[property prop_t13_num, prop_t13_empties]
        ,[property prop_t14_num, property prop_t14_bool]
        ,[property prop_t15_1, property prop_t15_2]
        ,[property prop_t16_normal, prop_t16_abnormal]
        ,[property prop_t17_normal, property prop_t17_abnormal]
        ,[prop_t18]
        ,[prop_t19]
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

m_t1 input exp = printTestCase (show input) $ allEqual input === exp

g_t1 input = let x = input++[False,True] in m_t1 x False

prop_t1 = conjoin [m_t1 ([] :: [Bool]) True
                  ,m_t1 [True] True
                  ,m_t1 [0] True
                  ,m_t1 [0,0,0] True
                  ,m_t1 [0,0,1] False
                  ,property g_t1
                  ]
          
m_t2 input = 
  length input > 2 ==>
    case secondSmallest input of
      Just s ->
        property $
         length (filter (<s) input) == 1
         || all (==s) (filter (<=s) input)
      Nothing -> fail "expected Just, was Nothing"
       
prop_t2 = (m_t2 :: [Int] -> Property)
          .&. (m_t2 :: [Double] -> Property)
          .&. printTestCase (show [1]) (secondSmallest ([1] :: [Int]) == Nothing)
          
prop_t3_eq :: [Integer] -> Property
prop_t3_eq xs =
  printTestCase ("findDifference "++show xs++" "++show xs) $
    isNothing (findDifference xs xs)
    
prop_t3_neq :: NonEmptyList Bool -> Property
prop_t3_neq (NonEmpty bs) = do
  i <- choose (0,length bs - 2)
  let (a,x:b) = splitAt i bs
      bs' = a ++ not x : b
    in printTestCase ("findDifference "++show bs++" "++show bs') $
      case findDifference bs bs' of Nothing -> fail "was Nothing, expected Just"
                                    Just s -> s === (show x ++ " /= " ++ show (not x))

prop_t3_len :: [Char] -> [Char] -> Property
prop_t3_len s s' =
  l /= l' ==> printTestCase ("findDifference "++show s++" "++show s') (findDifference s s' === Just err)
  where l = length s
        l' = length s'
        err = show l ++ " /= " ++ show l'
          
m_t4 input exp =
  printTestCase (show input) $
    average input === exp
    
prop_t4 = m_t4 [1,2,3] 2
          .&. m_t4 [9,9,9,9] 9
          .&. m_t4 [1,2,3,4] 2.5
          .&. m_t4 (replicate 10 1 ++ replicate 10 2) 1.5
          
m_t5 x y exp =
  printTestCase (show x ++ " == " ++ show y) $
   (x == y) === exp
          
prop_t5 = m_t5 Bar Bar True
          .&. m_t5 Quux Quux True
          .&. m_t5 Xyzzy Xyzzy True
          .&. m_t5 Bar Quux False
          .&. m_t5 Bar Xyzzy False
          .&. m_t5 Quux Bar False
          .&. m_t5 Quux Xyzzy False
          .&. m_t5 Xyzzy Bar False
          .&. m_t5 Xyzzy Quux False

prop_t6 = printTestCase ("Bar <= Bar") ((Bar <= Bar) === True)
          .&. printTestCase ("Quux < Bar") ((Quux < Bar) === True)
          .&. printTestCase ("compare Bar Xyzzy") (compare Bar Xyzzy === LT)
          .&. printTestCase ("compare Quux Quux") (compare Quux Quux === EQ)
          .&. printTestCase ("Xyzzy > Quux") ((Xyzzy > Quux) === True)
          .&. printTestCase ("min Xyzzy Bar") (min Xyzzy Bar === Bar)
          .&. printTestCase ("max Bar Quux") (max Bar Quux === Bar)
          
prop_t7_eq a b c =
  let v = Vector a b c in
  printTestCase (show v ++ " == " ++ show v) $
   (v == v) === True
   
prop_t7_neq a b c d e f =
  let v = Vector a b c
      v2 = Vector d e f
  in printTestCase (show v ++ " == " ++ show v2) $
     (v == v2) === ((a,b,c)==(d,e,f))
     
prop_t8_bops a b c d e f =
  let v1 = Vector a b c
      v2 = Vector d e f 
      g0 (Vector a _ _) = a
      g1 (Vector _ a _) = a
      g2 (Vector _ _ a) = a
  in conjoin
     [printTestCase (show v1 ++ " + " ++ show v2) $
      conjoin [g0 (v1+v2) === a+d
              ,g1 (v1+v2) === b+e
              ,g2 (v1+v2) === c+f]
     ,printTestCase (show v1 ++ " * " ++ show v2) $
      conjoin [g0 (v1*v2) === a*d
              ,g1 (v1*v2) === b*e
              ,g2 (v1*v2) === c*f]
     ,printTestCase (show v1 ++ " - " ++ show v2) $
      conjoin [g0 (v1-v2) === a-d
              ,g1 (v1-v2) === b-e
              ,g2 (v1-v2) === c-f]
     ]
     
prop_t8_sops a b c =
  let v = Vector a b c
      g0 (Vector a _ _) = a
      g1 (Vector _ a _) = a
      g2 (Vector _ _ a) = a
  in conjoin
     [printTestCase ("abs ("++show v++")") $
      conjoin [g0 (abs v) === abs a
              ,g1 (abs v) === abs b
              ,g2 (abs v) === abs c]
     ,printTestCase ("signum ("++show v++")") $
      conjoin [g0 (signum v) === signum a
              ,g1 (signum v) === signum b
              ,g2 (signum v) === signum c]
     ,printTestCase ("negate ("++show v++")") $
      conjoin [g0 (negate v) === negate a
              ,g1 (negate v) === negate b
              ,g2 (negate v) === negate c]]
     
prop_t8_fI a =
  fromIntegral a === Vector a a a
          
prop_t9_1 bs =
  let out = freqs bs
      (t,f) = partition id bs
  in (printTestCase "True-arvon lukumaara oikein" $
      null t || (length t,True) `elem` out)
     .&.
     (printTestCase "False-arvon lukumaara oikein" $
      null f || (length f,False) `elem` out)
     
prop_t9_2 :: [Integer] -> Property
prop_t9_2 is =
  let out = freqs is
      vals = nub is
  in (printTestCase "Palautuslistan pituus oikein" $
      length out === length vals)
     .&&.
     (foldl (.&&.) (property True) $ map (ck out is) vals)
  where ck out vals i = let exp = length (filter (==i) vals)
                        in printTestCase ("Loytyyko tuloksesta "++show out++" arvo "++show(exp,i)) $ (exp,i) `elem` out
          
genTree :: Int -> Gen ITree
genTree 0 = return ILeaf
genTree siz = do
  let siz' = siz-1
  sizl <- choose (0,siz')
  let sizr = siz'-sizl
  l <- genTree sizl
  r <- genTree sizr
  v <- choose (0,10)
  return $ INode v l r
  
modTree :: ITree -> Gen ITree
modTree ILeaf = do
  s <- choose (1,5)
  t <- genTree s
  return $ t
modTree (INode x l r) =
  oneof [return ILeaf,
         do x' <- choose (0,10) `suchThat` (/=x)
            return $ INode x' l r,
         do l' <- modTree l
            return $ INode x l' r,
         do r' <- modTree r
            return $ INode x l r']
  
prop_t10_1 =
  forAllShrink (choose (0,20)) shrink $ \s ->
  do t <- genTree s
     printTestCase (show t ++ "\n  ==\n"++show t) $ (t==t) == True
       
prop_t10_2 =
  forAllShrink (choose (0,20)) shrink $ \s ->
  do t <- genTree s
     t2 <- modTree t
     printTestCase (show t ++ "\n  ==\n"++show t2) $ (t==t2) == False
                           
prop_t11_eq :: [Bool] -> Property
prop_t11_eq xs =
  let l = foldr LNode Empty xs  in
  printTestCase (show l ++ " == " ++ show l) $
  (l == l) === True
  
prop_t11_neq :: [Integer] -> [Integer] -> Property
prop_t11_neq xs ys =
  let l = foldr LNode Empty xs  
      l2 = foldr LNode Empty ys
  in
   printTestCase (show l ++ " == "++ show l2) $
  (l == l2) === (xs == ys)
          
prop_t12_list :: [Integer] -> Property
prop_t12_list xs = printTestCase (show xs) $
  incrementAll xs === map (+1) xs
  
prop_t12_maybe :: Maybe Integer -> Property
prop_t12_maybe m = printTestCase (show m) $
  incrementAll m === case m of Nothing -> Nothing
                               Just x -> Just (x+1)
                               
prop_t13_num k =
  printTestCase ("fmap (+1) (MkResult "++show k) $
  fmap (+(1::Int)) (MkResult k) === MkResult (k+1)
  
prop_t13_empties =
  (printTestCase ("fmap not NoResult") $
   fmap not NoResult === NoResult)
  .&.
  (printTestCase ("fmap not (Fail \"moi\")") $
   fmap not (Failure "moi") === Failure "moi")
  
prop_t14_num :: [Int] -> Property
prop_t14_num xs =
  let l = foldr LNode Empty xs in
  printTestCase ("fmap (+1) "++show l) $
    ck (fmap (+1) l) (map (+1) xs)

prop_t14_bool bs =
  let l = foldr LNode Empty bs in
  printTestCase ("fmap not "++show l) $
    ck (fmap not l) (map not bs)

ck :: (Eq a, Show a) => List a -> [a] -> Property
ck Empty [] = property True
ck (LNode x xs) (y:ys) = (x === y) .&&. ck xs ys
ck Empty ys = fail "Tuloslista loppui liian aikaisin"
ck xs [] = fail "Tuloslista oli liian pitkä"

prop_t15_1 i =
  printTestCase ("runFun (fmap not (Fun even)) "++show i) $
    runFun (fmap not (Fun even)) i === odd i
    
prop_t15_2 i =
  printTestCase ("runFun (fmap (*2) (Fun (\\i -> i))) "++show i) $
    runFun (fmap (*2) (Fun id)) i === 2*i

prop_t16_normal b0 b1 =
  (b0 ||| b1) === (b0 || b1)
  
prop_t16_abnormal =
  printTestCase ("undefined ||| True") $
  (error "Evaluoit vasemman argumenttisi" ||| True) === True
  
prop_t17_normal bs =
  boolLength bs === length bs
  
prop_t17_abnormal bs =
  printTestCase ("Taman pitaisi epaonnistua: boolLength ("++show bs++"++[undefined])") $ monadicIO $ do
    e <- run $ try (evaluate (boolLength (bs++[undefined])))
    stop $ isLeft (e :: Either SomeException Int)
  where isLeft (Left _) = True
        isLeft _ = False
  
prop_t18 = do s <- choose (0,10)
              let g = mkStdGen s
                  (a,b,c) = threeRandom g :: (Int,Int,Int)
              printTestCase ("arvot eivat olleet eri: threeRandom (mkStdGen "++show s++")") $
                conjoin [a/=b,
                         a/=c,
                         b/=c]

shape :: (Show a, Show b) => Tree a -> Tree b -> Property
shape Leaf Leaf = property $ True
shape (Node _ l r) (Node _ l' r') = 
  conjoin [shape l l',
           shape r r']
shape x y = fail $ "Puut eivät ole samanmuotoisia:\n"++show x++"\n"++show y

genTree' :: Int -> Gen (Tree Bool)
genTree' 0 = return Leaf
genTree' siz = do
  let siz' = siz-1
  sizl <- choose (0,siz')
  let sizr = siz'-sizl
  l <- genTree' sizl
  r <- genTree' sizr
  v <- arbitrary
  return $ Node v l r

v Leaf = []
v (Node x l r) = x : v l ++ v r

prop_t19 = forAllShrink (choose (0,10)) shrink $ \siz -> 
  do s <- choose (0,10)
     t <- genTree' siz
     let g = mkStdGen s
         (t',_) = randomizeTree t g :: (Tree Int,StdGen)
         vals = v t'
     printTestCase ("randomizeTree ("++show t++") (mkStdGen "++show s++")") $
       conjoin [shape t t'
               ,printTestCase "arvot eivat olleet eri" $ vals == nub vals]