{-# LANGUAGE TemplateHaskell #-}

module W3Test where

import W3

import GHC.IO.Handle
import System.Directory
import System.IO
import Control.Exception (finally)
import Control.Monad
import Data.List
import Data.IORef

import Test.QuickCheck hiding (Result,reason)
import Test.QuickCheck.Test
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Test.QuickCheck.Monadic

infixl 5 ===
actual === expected = 
  printTestCase ("Expected " ++ show expected ++ ", got " ++ show actual) $ actual == expected

capture :: String -> IO a -> IO (String,a)
capture input op = do
  dir <- getTemporaryDirectory
  (path,h) <- openTempFile dir "jfo.in"
  hPutStrLn h input
  hClose h
  
  (opath,oh) <- openTempFile dir "jfo.out"
  
  mystdout <- hDuplicate stdout
  mystdin <- hDuplicate stdin

  read <- openFile path ReadMode
  hDuplicateTo read stdin
  hDuplicateTo oh stdout
  
  -- TODO catch
  val <- op `finally` do
    hDuplicateTo mystdin stdin
    hDuplicateTo mystdout stdout
    hClose oh
  
  str <- readFile opath
  
  return (str,val)
  
runc string op = run (capture string op)
runc' op = run (capture "" op)

args = stdArgs {maxSize = 50, maxSuccess = 50}
main = testExs tests -- $forAllProperties (quickCheckWithResult args)

tests = [[prop_t1_hei]
        ,[prop_t2_tervehdi]
        ,[prop_t3_tervehdi']
        ,[prop_t4_lueSanat]
        ,[prop_t5_lueKunnes] -- 5
        ,[prop_t6_printFibs] -- 6
        ,[prop_t7_isums] -- 7
        ,[prop_t8_whenM_True, prop_t8_whenM_False] -- 8
        ,[prop_t9_while] -- 9
        ,[prop_t10_debug]
        ,[prop_t11_mapM_]
        ,[prop_t12_forM]
        ,[prop_t13_tuplaKutsu] -- 13
        ,[prop_t14_yhdista] -- 14
        ,[prop_t15_mkCounter]
        ,[prop_t16_hFetchLines]
        ,[prop_t17_readCSV]
        ,[prop_t18_compareFiles] -- 18
        ,[prop_t19_interact_terminates, prop_t19_interact_loop] -- 19
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

-- -- -- -- --

prop_t1_hei = monadicIO $ do
  (text,()) <- runc' hei
  stop $ text === "HEI\nMAAILMA\n"
  
word = listOf1 (choose ('a','z'))
  
prop_t2_tervehdi = monadicIO $ do
  nimi <- pick word
  (text,()) <- runc' $ tervehdi nimi
  stop $ text === ("HEI "++nimi++"\n")
  
prop_t3_tervehdi' =  monadicIO $ do
  nimi <- pick word
  (text,()) <- runc (nimi++"\n") tervehdi'
  stop $ text === ("HEI "++nimi++"\n")
  
prop_t4_lueSanat = monadicIO $ do
  sanat <- pick $ listOf1 word
  (_,ret) <- runc (unlines sanat) (lueSanat (length sanat - 1))
  stop $ ret === sort (init sanat)
  
prop_t5_lueKunnes = monadicIO $ do
  lopetus <- pick word
  sanat <- pick $ listOf1 (word `suchThat` (/=lopetus))
  let input = unlines $ sanat ++ [lopetus]
  (_,ret) <- runc input (lueKunnes (==lopetus))
  stop . printTestCase ("lueKunnes (==" ++ show lopetus ++ ")\nSyöte: "++show input) $
    ret === sanat
  
prop_t6_printFibs = monadicIO $ do
  n <- pick $ choose (0,50)
  (text,_) <- runc' $ printFibs n
  stop . printTestCase ("printFibs "++show n) $
    text === unlines (map show (take n fibs))
  where fibs = 1:1:zipWith (+) fibs (tail fibs)
  
prop_t7_isums = monadicIO $ do
  luvut <- pick . listOf1 $ choose (-10,10)
  let n = length luvut
  (text,ret) <- runc (unlines $ map show luvut) $ isums n
  stop . printTestCase ("isums "++show n) $
    conjoin [printTestCase "palautus" $ ret === sum luvut,
             printTestCase "tulostus" $ text === unlines (map show $ scanl1 (+) luvut)]
  
prop_t8_whenM_True = monadicIO $ do
  r <- run $ newIORef False
  let op = writeIORef r True
  let cond = return True
  run $ whenM cond op
  v <- run $ readIORef r
  stop $ printTestCase "whenM (return True)" $
    v
  
prop_t8_whenM_False = monadicIO $ do
  r <- run $ newIORef False
  let op = writeIORef r True
  let cond = return False
  run $ whenM cond op
  v <- run $ readIORef r
  stop $ printTestCase "whenM (return False)" $
    not v

prop_t9_while = monadicIO $ do
  i <- pick $ choose (0,10 :: Int)
  a <- run $ newIORef 0
  b <- run $ newIORef 0
  let ehto = modifyIORef a (+1) >> fmap (<=i) (readIORef a)
      op = modifyIORef b (+1)
  run $ while ehto op
  af <- run $ readIORef a
  bf <- run $ readIORef b
  stop $ printTestCase "while" $
    conjoin [printTestCase "ehdon kutsukerrat" $ af === i+1,
             printTestCase "operaation kutsukerrat" $ bf === i]

prop_t10_debug = monadicIO $ do
  sana <- pick word
  palautus <- pick word
  (text,ret) <- runc' $ debug sana (return palautus)
  stop $ printTestCase ("debug "++show sana++" (return "++show palautus++")") $
    conjoin [printTestCase "tulostus" $ text === (sana ++ "\n" ++ sana ++ "\n"),
             printTestCase "palautus" $ ret === palautus]
    
prop_t11_mapM_ = monadicIO $ do
  r <- run $ (newIORef [] :: IO (IORef [Int]))
  lis <- pick $ listOf1 arbitrary
  let op x = modifyIORef r (x:)
  run $ mymapM_ op lis
  ret <- run $ readIORef r
  stop $ printTestCase ("mapM op "++show lis) $ 
    ret === reverse lis
  
prop_t12_forM = monadicIO $ do
  r <- run $ (newIORef [] :: IO (IORef [Int]))
  lis <- pick $ listOf1 arbitrary
  let op x = do modifyIORef r (x:)
                return $ x+1
  ret <- run $ myforM lis op
  out <- run $ readIORef r
  stop $ printTestCase ("forM "++show lis++" op") $
    conjoin [printTestCase "palautus" $ ret === map (+1) lis,
             printTestCase "sivuvaikutukset" $ out === reverse lis]
  
prop_t13_tuplaKutsu = monadicIO $ do
  i <- pick $ (choose (0,20) :: Gen Int)
  let op = return (return i)
  out <- run $ tuplaKutsu $ op
  stop $ printTestCase ("tuplaKutsu (return (return "++show i++"))") $
    out === i
  
prop_t14_yhdista = monadicIO $ do
  i <- pick $ (choose (0,20) :: Gen Int)
  let op1 = return . (*2)
      op2 = return . (+1)
  out <- run $ yhdista op1 op2 i
  stop $ printTestCase "yhdista (return . (*2)) (return . (+1))" $
    out === (i+1)*2
  
prop_t15_mkCounter = monadicIO $ do
  n <- pick $ choose (0,20)
  m <- run $ do (i,g) <- mkCounter
                replicateM_ n i
                g
  
  stop $ m === n
  
prop_t16_hFetchLines = monadicIO $ do
  lines <- pick $ listOf1 word
  inds <- fmap (nub.sort) . pick . listOf1 $ choose (1,length lines)
  
  dir <- run $ getTemporaryDirectory
  (path,h) <- run $ openTempFile dir "hFetchLines.in"
  run $ hPutStr h $ unlines lines
  run $ hSeek h AbsoluteSeek 0
  
  outs <- run $ hFetchLines h inds

  stop $ printTestCase ("hFetchLines h "++show inds++"\nSisältö:\n"++unlines lines) $
    conjoin [outs !! j === lines !! (i-1) | (j,i) <- zip [0..] inds]
    
toCSV = unlines . map (intercalate ",") 
  
tmpSpit pattern conts = do
  dir <- getTemporaryDirectory
  (path,h) <- openTempFile dir pattern
  hPutStr h conts
  hClose h
  return path
        
prop_t17_readCSV = monadicIO $ do
  dat <- pick $ listOf1 (listOf1 word)
  let dat' = toCSV dat
  path <- run $ tmpSpit "readCSV.in" dat'
  ret <- run $ readCSV path
  stop $ printTestCase ("Tiedoston sisältö: "++show dat') $ ret === dat
  
prop_t18_compareFiles = monadicIO $ do
  alines <- pick $ listOf1 word
  lines2 <- pick $ vectorOf (length alines) word
  diffs <- pick $ fmap (nub.sort) $ listOf1 (choose (0,length alines-1))
  let blines = [ if elem i diffs then s1++s2 else s1 | (i,s1,s2) <- zip3 [0..] alines lines2]
      ac = unlines alines
      bc = unlines blines
      should = concatMap (\i -> ["< "++alines!!i,"> "++alines!!i++lines2!!i]) diffs
  path1 <- run $ tmpSpit "compareFilesA.in" ac
  path2 <- run $ tmpSpit "compareFilesB.in" bc
  (outp,()) <- runc' $ compareFiles path1 path2
  let ls = lines outp
  stop $ printTestCase ("compareFiles\nTiedoston A sisältö:\n"++ac++"Tiedoston B sisältö:\n"++bc) $
    conjoin [printTestCase "tulosterivien määrä" $ length ls === 2*length diffs,
             printTestCase "tulosterivit" $ ls === should]

prop_t19_interact_terminates = monadicIO $ do
  let f :: (String,String) -> (Bool,String,String)
      f (s,_) = (False,s,s)
  w <- pick $ word
  (text,ret) <- runc w $ interact' f ""
  stop $ conjoin [printTestCase "tulostus" $ text === w,
                  printTestCase "palautus" $ ret === w]

prop_t19_interact_loop = monadicIO $ do
  is <- pick $ listOf1 (arbitrary :: Gen Int)
  let f :: (String,[Int]) -> (Bool,String,[Int])
      f ("END",lis) = (False,"END\n", lis)
      f (x,lis)     = (True, "PICK\n", read x : lis) 
      eret = reverse $ 0:is
      etext = unlines $ replicate (length is) "PICK" ++ ["END"]
  (text,ret) <- runc (unlines $ map show is ++ ["END"]) $ interact' f [0]
  stop $ conjoin [printTestCase "tulostus" $ text === etext,
                  printTestCase "palautus" $ ret === eret]
