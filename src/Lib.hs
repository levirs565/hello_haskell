module Lib
  ( someFunc,
    helloFunction,
    readAndPrint,
    doReadAndPrint,
    MyRecord (..),
    helloRecord,
    helloList,
    monadWithoutSeq,
    myRecordIncId,
    helloMyData,
  )
where

import Control.Monad.Reader

helloFunction :: String -> IO ()
helloFunction = putStrLn

readAndPrint =
  putStrLn "Write a line:"
    >> getLine
      >>= \x ->
        putStrLn "Write second line:"
          >> getLine
            >>= \y ->
              let sx = show x
                  sy = show y
               in putStrLn sx
                    >> putStrLn sy

-- keduanya ekuivalen
-- di atas dinamakan sequencing
-- ada dua sequencing
--  >>, menghiraukan output monad pertama
--  >>=, memasukkan output pertama
-- fungsi monad hanya akan dijalankan jika disequencing
-- saat di sequencing maka akan dijalankan dan ditunggu
-- hanya monad yang bisa disequencing
-- di atas adalah desugarred dan di bawah sugar syntax
-- semua kode di do akan dubah menjadi sequcing
-- jadi semua kode harus menghasilkan monad

doReadAndPrint = do
  putStrLn "Write a line: "
  x <- getLine
  putStrLn "Write second line:"
  y <- getLine
  let sx = show x
      sy = show y
  putStrLn sx
  putStrLn sy

data MyData x = NullData | OneData x | TwoData x x deriving (Show)

type MyDataStr = MyData String

data MyRecord = MyRecord {name :: String, recId :: Int} deriving (Eq)

class Equalable x where
  equal :: x -> x -> Bool
  notEqual :: x -> x -> Bool
  equal a b = not $ notEqual a b
  notEqual a b = not $ equal a b

instance (Eq x) => Equalable (MyData x) where
  NullData `equal` NullData = True
  (OneData a) `equal` (OneData b) = a == b
  (TwoData a1 a2) `equal` (TwoData b1 b2) = (a1 == b1) && (a2 == b2)
  _ `equal` _ = False

instance Functor MyData where
  fmap f NullData = NullData
  fmap f (OneData a) = OneData (f a)
  fmap f (TwoData a1 a2) = TwoData (f a1) (f a2)

instance Show MyRecord where
  show MyRecord {name = name, recId = rcId} = "Hello, my id " ++ show rcId ++ " and my name " ++ name

helloRecord =
  let myRecord = MyRecord {name = "AAA", recId = 1}
   in do
        putStrLn $ name myRecord
        print myRecord
        print $ myRecordIncId myRecord

helloMyData =
  let nullData = NullData :: MyDataStr
      oneData = OneData "Satu"
      twoDataBuilder = TwoData "A"
      twoData1 = twoDataBuilder "B"
      twoData2 = twoDataBuilder "C"
      myFunc = ("Hai " ++)
   in do
        putStrLn "Print nullData"
        print nullData
        printMyData nullData
        putStrLn "Print oneData"
        print oneData
        printMyData oneData
        putStrLn "Print twoData1"
        print twoData1
        printMyData twoData1
        putStrLn "Print twoData2"
        print twoData2
        printMyData twoData2
        putStrLn $ "nullData == nullData = " ++ show (equal nullData nullData)
        putStrLn $ "oneData == oneData = " ++ show (equal oneData oneData)
        putStrLn $ "twoData1 == twoData1 = " ++ show (equal twoData1 twoData1)
        putStrLn $ "twoData2 == twoData2 = " ++ show (equal twoData2 twoData2)
        putStrLn $ "nullData == oneData = " ++ show (equal nullData oneData)
        putStrLn $ "twoData1 == twoData2 = " ++ show (equal twoData1 twoData2)
        putStrLn "nullData fmap"
        printMyData $ fmap myFunc nullData
        putStrLn "oneData fmap"
        printMyData $ fmap myFunc oneData
        putStrLn "twoData1 fmap"
        printMyData $ fmap myFunc twoData1
  where
    printMyData :: (Show x) => MyData x -> IO ()
    printMyData NullData = putStrLn "Data is null"
    printMyData (OneData a1) = putStrLn $ "Data is " ++ show a1
    printMyData (TwoData a1 a2) = putStrLn $ "Data is " ++ show a1 ++ " and " ++ show a2

readMyRecordIncId :: Reader MyRecord Int
readMyRecordIncId = do
  record <- ask
  let myId = recId record
  return $ myId + 1

myRecordIncId :: MyRecord -> Int
myRecordIncId = runReader readMyRecordIncId

helloList =
  let myList = [1, 2, 3, 4, 5]
   in do
        print $ length myList
        print $ myList !! 3
        let mapped = flip map myList $ \x -> "My is: " ++ show x
        mapM_ putStrLn mapped

monadWithoutSeq = do
  -- this skipped until <-
  -- getLine tidak dijalankan tanpa sequencing (>>, >>=, <-)
  let ioY = getLine
  let ioX = getLine
  putStrLn "Write a line: "
  x <- ioX
  putStrLn "Write second line: "
  y <- ioY
  putStrLn "You write: "
  putStrLn x
  putStrLn y

someFunc :: IO ()
someFunc = putStrLn "someFunc"
