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

data MyRecord = MyRecord {name :: String, recId :: Int} deriving (Eq)

instance Show MyRecord where
  show MyRecord {name = name, recId = rcId} = "Hello, my id " ++ show rcId ++ " and my name " ++ name

helloRecord =
  let myRecord = MyRecord {name = "AAA", recId = 1}
   in do
        putStrLn $ name myRecord
        print myRecord
        print $ myRecordIncId myRecord

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
