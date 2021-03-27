module Lib
  ( someFunc,
    helloFunction,
    readAndPrint,
    doReadAndPrint,
    MyRecord (..),
    helloRecord,
    helloList,
    monadWithoutSeq,
  )
where

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
        putStrLn $ show myRecord

helloList =
  let myList = [1, 2, 3, 4, 5]
   in do
        putStrLn $ show $ length myList
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
