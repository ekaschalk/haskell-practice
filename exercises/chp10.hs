import           Data.Time

data DBItem = DBString String
            | DBNumber Integer
            | DBDate UTCTime
            deriving (Eq, Ord, Show)

theDB :: [DBItem]
theDB =
  [ DBDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DBString "Hello, world!"
  , DBDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDBDate :: [DBItem] -> [UTCTime]
filterDBDate = map toDate . filter isDate
  where toDate (DBDate x) = x
        isDate (DBDate x) = True
        isDate _          = False

filterDBNumber :: [DBItem] -> [Integer]
filterDBNumber = map toNum . filter isNum
  where toNum (DBNumber x) = x
        isNum (DBNumber x) = True
        isNum _          = False

mostRecent :: [DBItem] -> UTCTime
mostRecent = foldr1 max . filterDBDate

sumDB :: [DBItem] -> Integer
sumDB = sum . filterDBNumber

avgDB :: [DBItem] -> Double
avgDB x = (/) (fromIntegral . sumDB $ x) $
          fromIntegral . length . filterDBNumber $ x

myOr :: [Bool] -> Bool
myOr = foldr1 (||)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr1 (||) . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []
