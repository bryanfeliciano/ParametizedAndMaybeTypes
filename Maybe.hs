import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show , Eq , Ord , Enum)

data Container = Vat Organ | Cooler Organ | Bag Organ

allOrgans:: [Organ]
allOrgans = [Heart ..Kidney]

ids:: [Int]
ids = [2,7,13,14,21,24]

values :: [Organ]
values = map snd (Map.toList organCatalog)

organPairs :: [(Int,Organ)]
organPairs = zip ids allOrgans

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents  ids catalog = map getContents ids
  where
    getContents = (\id -> Map.lookup id catalog)

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length ( filter (\x -> x == Just organ) available)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ

organList :: [String]
organList = map showOrgan justTheOrgans

instance Show Container where
    show (Vat organ) = show organ ++ "In a vat"
    show (Cooler organ) = show organ ++ "In a cooler"
    show (Bag organ) = show organ ++ "In a bag"