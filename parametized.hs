import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show , Eq , Ord , Enum)

allOrgans:: [Organ]
allOrgans = [Heart ..Kidney]

ids:: [Int]
ids = [2,7,13,14,21,24]

values :: [Organ]
values = map snd (Map.toList organCatalog)

-- Organ pairs created by using zip --

organPairs :: [(Int,Organ)]
organPairs = zip ids allOrgans

-- Put together an organ catalog --

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Now you can look up by using Map.lookup index organCatalog --

data Triple a = Triple a a a deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3


type FullName = Triple String
type Initials = Triple Char

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "LoveCraft"

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a 
third (Triple _ _ x) = x

data Box a = Box a deriving Show

wrap:: a -> Box a
wrap x = Box x

unWrap :: Box a -> a
unWrap (Box x) = x

-- Chapter objective functions --

-- Q18.1 --

boxMap :: ( a -> b) -> Box a -> Box b
boxMap func (Box val) = Box (func val)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap func (Triple v1 v2 v3) = Triple (func v1) (func v2) (func v3)

-- Q18.2 --

organCounts :: [Int]
organCounts = map countOrgan allOrgans
   where
       countOrgan = (\organ->
                      (length. filter (== organ))values)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)