module Database.DbAbstraction (
    SqlId,
    NoId(..),

    SqlRow,
    safeFromSqlRow,
    toRowAL,
    rowId,

    valueFromMap,
    convertFromRow,
    notFoundInMapMsg,
    queryAllRowsMap,
    mergeLeft,
    joinLookup,
    joinReplicate,
    foldFunctor,
    shiftApplicativeList,
) where

import Import

import qualified Data.Map as Map
import qualified Data.List as List

import Database.HDBC
import Data.Convertible

data NoId = NoId deriving (Show, Eq)

-- TODO: convert to HaskellDB, remove clutter

class (Show a, Eq a) => SqlId a
instance SqlId NoId
instance SqlId SqlValue

class SqlRow a where -- restrict type parameters for a to sqlId ?
    safeFromSqlRow :: Map.Map String SqlValue -> Either String (a SqlValue)
    -- nope, no unsafe version.
    toRowAL :: a NoId -> [(String, SqlValue)] -- Is this even still useful?
    rowId :: (SqlId i) => a i -> i

notFoundInMapMsg :: (Show a) => Map.Map a b -> a -> String
notFoundInMapMsg vals field =
    "Could not find field " ++ (show field) ++ " in map with keys: " ++ (show $ Map.keys vals)

queryAllRowsMap :: (IConnection conn) => String -> [SqlValue] -> conn -> IO [Map.Map String SqlValue]
queryAllRowsMap queryString params conn = do
    stmt <- prepare conn queryString
    _ <- execute stmt params
    fetchAllRowsMap' stmt

valueFromMap :: Map.Map String v -> String -> Either String v
valueFromMap vals field =
    case Map.lookup field vals of
         Just x  -> Right x
         Nothing -> Left $ notFoundInMapMsg vals field

convertFromRow :: (Show v, Convertible v a) => Map.Map String v -> String -> Either String a
convertFromRow vals field =
    (valueFromMap vals field) >>= \fromMap -> (first show (safeConvert fromMap))

mergeLeft :: (Eq a) => [(a, b)] -> [(a, [b])]
mergeLeft []            = []
mergeLeft [(a, b)]      = [(a, [b])]
mergeLeft ((k, v):rest) = (k, values): mergeLeft unequal
    where
        (_, values) = unzip equal
        (equal, unequal) = List.span (\(k', _) -> k' == k) ((k, v):rest)

joinReplicate :: (Eq a) => [a] -> [b] -> [b]
joinReplicate [] _ = []
joinReplicate _ [] = []
joinReplicate (a0:a1:as) (b0:bs)
    | a0 == a1  = b0: joinReplicate (a1:as) (b0:bs)
    | otherwise = b0: joinReplicate (a1:as) (bs)
joinReplicate (_:[]) (b0:_) = [b0]

joinLookup :: (Eq a) => [a] -> [(a, [b])] -> [[b]]
joinLookup as asbs = map (\x -> fromMaybe [] $ lookup x asbs) as

foldFunctor :: (Applicative f) => [f a] -> f [a]
foldFunctor = foldr (\ l r -> ((:) <$> l <*> r)) (pure [])

shiftApplicativeList :: (Applicative f) => [f [a]] -> f [a]
shiftApplicativeList = foldr (\ l r -> ((++) <$> l <*> r)) (pure [])
