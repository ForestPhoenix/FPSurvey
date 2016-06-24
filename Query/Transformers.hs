{-# LANGUAGE FunctionalDependencies #-}

module Query.Transformers (
    Grouping,
    groupedAs,
    groupedBs,
    unwrapGrouped,

    RightGrouped(),
    unRightGrouped,
    unsafeLiftGrouped,

    groupRight,
    unGroupRight,

    RightCollapsed(),
    unRightCollapsed,
    unsafeLiftRightCollapsed,

    collapseRight,
    fullCollapseRight,
    unCollapseRight,

    WeakCollapsed(),
    unWeakCollapsed,
    fromRightCollapsed,

    dataLeftJoin,
    fullDataLeftJoin
) where

import           Control.Category
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict    as M
import           Import

class Grouping t a b | t -> a b where
    groupedAs :: t -> [a]
    groupedBs :: t -> [b]
    unwrapGrouped :: t -> [(a, b)]
    unwrapGrouped = uncurry zip . (groupedAs &&& groupedBs)

newtype RightGrouped a b = RightGrouped { unRightGrouped :: [NonEmpty (a, b)] }
    deriving (Show, Eq)

instance Grouping (RightGrouped a b) (NonEmpty a) (NonEmpty b) where
    groupedAs = fmap (fmap fst) . unRightGrouped
    groupedBs = fmap (fmap snd) . unRightGrouped

unsafeLiftGrouped :: ([NonEmpty (a, b)] -> [NonEmpty (c, d)]) -> RightGrouped a b -> RightGrouped c d
unsafeLiftGrouped f = RightGrouped . f . unRightGrouped

groupRight :: Eq a => [(a, b)] -> RightGrouped a b
groupRight = RightGrouped <$> NE.groupBy (\a b -> fst a == fst b)

unGroupRight :: RightGrouped a b -> [(a, b)]
unGroupRight (RightGrouped xs) = mconcat $ NE.toList <$> xs


newtype RightCollapsed a b = RightCollapsed { unRightCollapsed :: [(a, NonEmpty b)] }
    deriving (Show, Eq)

instance Grouping (RightCollapsed a b) a (NonEmpty b) where
    groupedAs = fmap fst . unRightCollapsed
    groupedBs = fmap snd . unRightCollapsed

unsafeLiftRightCollapsed ::
    ([(a, NonEmpty b)] -> [(c, NonEmpty d)]) -> RightCollapsed a b -> RightCollapsed c d
unsafeLiftRightCollapsed f = RightCollapsed . f . unRightCollapsed

collapseRight :: RightGrouped a b -> RightCollapsed a b
collapseRight (RightGrouped xs) = RightCollapsed $
    (first NE.head . NE.unzip) <$> xs

fullCollapseRight :: Eq a => [(a, b)] -> RightCollapsed a b
fullCollapseRight = collapseRight . groupRight

unCollapseRight :: RightCollapsed a b -> RightGrouped a b
unCollapseRight (RightCollapsed xs) = RightGrouped $ (\(a, bs) -> NE.zip (NE.repeat a) bs) <$> xs


newtype WeakCollapsed a b = WeakCollapsed { unWeakCollapsed :: [(a, [b])] }
    deriving (Show, Eq)

instance Grouping (WeakCollapsed a b) a [b] where
    groupedAs = fmap fst . unWeakCollapsed
    groupedBs = fmap snd . unWeakCollapsed

fromRightCollapsed :: RightCollapsed a b -> WeakCollapsed a b
fromRightCollapsed = WeakCollapsed . fmap (second NE.toList) . unRightCollapsed

dataLeftJoin :: (Ord a) => RightCollapsed a b -> [a] -> WeakCollapsed a b
dataLeftJoin (RightCollapsed xs) as = WeakCollapsed $ lookupTuple xsMap <$> as
    where
        xsMap = M.fromList $ second NE.toList <$> xs
        lookupTuple cMap x = (,) x $ fromMaybe [] $ M.lookup x cMap

fullDataLeftJoin :: Ord a => [a] -> [(a, b)] -> WeakCollapsed a b
fullDataLeftJoin as = flip dataLeftJoin as . collapseRight . groupRight

--unsafeFastLeftJoin :: RightCollapsed a b -> [a] -> LeftJoined a b]
