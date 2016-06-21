module Query.Transformers (
    RightGrouped(),
    unsafeLiftGrouped,
    groupRight,
    unGroupRight,
    unRightGrouped,
    groupedAs,
    groupedBs,

    RightCollapsed(),
    unsafeLiftRightCollapsed,
    collapseRight,
    unCollapseRight,
    unRightCollapsed,
    collapsedAs,
    collapsedBs,
    unwrapRightCollapsed,

    leftJoin
) where

import           Data.List.NonEmpty as NE
import           Import
import Data.Map.Strict as M

-- arrayAgg

newtype RightGrouped a b = RightGrouped { unRightGrouped :: [NonEmpty (a, b)] }
    deriving (Show, Eq)

unsafeLiftGrouped :: ([NonEmpty (a, b)] -> [NonEmpty (c, d)]) -> RightGrouped a b -> RightGrouped c d
unsafeLiftGrouped f = RightGrouped . f . unRightGrouped

groupRight :: Eq a => [(a, b)] -> RightGrouped a b
groupRight = RightGrouped <$> NE.groupBy (\a b -> fst a == fst b)

unGroupRight :: RightGrouped a b -> [(a, b)]
unGroupRight (RightGrouped xs) = mconcat $ NE.toList <$> xs

groupedAs :: RightGrouped a b -> [NonEmpty a]
groupedAs = fmap (fmap fst) . unRightGrouped

groupedBs :: RightGrouped a b -> [NonEmpty b]
groupedBs = fmap (fmap snd) . unRightGrouped


newtype RightCollapsed a b = RightCollapsed { unRightCollapsed :: [(a, NonEmpty b)] }
    deriving (Show, Eq)

unsafeLiftRightCollapsed ::
    ([(a, NonEmpty b)] -> [(c, NonEmpty d)]) -> RightCollapsed a b -> RightCollapsed c d
unsafeLiftRightCollapsed f = RightCollapsed . f . unRightCollapsed

collapseRight :: RightGrouped a b -> RightCollapsed a b
collapseRight (RightGrouped xs) = RightCollapsed $
    (first NE.head . NE.unzip) <$> xs

unCollapseRight :: RightCollapsed a b -> RightGrouped a b
unCollapseRight (RightCollapsed xs) = RightGrouped $ (\(a, bs) -> NE.zip (NE.repeat a) bs) <$> xs

collapsedAs :: RightCollapsed a b -> [a]
collapsedAs = fmap fst . unRightCollapsed

collapsedBs :: RightCollapsed a b -> [NonEmpty b]
collapsedBs = fmap snd . unRightCollapsed

unwrapRightCollapsed :: RightCollapsed a b -> [(a, [b])]
unwrapRightCollapsed = fmap (second NE.toList) . unRightCollapsed

leftJoin :: (Ord a) => RightCollapsed a b -> [a] -> [(a, [b])]
leftJoin (RightCollapsed xs) as = lookupTuple xsMap <$> as
    where
        xsMap = M.fromList $ second NE.toList <$> xs
        lookupTuple cMap x = (,) x $ fromMaybe [] $ M.lookup x cMap
