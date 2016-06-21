module Query.TransformersSpec (spec) where

import           Query.Transformers
import           TestImport
import qualified Data.List as L

spec :: Spec
spec = do
    describe "groupRight" $ do
        it "unGroupRight is its inverse" $ property $
            \(xs :: [(A, B)]) ->
            (unGroupRight . groupRight) xs == xs
        it "stays sorted if the input 'a's was sorted" $ property $
            \(xs :: [(OrdA, B)]) ->
            (unGroupRight . groupRight) (sortFirst xs) == sortFirst xs
        it "is the inverse of unGroupRight if all equal as are adjacent" $ property $
            \(xs :: [(OrdA, B)]) ->
            (groupRight . unGroupRight . groupRight . sortFirst) xs == (groupRight . sortFirst) xs

    describe "collapseRight" $ do
        it "unCollapseRight is its inverse if RightGroup is valid" $ property $
            \(xs :: [(A, B)]) ->
            (unGroupRight . unCollapseRight . collapseRight . groupRight) xs == xs
        it "has no duplicate 'a' if the input was sorted" $ property $
            \(xs :: [(OrdA, B)]) ->
            collapseSort xs == collapseNub (sortFirst xs)
        it "is the inverse of unCollapseRight if there are no duplicate 'a's" $ property $
            \(xs :: [(OrdA, B)]) ->
            (collapseRight . unCollapseRight . collapseSort) xs == collapseSort xs

    describe "leftJoin coll as" $ do
        it "is id when 'as = collapsedAs coll' and no duplicate as are in 'coll'" $ property $
            \(xs :: [(OrdA, B)]) ->
            let collapsed = collapseNub xs in
            leftJoin collapsed (collapsedAs collapsed) == unwrapRightCollapsed collapsed
        it "preserves the as in its structure" $ property $
            \(xs :: [(OrdA, B)], as :: [OrdA]) ->
            let collapsed = (collapseRight . groupRight) xs in
            (fst <$> leftJoin collapsed as) == as

collapseNub :: Eq a => [(a, b)] -> RightCollapsed a b
collapseNub xs = unsafeLiftRightCollapsed (L.nubBy (\a b -> fst a == fst b)) $
    (collapseRight . groupRight) xs

sortFirst :: Ord a => [(a, b)] -> [(a, b)]
sortFirst = sortBy (\a b -> compare (fst a) (fst b))

collapseSort :: Ord a => [(a, b)] -> RightCollapsed a b
collapseSort = collapseRight . groupRight . sortFirst
