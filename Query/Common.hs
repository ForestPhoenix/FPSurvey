{-# LANGUAGE ScopedTypeVariables #-}

module Query.Common where

import Prelude -- not using Import, because Query conflicts
import Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

import Opaleye

import Query.Transformers

type ColNull a = Column (Nullable a)
type ColNullArr a = Column (Nullable (PGArray a))
type ColNullArrArr a = Column (Nullable (PGArray (PGArray a)))

type ColArr a = Column (PGArray a)
type ColArrArr a = Column (PGArray (PGArray a))

arrayAggSnd :: (D.Default ArrayAgg b c, D.Default GroupBy a a) =>
    Query (a, b) -> Query (a, c)
arrayAggSnd = aggregate (p2
    (allGroupBy,
    allArrayAgg))
