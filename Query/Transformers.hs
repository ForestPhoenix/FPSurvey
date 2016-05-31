module Query.Transformers where

import Import
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D

import qualified Opaleye as O
import qualified Opaleye.Internal.PGTypes as OIP
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

emptyArray :: O.Column (O.PGArray a)
emptyArray = OIP.literalColumn $ HPQ.StringLit "{}"

-- arrayAgg

allArrayAgg :: D.Default ArrayAgg a b => O.Aggregator a b
allArrayAgg = unArrayAgg D.def

newtype ArrayAgg a b = ArrayAgg { unArrayAgg :: (O.Aggregator a b) }

instance Profunctor ArrayAgg where
    dimap l r (ArrayAgg aggr) = ArrayAgg $ dimap l r aggr

instance PP.ProductProfunctor ArrayAgg where
    empty = ArrayAgg PP.empty
    (***!) (ArrayAgg a) (ArrayAgg b) = ArrayAgg (a PP.***! b)

instance D.Default ArrayAgg (O.Column a) (O.Column (O.PGArray a)) where
    def = ArrayAgg O.arrayAgg

-- groupBy

allGroupBy :: D.Default GroupBy a b => O.Aggregator a b
allGroupBy = unGroupBy D.def

newtype GroupBy a b = GroupBy { unGroupBy :: (O.Aggregator a b) }

instance Profunctor GroupBy where
    dimap l r (GroupBy aggr) = GroupBy $ dimap l r aggr

instance PP.ProductProfunctor GroupBy where
    empty = GroupBy PP.empty
    (***!) (GroupBy a) (GroupBy b) = GroupBy (a PP.***! b)

instance D.Default GroupBy (O.Column a) (O.Column a) where
    def = GroupBy O.groupBy

-- fromNullable emptyArray

allFromNullableEmptyArray :: D.Default FromNullableEmptyArray a b => a -> b
allFromNullableEmptyArray = unFromNullableEmptyArray D.def

newtype FromNullableEmptyArray a b = FromNullableEmptyArray { unFromNullableEmptyArray :: a -> b }

instance Profunctor FromNullableEmptyArray where
    dimap l r (FromNullableEmptyArray f) = FromNullableEmptyArray $ dimap l r f

instance PP.ProductProfunctor FromNullableEmptyArray where
    empty = FromNullableEmptyArray PP.empty
    (***!) (FromNullableEmptyArray a) (FromNullableEmptyArray b) = FromNullableEmptyArray (a PP.***! b)

instance D.Default FromNullableEmptyArray (O.Column (O.Nullable (O.PGArray a))) (O.Column (O.PGArray a)) where
    def = FromNullableEmptyArray $ O.fromNullable emptyArray
