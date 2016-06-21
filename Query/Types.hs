module Query.Types where
import Prelude

import Text.Read
import Control.Exception

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Ok
import Opaleye.Internal.RunQuery

readOk :: Read a => String -> String -> Ok a
readOk dat err = case readMaybe dat of
    Just a -> Ok a
    _ -> Errors [toException $ PatternMatchFail err]

newtype EnumField a = EnumField { unEnumField :: a}
data PGEnumField a

instance (Read a, Show a) => FromField (EnumField a) where
    fromField f bs = conversionMap (>>= fmap EnumField . readOk "could not read FreeField") $ fromField f bs
instance (Read a, Show a) => ToField (EnumField a) where
    toField = toField . show . unEnumField
instance (Read a, Show a) => QueryRunnerColumnDefault (PGEnumField a) (EnumField a) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

data FreeField =
    FreeText |
    FreeInteger
    deriving (Read, Show, Eq)

data QuestionDisplay =
    Empty |
    RadioInline | RadioTable |
    DropDown
    deriving (Read, Show, Eq)

data Scale = MetricScale | OrdinalScale | IntervalScale | RatioScale
    deriving (Read, Show, Eq, Ord)
