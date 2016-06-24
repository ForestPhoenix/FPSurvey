module Query.Types where
import Prelude

import Text.Read
import Control.Exception

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Ok
import Opaleye.Internal.RunQuery

import Debug.Trace

readOk :: Read a => String -> Ok a
readOk dat = trace ("trace1 " ++ show dat) $ case readMaybe dat of
    Just a -> Ok a
    _ -> Errors [toException $ PatternMatchFail "could not read FreeField"]

newtype EnumField a = EnumField { unEnumField :: a} deriving (Show, Eq, Ord)
data PGEnumField a

instance (Read a, Show a) => FromField (EnumField a) where
    fromField f bs = conversionMap (>>= fmap EnumField . readOk) $ fromField f bs
instance (Read a, Show a) => ToField (EnumField a) where
    toField = toField . show . unEnumField
instance (Read a, Show a) => QueryRunnerColumnDefault (PGEnumField a) (EnumField a) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

data FreeField =
    FreeText |
    FreeInteger |
    NoFreeField
    deriving (Read, Show, Eq, Ord)

data QuestionDisplay =
    Empty |
    RadioInline | RadioTable |
    DropDown
    deriving (Read, Show, Eq, Ord)

data Scale = NominalScale | OrdinalScale | IntervalScale | RatioScale
    deriving (Read, Show, Eq, Ord)
