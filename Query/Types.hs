module Query.Types where

import           Prelude

import           Control.Arrow
import           Control.Exception
import           Data.List
import           Data.Maybe
import           Text.Read

import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.ToField
import           Opaleye.Internal.RunQuery

readOk :: Read a => String -> Ok a
readOk dat = case readMaybe dat of
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
    RadioInline | RadioTable | RadioLines
    deriving (Read, Show, Eq, Ord)

data Scale = NominalScale | OrdinalScale | IntervalScale | RatioScale
    deriving (Read, Show, Eq, Ord)


data InputWithOther a b = InputData a | InputOther b

isData :: InputWithOther a b -> Bool
isData (InputData _) = True
isData _ = False

isOther :: InputWithOther a b -> Bool
isOther (InputOther _) = True
isOther _ = False
