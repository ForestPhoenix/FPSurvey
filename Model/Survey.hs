module Model.Survey (
    Participant(..),
    Section(..),
    DisplayVariant(..),
    QType(..),
    QGroup(..),
    Question(..),
    Rating(..),

    UntakenSurvey,
    UntakenSurveySection,
    UntakenSurveyGroup,
    convertUntakenSurvey,
) where

import Import

import Database.HDBC as HDBC
import Data.Convertible

import Database.DbAbstraction

--TODO: convert model to HaskellDB

data DisplayVariant = 
    Radio |
    RadioInline |
    DropDown |
    IntegerInput |
    TextInput |
    RadioWith DisplayVariant
        deriving (Eq, Show, Read, Typeable)

instance Convertible String DisplayVariant where
    safeConvert "radio"         = Right Radio
    safeConvert "dropdown"      = Right DropDown
    safeConvert "integer_input" = Right IntegerInput
    safeConvert "text_input"    = Right TextInput
    safeConvert "radio_inline"  = Right RadioInline
    safeConvert (stripPrefix "radio_with_" -> Just rest)
                = RadioWith <$> safeConvert rest
    safeConvert str = convError "No DisplayVariant for Value" str

instance Convertible HDBC.SqlValue DisplayVariant where
    safeConvert sqlVal = (safeConvert sqlVal :: Either ConvertError String) >>= safeConvert


instance Convertible DisplayVariant String where
    safeConvert Radio = Right "radio"
    safeConvert DropDown = Right "dropdown"
    safeConvert IntegerInput = Right "integer_input"
    safeConvert TextInput = Right "text_input"
    safeConvert RadioInline = Right "radio_inline"
    safeConvert (RadioWith otherInput) = ((++) "radio_with_") <$> safeConvert otherInput


instance Convertible DisplayVariant HDBC.SqlValue where
    safeConvert value = HDBC.SqlString <$> safeConvert value
-- I use snake_case ONLY in this file for consistency with my SQL style
-- Foreign key columns do not seem relevant outside of SQL statements

data (SqlId a) => Participant a = Participant {
    participant_id :: a
} deriving (Eq, Show)

instance SqlRow Participant where
    safeFromSqlRow row = Participant <$>
        valueFromMap row "participant_id"

    toRowAL _ = []

    rowId = participant_id


data (SqlId a) => Section a = Section {
    section_id :: a,
    section_title :: String
} deriving (Eq, Show)

instance SqlRow Section where
    safeFromSqlRow row = Section <$>
        valueFromMap row "section_id" <*>
        convertFromRow row "section_title"

    toRowAL section = [("section_title", toSql $ section_title section)]

    rowId = section_id


data (SqlId a) => QType a = QType {
    qtype_id :: a,
    qtype_is_freeform :: Bool,
    qtype_display_variant :: DisplayVariant
} deriving (Eq, Show)

instance SqlRow QType where
    safeFromSqlRow row = QType <$>
        valueFromMap row "qtype_id" <*>
        convertFromRow row "qtype_is_freeform" <*>
        convertFromRow row "qtype_display_variant"

    toRowAL qtype = [
        ("qtype_is_freeform", toSql $ qtype_is_freeform qtype),
        ("qtype_display_variant", toSql $ qtype_display_variant qtype)
        ]

    rowId = qtype_id


data (SqlId a) => QGroup a = QGroup {
    qgroup_id :: a,
    qgroup_header :: String
} deriving (Eq, Show)

instance SqlRow QGroup where
    safeFromSqlRow row = QGroup <$>
        valueFromMap row "qgroup_id" <*>
        convertFromRow row "qgroup_header"

    toRowAL qgroup = [("qgroup_header", toSql $ qgroup_header qgroup)]

    rowId = qgroup_id


data (SqlId a) => Question a = Question {
    question_id :: a,
    question_text :: String
} deriving (Eq, Show)

instance SqlRow Question where
    safeFromSqlRow row = Question <$>
        valueFromMap row "question_id" <*>
        convertFromRow row "question_text"

    toRowAL question = [("question_text", toSql $ question_text question)]

    rowId = question_id


data (SqlId a) => Rating a = Rating {
    rating_id :: a,
    rating_value :: String
} deriving (Eq, Show)

instance SqlRow Rating where
    safeFromSqlRow row = Rating <$>
        valueFromMap row "rating_id" <*>
        convertFromRow row "rating_value"

    toRowAL rating = [("rating_value", toSql $ rating_value rating)]

    rowId = rating_id

data (SqlId a) => Answer a = Answer {
    answer_id :: a
} deriving (Eq, Show)

instance SqlRow Answer where
    safeFromSqlRow row = Answer <$>
        valueFromMap row "answer_id"

    toRowAL _ = []

    rowId = answer_id

type UntakenSurvey a = [ UntakenSurveySection a ]

type UntakenSurveySection a = ( Section a, [UntakenSurveyGroup a] )

type UntakenSurveyGroup a = ( QGroup a, QType a, [Question a], [Rating a] )

convertUntakenSurvey :: (SqlId a) =>
    [((Section a, QGroup a), Question a)] ->
    [(QGroup a, QType a)] ->
    [(QGroup a, Rating a)] ->
    UntakenSurvey a

convertUntakenSurvey sections types ratings = mergeLeft $ zip preMergedSections groups
    where
        groups = zip4 mergedGroups
            joinedTypes
            questions
            joinedRatings
        joinedTypes = joinReplicate mergedGroups (snd $ unzip types)
        joinedRatings = joinLookup mergedGroups mergedRatings
        mergedRatings = mergeLeft ratings
        (preMergedSections, mergedGroups) = unzip mergedSectionsAndGroups
        (mergedSectionsAndGroups, questions) = unzip $ mergeLeft sections
