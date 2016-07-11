module Query.Model where

import Import
import Opaleye as OE
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Query.Types

-- table participant

data Participant' a = Participant {
    participantId :: a
} deriving (Show, Eq)
$(makeAdaptorAndInstance "pParticipant" ''Participant')

newtype ParticipantId' a = ParticipantId { unParticipantId :: a } deriving (Show, Eq)
$(makeAdaptorAndInstance "pParticipantId" ''ParticipantId')

type ParticipantIdColumn = (ParticipantId' (Column PGInt4))
type ParticipantWriteIdColumn = (ParticipantId' (Maybe (Column PGInt4)))

type ParticipantColumns = Participant' ParticipantIdColumn
type ParticipantWriteColumns = Participant' ParticipantWriteIdColumn
type ParticipantData = Participant' (ParticipantId' Int)

participantTable :: Table
    ParticipantWriteColumns
    ParticipantColumns
participantTable = Table "participant" $
    pParticipant Participant {
        participantId    = pParticipantId . ParticipantId $ OE.optional "participant_id"
    }

-- table section

data Section' a b c = Section {
    sectionId :: a,
    sectionSort :: b,
    sectionTitle :: c
} deriving (Show, Eq, Ord)
$(makeAdaptorAndInstance "pSection" ''Section')

newtype SectionId' a = SectionId { unSectionId :: a } deriving (Show, Eq, Ord)
$(makeAdaptorAndInstance "pSectionId" ''SectionId')

type SectionIdColumn = SectionId' (Column PGInt4)
type SectionWriteIdColumn = (SectionId' (Maybe (Column PGInt4)))

type SectionColumns = (Section' (SectionId' (Column PGInt4)) (Column PGInt4) (Column PGText))
type SectionData = Section' (SectionId' Int) Int String

sectionTable :: Table
    (Section' (SectionId' (Maybe (Column PGInt4))) (Column PGInt4) (Column PGText))
    SectionColumns
sectionTable = Table "section" $
    pSection Section {
        sectionId    = pSectionId . SectionId $ OE.optional "section_id",
        sectionSort  = required "section_sort",
        sectionTitle = required "section_title"
    }

-- table qgroup

data Qgroup' a b c d e f g = Qgroup {
    qgroupId :: a,
    qgroupSort :: b,
    qgroupHeader :: c,
    qgroupScale :: d,
    qgroupQuestionDisplay :: e,
    qgroupFreeField :: f,
    qgroupSectionId :: g
} deriving (Show, Eq, Ord)
$(makeAdaptorAndInstance "pQgroup" ''Qgroup')

newtype QgroupId' a = QgroupId { unQgroupId :: a } deriving (Show, Eq, Ord)
$(makeAdaptorAndInstance "pQgroupId" ''QgroupId')

type QgroupIdColumn = (QgroupId' (Column PGInt4))
type QgroupWriteIdColumn = (QgroupId' (Maybe (Column PGInt4)))

type QgroupColumnsWrapped w = Qgroup'
    (QgroupId' (w PGInt4))
    (w PGInt4)
    (w PGText)
    (w (PGEnumField Scale))
    (w (PGEnumField QuestionDisplay))
    (w (PGEnumField FreeField))
    (SectionId' (w PGInt4))
type QgroupColumns = QgroupColumnsWrapped Column

type QgroupData = Qgroup'
    (QgroupId' Int)
    Int
    Text
    (EnumField Scale)
    (EnumField QuestionDisplay)
    (EnumField FreeField)
    (SectionId' Int)

qgroupTable :: Table
    (Qgroup'
        QgroupWriteIdColumn
        (Column PGInt4)
        (Column PGText)
        (Column (PGEnumField Scale))
        (Column (PGEnumField QuestionDisplay))
        (Column (PGEnumField FreeField))
        SectionIdColumn)
    QgroupColumns
qgroupTable = Table "qgroup" $
    pQgroup Qgroup {
        qgroupId                  = pQgroupId  . QgroupId  $ OE.optional "qgroup_id",
        qgroupSort                = required "qgroup_sort",
        qgroupHeader              = required "qgroup_header",
        qgroupScale               = required "qgroup_scale",
        qgroupQuestionDisplay     = required "qgroup_questiondisplay",
        qgroupFreeField           = required "qgroup_freefield",
        qgroupSectionId = pSectionId . SectionId $ required "qgroup_section_id"
    }

-- table question

data Question' a b c d = Question {
    questionId :: a,
    questionSort :: b,
    questionText :: c,
    questionQgroupId :: d
} deriving (Show, Eq)
$(makeAdaptorAndInstance "pQuestion" ''Question')

newtype QuestionId' a = QuestionId { unQuestionId :: a } deriving (Show, Eq)
$(makeAdaptorAndInstance "pQuestionId" ''QuestionId')

type QuestionIdColumn = QuestionId' (Column PGInt4)
type QuestionWriteIdColumn = (QuestionId' (Maybe (Column PGInt4)))

type QuestionColumnsWrapped w = Question'
    (QuestionId' (w PGInt4))
    (w PGInt4)
    (w PGText)
    (QgroupId' (w PGInt4))
type QuestionColumns = QuestionColumnsWrapped Column

type QuestionDataWrapped w = Question'
    (QuestionId' (w Int))
    (w Int)
    (w Text)
    (QgroupId' (w Int))
type QuestionData = Question'
    (QuestionId' Int)
    Int
    Text
    (QgroupId' Int)

questionTable :: Table
    (Question' QuestionWriteIdColumn (Column PGInt4) (Column PGText) QgroupIdColumn)
    QuestionColumns
questionTable = Table "question" $
    pQuestion Question {
        questionId       = pQuestionId . QuestionId $ OE.optional "question_id",
        questionSort     = required "question_sort",
        questionText     = required "question_text",
        questionQgroupId = pQgroupId   . QgroupId   $ required "question_qgroup_id"
    }

-- table rating

data Rating' a b c d e = Rating {
    ratingId :: a,
    ratingSort :: b,
    ratingValue :: c,
    ratingDevGiven :: d,
    ratingQgroupId :: e
} deriving (Show, Eq)
$(makeAdaptorAndInstance "pRating" ''Rating')

newtype RatingId' a = RatingId { unRatingId :: a } deriving (Show, Eq)
$(makeAdaptorAndInstance "pRatingId" ''RatingId')

type RatingIdColumn = RatingId' (Column PGInt4)
type RatingWriteIdColumn = (RatingId' (Maybe (Column PGInt4)))

type RatingColumnsWrapped w = Rating'
    (RatingId' (w PGInt4))
    (w PGInt4)
    (w PGText)
    (w PGBool)
    (QgroupId' (w PGInt4))
type RatingColumns = RatingColumnsWrapped Column

type RatingWriteColumns = Rating'
    RatingWriteIdColumn (
    Column PGInt4)
    (Column PGText)
    (Column PGBool) QgroupIdColumn

type RatingDataWrapped w = Rating'
    (RatingId' (w Int))
    (w Int)
    (w Text)
    (w Bool)
    (QgroupId' (w Int))
type RatingData = Rating'
    (RatingId' Int)
    Int
    Text
    Bool
    (QgroupId' Int)

ratingTable :: Table
    RatingWriteColumns
    RatingColumns
ratingTable = Table "rating" $
    pRating Rating {
        ratingId       = pRatingId . RatingId $ OE.optional "rating_id",
        ratingSort     = required "rating_sort",
        ratingValue    = required "rating_value",
        ratingDevGiven = required "rating_dev_given",
        ratingQgroupId = pQgroupId . QgroupId $ required "rating_qgroup_id"
    }

-- table answer

data Answer' a b c d = Answer {
    answerId :: a,
    answerRatingId :: b,
    answerQuestionId :: c,
    answerParticipantId :: d
} deriving (Show, Eq)
$(makeAdaptorAndInstance "pAnswer" ''Answer')

newtype AnswerId' a = AnswerId { unAnswerId :: a } deriving (Show, Eq)
$(makeAdaptorAndInstance "pAnswerId" ''AnswerId')

type AnswerIdColumn = AnswerId' (Column PGInt4)
type AnswerWriteIdColumn = (AnswerId' (Maybe (Column PGInt4)))

type AnswerWriteColumns = Answer'
    (AnswerId' (Maybe (Column PGInt4)))
    RatingIdColumn
    QuestionIdColumn
    ParticipantIdColumn

answerTable :: Table
    AnswerWriteColumns
    (Answer' AnswerIdColumn RatingIdColumn QuestionIdColumn ParticipantIdColumn)
answerTable = Table "answer" $
    pAnswer Answer {
        answerId            = pAnswerId      . AnswerId      $ OE.optional "answer_id",
        answerRatingId      = pRatingId      . RatingId      $ required "answer_rating_id",
        answerQuestionId    = pQuestionId    . QuestionId    $ required "answer_question_id",
        answerParticipantId = pParticipantId . ParticipantId $ required "answer_participant_id"
    }

-- type synonyms

type TakeSurvey = [(
        SectionData, [(
            QgroupData,
            [QuestionData],
            [RatingData]
        )]
    )]

type SurveyInput = InputWithOther RatingData Text
