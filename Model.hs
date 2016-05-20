module Model where

import Import
import Opaleye as OE
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

-- table participant

data Participant' a b = Participant {
    participantId :: a,
    participantToken :: b
} deriving (Show, Eq)
$(makeAdaptorAndInstance "pParticipant" ''Participant')

newtype ParticipantId' a = ParticipantId { unParticipantId :: a } deriving (Show, Eq)
$(makeAdaptorAndInstance "pParticipantId" ''ParticipantId')
    
type ParticipantIdColumn = (ParticipantId' (Column PGInt4))
type ParticipantWriteIdColumn = (ParticipantId' (Maybe (Column PGInt4)))

participantTable :: Table
    (Participant' ParticipantWriteIdColumn (Column PGText))
    (Participant' ParticipantIdColumn (Column PGText))
participantTable = Table "participant" $
    pParticipant Participant {
        participantId    = pParticipantId . ParticipantId $ OE.optional "participant_id",
        participantToken = required "participant_token"
    }

zipParticipant :: Participant' (ParticipantId' [a]) [b] -> [Participant' (ParticipantId' a) b]
zipParticipant (Participant a b) = zipWith Participant (fmap ParticipantId $ unParticipantId a) b

-- table section

data Section' a b c = Section {
    sectionId :: a,
    sectionSort :: b,
    sectionTitle :: c
} deriving (Show, Eq)
$(makeAdaptorAndInstance "pSection" ''Section')

newtype SectionId' a = SectionId { unSectionId :: a } deriving (Show, Eq)
$(makeAdaptorAndInstance "pSectionId" ''SectionId')

type SectionIdColumn = SectionId' (Column PGInt4)
type SectionWriteIdColumn = (SectionId' (Maybe (Column PGInt4)))

type SectionColsWrapped w = (Section' SectionIdColumn (w PGInt4) (w PGText))
type SectionColumns = SectionColsWrapped Column

sectionTable :: Table 
    (Section' (SectionId' (Maybe (Column PGInt4))) (Column PGInt4) (Column PGText))
    SectionColumns
sectionTable = Table "section" $
    pSection Section {
        sectionId    = pSectionId . SectionId $ OE.optional "section_id",
        sectionSort  = required "section_sort",
        sectionTitle = required "section_title"
    }

zipSection :: Section' (SectionId' [a]) [b] [c] -> [Section' (SectionId' a) b c]
zipSection (Section a b c) = zipWith3 Section (fmap SectionId $ unSectionId a) b c

-- table qtype

data Qtype' a b = Qtype {
    qtypeId :: a,
    qtypeDisplayVariant :: b
} deriving (Show, Eq)
$(makeAdaptorAndInstance "pQtype" ''Qtype')

newtype QtypeId' a = QtypeId { unQtypeId :: a } deriving (Show, Eq)
$(makeAdaptorAndInstance "pQtypeId" ''QtypeId')

type QtypeIdColumn = QtypeId' (Column PGInt4)
type QtypeWriteIdColumn = (QtypeId' (Maybe (Column PGInt4)))

qtypeTable :: Table
    (Qtype' QtypeWriteIdColumn (Column PGText))
    (Qtype' QtypeIdColumn (Column PGText))
qtypeTable = Table "qtype" $
    pQtype Qtype {
        qtypeId             = pQtypeId . QtypeId $ OE.optional "qtype_id",
        qtypeDisplayVariant = required "qtype_display_variant"
    }

zipQtype :: Qtype' (QtypeId' [a]) [b] -> [Qtype' (QtypeId' a) b]
zipQtype (Qtype a b) = zipWith Qtype (fmap QtypeId $ unQtypeId a) b

-- table qgroup

data Qgroup' a b c d e f = Qgroup {
    qgroupId :: a,
    qgroupSort :: b,
    qgroupHeader :: c,
    qgroupScale :: d,
    qgroupQtypeId :: e,
    qgroupSectionId :: f
} deriving (Show, Eq)
$(makeAdaptorAndInstance "pQgroup" ''Qgroup')

newtype QgroupId' a = QgroupId { unQgroupId :: a } deriving (Show, Eq)
$(makeAdaptorAndInstance "pQgroupId" ''QgroupId')

type QgroupIdColumn = (QgroupId' (Column PGInt4))
type QgroupWriteIdColumn = (QgroupId' (Maybe (Column PGInt4)))

qgroupTable :: Table 
    (Qgroup' QgroupWriteIdColumn (Column PGInt4) (Column PGText) (Column PGText) (QtypeIdColumn) (SectionIdColumn))
    (Qgroup' QgroupIdColumn (Column PGInt4) (Column PGText) (Column PGText) (QtypeIdColumn) (SectionIdColumn))
qgroupTable = Table "qgroup" $
    pQgroup Qgroup {
        qgroupId        = pQgroupId  . QgroupId  $ OE.optional "qgroup_id",
        qgroupSort      = required "qgroup_sort",
        qgroupHeader    = required "qgroup_header",
        qgroupScale     = required "qgroup_scale",
        qgroupQtypeId   = pQtypeId   . QtypeId   $ required "qgroup_qtype_id",
        qgroupSectionId = pSectionId . SectionId $ required "qgroup_section_id"
    }

zipQgroup :: Qgroup' (QgroupId' [a]) [b] [c] [d] [e] [f] -> [Qgroup' (QgroupId' a) b c d e f]
zipQgroup (Qgroup a b c d e f) = zipWith6 Qgroup (fmap QgroupId $ unQgroupId a) b c d e f

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

questionTable :: Table
    (Question' QuestionWriteIdColumn (Column PGInt4) (Column PGText) (QgroupIdColumn))
    (Question' QuestionIdColumn (Column PGInt4) (Column PGText) (QgroupIdColumn))
questionTable = Table "question" $
    pQuestion Question {
        questionId       = pQuestionId . QuestionId $ OE.optional "question_id",
        questionSort     = required "question_sort",
        questionText     = required "question_text",
        questionQgroupId = pQgroupId   . QgroupId   $ required "question_qgroup_id"
    }

zipQuestion :: Question' (QuestionId' [a]) [b] [c] [d] -> [Question' (QuestionId' a) b c d]
zipQuestion (Question a b c d) = zipWith4 Question (fmap QuestionId $ unQuestionId a) b c d

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

ratingTable :: Table
    (Rating' RatingWriteIdColumn (Column PGInt4) (Column PGText) (Column PGBool) (QgroupIdColumn))
    (Rating' RatingIdColumn (Column PGInt4) (Column PGText) (Column PGBool) (QgroupIdColumn))
ratingTable = Table "rating" $
    pRating Rating {
        ratingId       = pRatingId . RatingId $ OE.optional "rating_id",
        ratingSort     = required "rating_sort",
        ratingValue    = required "rating_value",
        ratingDevGiven = required "rating_dev_given",
        ratingQgroupId = pQgroupId . QgroupId $ required "rating_qgroup_id"
    }

zipRating :: Rating' (RatingId' [a]) [b] [c] [d] [e] -> [Rating' (RatingId' a) b c d e]
zipRating (Rating a b c d e) = zipWith5 Rating (fmap RatingId $ unRatingId a) b c d e

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

answerTable :: Table
    (Answer' (AnswerId' (Maybe (Column PGInt4))) RatingIdColumn QuestionIdColumn ParticipantIdColumn)
    (Answer' AnswerIdColumn RatingIdColumn QuestionIdColumn ParticipantIdColumn)
answerTable = Table "answer" $
    pAnswer Answer {
        answerId            = pAnswerId      . AnswerId      $ OE.optional "answer_id",
        answerRatingId      = pRatingId      . RatingId      $ required "answer_rating_id",
        answerQuestionId    = pQuestionId    . QuestionId    $ required "answer_question_id",
        answerParticipantId = pParticipantId . ParticipantId $ required "answer_participant_id"
    }

zipAnswer :: Answer' (AnswerId' [a]) [b] [c] [d] -> [Answer' (AnswerId' a) b c d]
zipAnswer (Answer a b c d) = zipWith4 Answer (fmap AnswerId $ unAnswerId a) b c d
