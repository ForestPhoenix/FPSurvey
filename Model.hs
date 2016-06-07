
module Model where

import Import
import Opaleye as OE
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product
import Query.Relation

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
zipParticipant (Participant a b) = zipWith Participant (ParticipantId <$> unParticipantId a) b

-- table section

data Section' a b c = Section {
    sectionId :: a,
    sectionSort :: b,
    sectionTitle :: c
} deriving (Show, Eq)
$(deriveRelation ''Section' "pSection")

newtype SectionId' a = SectionId { unSectionId :: a } deriving (Show, Eq)
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

zipSection :: Section' (SectionId' [a]) [b] [c] -> [Section' (SectionId' a) b c]
zipSection (Section a b c) = Section <$> (SectionId <$> unSectionId a) <*> b <*> c

liftSectionIds :: (ProductProfunctor p) =>
    p a0 b0 -> p a1 b1 -> p a2 b2 ->
    p (Section' (SectionId' a0) a1 a2) (Section' (SectionId' b0) b1 b2)
liftSectionIds q0 q1 q2 =
    pSection $ Section (dimap unSectionId SectionId q0) q1 q2

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

type QtypeColumnsWrapped w = Qtype'
    (QtypeId' (w PGInt4))
    (w PGText)
type QtypeColumns = QtypeColumnsWrapped Column

type QtypeDataWrapped w = Qtype'
    (QtypeId' (w Int))
    (w Text)
type QtypeData = Qtype'
    (QtypeId' Int)
    Text

qtypeTable :: Table
    (Qtype' QtypeWriteIdColumn (Column PGText))
    QtypeColumns
qtypeTable = Table "qtype" $
    pQtype Qtype {
        qtypeId             = pQtypeId . QtypeId $ OE.optional "qtype_id",
        qtypeDisplayVariant = required "qtype_display_variant"
    }

zipQtype :: Qtype' (QtypeId' [a]) [b] -> [Qtype' (QtypeId' a) b]
zipQtype (Qtype a b) = zipWith Qtype (QtypeId <$> unQtypeId a) b

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

type QgroupColumnsWrapped w = Qgroup'
    (QgroupId' (w PGInt4))
    (w PGInt4)
    (w PGText)
    (w PGText)
    (QtypeId' (w PGInt4))
    (SectionId' (w PGInt4))
type QgroupColumns = QgroupColumnsWrapped Column

type QgroupDataWrapped w = Qgroup'
    (QgroupId' (w Int))
    (w Int)
    (w Text)
    (w Text)
    (QtypeId' (w Int))
    (SectionId' (w Int))
type QgroupData = Qgroup'
    (QgroupId' Int)
    Int
    Text
    Text
    (QtypeId' Int)
    (SectionId' Int)

qgroupTable :: Table
    (Qgroup' QgroupWriteIdColumn (Column PGInt4) (Column PGText) (Column PGText) QtypeIdColumn SectionIdColumn)
    QgroupColumns
qgroupTable = Table "qgroup" $
    pQgroup Qgroup {
        qgroupId        = pQgroupId  . QgroupId  $ OE.optional "qgroup_id",
        qgroupSort      = required "qgroup_sort",
        qgroupHeader    = required "qgroup_header",
        qgroupScale     = required "qgroup_scale",
        qgroupQtypeId   = pQtypeId   . QtypeId   $ required "qgroup_qtype_id",
        qgroupSectionId = pSectionId . SectionId $ required "qgroup_section_id"
    }

zipQgroup ::
    Qgroup' (QgroupId' [a]) [b] [c] [d] (QtypeId' [e]) (SectionId' [f]) ->
    [Qgroup' (QgroupId' a) b c d (QtypeId' e) (SectionId' f)]
zipQgroup (Qgroup a b c d e f) = zipWith6 Qgroup
    (QgroupId <$> unQgroupId a) b c d
    (QtypeId <$> unQtypeId e) (SectionId <$> unSectionId f)

liftQgroup :: (ProductProfunctor p) =>
    p a0 b0 -> p a1 b1 -> p a2 b2 -> p a3 b3 -> p a4 b4 -> p a5 b5 ->
    p (Qgroup' (QgroupId' a0) a1 a2 a3 (QtypeId' a4) (SectionId' a5))
      (Qgroup' (QgroupId' b0) b1 b2 b3 (QtypeId' b4) (SectionId' b5))
liftQgroup q0 q1 q2 q3 q4 q5 = pQgroup $ Qgroup
    (dimap unQgroupId QgroupId q0)
    q1
    q2
    q3
    (dimap unQtypeId QtypeId q4)
    (dimap unSectionId SectionId q5)

liftQgroupId :: (Profunctor p) =>
    p a b ->
    p (QgroupId' a) (QgroupId' b)
liftQgroupId = dimap unQgroupId QgroupId

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
    (Question' QuestionWriteIdColumn (Column PGInt4) (Column PGText) (QgroupIdColumn))
    QuestionColumns
questionTable = Table "question" $
    pQuestion Question {
        questionId       = pQuestionId . QuestionId $ OE.optional "question_id",
        questionSort     = required "question_sort",
        questionText     = required "question_text",
        questionQgroupId = pQgroupId   . QgroupId   $ required "question_qgroup_id"
    }

zipQuestion :: Question' (QuestionId' [a]) [b] [c] (QgroupId' [d]) ->
    [Question' (QuestionId' a) b c (QgroupId' d)]
zipQuestion (Question a b c d) = zipWith4 Question (QuestionId <$> unQuestionId a) b c (QgroupId <$> unQgroupId d)

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
    (Rating' RatingWriteIdColumn (Column PGInt4) (Column PGText) (Column PGBool) (QgroupIdColumn))
    RatingColumns
ratingTable = Table "rating" $
    pRating Rating {
        ratingId       = pRatingId . RatingId $ OE.optional "rating_id",
        ratingSort     = required "rating_sort",
        ratingValue    = required "rating_value",
        ratingDevGiven = required "rating_dev_given",
        ratingQgroupId = pQgroupId . QgroupId $ required "rating_qgroup_id"
    }

zipRating :: Rating' (RatingId' [a]) [b] [c] [d] (QgroupId' [e]) ->
    [Rating' (RatingId' a) b c d (QgroupId' e)]
zipRating (Rating a b c d e) = zipWith5 Rating (RatingId <$> unRatingId a) b c d (QgroupId <$> unQgroupId e)

liftRating :: (ProductProfunctor p) =>
    p a0 b0 -> p a1 b1 -> p a2 b2 -> p a3 b3 -> p a4 b4 ->
    p (Rating' (RatingId' a0) a1 a2 a3 (QgroupId' a4)) (Rating' (RatingId' b0) b1 b2 b3 (QgroupId' b4))
liftRating q0 q1 q2 q3 q4 = pRating $ Rating (liftRatingId q0) q1 q2 q3 (liftQgroupId q4)

liftRatingId :: (Profunctor p) =>
    p a b -> p (RatingId' a) (RatingId' b)
liftRatingId = dimap unRatingId RatingId

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
zipAnswer (Answer a b c d) = zipWith4 Answer (AnswerId <$> unAnswerId a) b c d
