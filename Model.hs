module Model where

import Import
import Opaleye as OE
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

-- table participant

data Participant' a b = Participant {
    participantId :: a,
    participantToken :: b
}
$(makeAdaptorAndInstance "pParticipant" ''Participant')

newtype ParticipantId' a = ParticipantId a
$(makeAdaptorAndInstance "pParticipantId" ''ParticipantId')

type ParticipantColumns idCol = Participant' 
    (idCol)
    (Column PGText)
    
type ParticipantIdColumn = (ParticipantId' (Column PGInt4))

participantTable :: Table
    (ParticipantColumns (ParticipantId' (Maybe (Column PGInt4))))
    (ParticipantColumns ParticipantIdColumn)
participantTable = Table "participant" $
    pParticipant Participant {
        participantId    = pParticipantId . ParticipantId $ OE.optional "participant_id",
        participantToken = required "participant_token"
    }

-- table section

data Section' a b c = Section {
    sectionId :: a,
    sectionSort :: b,
    sectionTitle :: c
}
$(makeAdaptorAndInstance "pSection" ''Section')

newtype SectionId' a = SectionId a
$(makeAdaptorAndInstance "pSectionId" ''SectionId')

type SectionColumns idCol = Section'
    (idCol)
    (Column PGInt4)
    (Column PGText)

type SectionIdColumn = SectionId' (Column PGInt4)

sectionTable :: Table 
    (SectionColumns (SectionId' (Maybe (Column PGInt4)))) 
    (SectionColumns SectionIdColumn)
sectionTable = Table "section" $
    pSection Section {
        sectionId    = pSectionId . SectionId $ OE.optional "section_id",
        sectionSort  = required "section_sort",
        sectionTitle = required "section_title"
    }

-- table qtype

data Qtype' a b = Qtype {
    qtypeId :: a,
    qtypeDisplayVariant :: b
}
$(makeAdaptorAndInstance "pQtype" ''Qtype')

newtype QtypeId' a = QtypeId a
$(makeAdaptorAndInstance "pQtypeId" ''QtypeId')

type QtypeColumns idCol = Qtype'
    (idCol)
    (Column PGText)

type QtypeIdColumn = QtypeId' (Column PGInt4)

qtypeTable :: Table
    (QtypeColumns (QtypeId' (Maybe (Column PGInt4))))
    (QtypeColumns QtypeIdColumn)
qtypeTable = Table "qtype" $
    pQtype Qtype {
        qtypeId             = pQtypeId . QtypeId $ OE.optional "qtype_id",
        qtypeDisplayVariant = required "qtype_display_variant"
    }

-- table qgroup

data Qgroup' a b c d e f = Qgroup {
    qgroupId :: a,
    qgroupSort :: b,
    qgroupHeader :: c,
    qgroupScale :: d,
    qgroupQtypeId :: e,
    qgroupSectionId :: f
}
$(makeAdaptorAndInstance "pQgroup" ''Qgroup')

newtype QgroupId' a = QgroupId a
$(makeAdaptorAndInstance "pQgroupId" ''QgroupId')

type QgroupColumns idCol = Qgroup'
    (idCol)
    (Column PGInt4)
    (Column PGText)
    (Column PGText)
    (QtypeIdColumn)
    (SectionIdColumn)

type QgroupIdColumn = (QgroupId' (Column PGInt4))

qgroupTable :: Table 
    (QgroupColumns (QgroupId' (Maybe (Column PGInt4))))
    (QgroupColumns QgroupIdColumn)
qgroupTable = Table "qgroup" $
    pQgroup Qgroup {
        qgroupId        = pQgroupId  . QgroupId  $ OE.optional "qgroup_id",
        qgroupSort      = required "qgroup_sort",
        qgroupHeader    = required "qgroup_header",
        qgroupScale     = required "qgroup_scale",
        qgroupQtypeId   = pQtypeId   . QtypeId   $ required "qgroup_qtype_id",
        qgroupSectionId = pSectionId . SectionId $ required "qgroup_section_id"
    }

-- table question

data Question' a b c d = Question {
    questionId :: a,
    questionSort :: b,
    questionText :: c,
    questionQgroupId :: d
}
$(makeAdaptorAndInstance "pQuestion" ''Question')

newtype QuestionId' a = QuestionId a
$(makeAdaptorAndInstance "pQuestionId" ''QuestionId')

type QuestionColumns idCol = Question'
    (idCol)
    (Column PGInt4)
    (Column PGText)
    (QgroupIdColumn)

type QuestionIdColumn = QuestionId' (Column PGInt4)

questionTable :: Table
    (QuestionColumns (QuestionId' (Maybe (Column PGInt4))))
    (QuestionColumns QuestionIdColumn)
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
}
$(makeAdaptorAndInstance "pRating" ''Rating')

newtype RatingId' a = RatingId a
$(makeAdaptorAndInstance "pRatingId" ''RatingId')

type RatingColumns idCol = Rating'
    (idCol)
    (Column PGInt4)
    (Column PGText)
    (Column PGBool)
    (QgroupIdColumn)

type RatingIdColumn = RatingId' (Column PGInt4)

ratingTable :: Table
    (RatingColumns (RatingId' (Maybe (Column PGInt4))))
    (RatingColumns RatingIdColumn)
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
}
$(makeAdaptorAndInstance "pAnswer" ''Answer')

newtype AnswerId' a = AnswerId a
$(makeAdaptorAndInstance "pAnswerId" ''AnswerId')

type AnswerColumns idCol = Answer'
    (idCol)
    (RatingIdColumn)
    (QuestionIdColumn)
    (ParticipantIdColumn)

type AnswerIdColumn = AnswerId' (Column PGInt4)

answerTable :: Table
    (AnswerColumns (AnswerId' (Maybe (Column PGInt4))))
    (AnswerColumns AnswerIdColumn)
answerTable = Table "answer" $
    pAnswer Answer {
        answerId            = pAnswerId      . AnswerId      $ OE.optional "answer_id",
        answerRatingId      = pRatingId      . RatingId      $ required "answer_rating_id",
        answerQuestionId    = pQuestionId    . QuestionId    $ required "answer_question_id",
        answerParticipantId = pParticipantId . ParticipantId $ required "answer_participant_id"
    }
