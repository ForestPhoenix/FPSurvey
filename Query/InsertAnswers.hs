
module  Query.InsertAnswers where

import qualified Data.List                  as L (head)
import           Database.PostgreSQL.Simple
import           Import                     hiding (Query)
import           Opaleye                    hiding (not, null)
import qualified Opaleye                    as O
import           Query.Model
import           Query.Types

insertAnswers :: [(QuestionData, SurveyInput)] -> Connection -> IO ()
insertAnswers answers conn = do
    let dataInputs = second (\(InputData d) -> d) <$> filter (isData . snd) answers
    let otherInputs = second (\(InputOther d) -> d) <$> filter (isOther . snd) answers
    withTransaction conn $ do
        insertedOthers <- if not $ null otherInputs
            then runInsertManyReturning conn ratingTable (convertNewOther <$> otherInputs) id
            else return []
        let allInputs = zip (fst $ unzip otherInputs) insertedOthers ++ dataInputs
        participant <- runInsertReturning conn participantTable newParticipant id
        _ <- runInsertMany conn answerTable (convertInput (L.head participant) <$> allInputs)
        return ()

newParticipant :: ParticipantWriteColumns
newParticipant = Participant { participantId = ParticipantId Nothing }

convertNewOther :: (QuestionData, Text) -> RatingWriteColumns
convertNewOther (question, newRating) = Rating {
        ratingId = RatingId Nothing,
        ratingSort = pgInt4 $ negate 1,
        ratingValue = pgStrictText newRating,
        ratingDevGiven = pgBool False,
        ratingQgroupId = QgroupId $ pgInt4 $ unQgroupId $ questionQgroupId question
    }

convertInput :: ParticipantData -> (QuestionData, RatingData) -> AnswerWriteColumns
convertInput participant (question, rating) = Answer {
        answerId = AnswerId Nothing,
        answerRatingId = RatingId $ pgInt4 $ unRatingId $ ratingId rating,
        answerQuestionId = QuestionId $ pgInt4 $ unQuestionId $ questionId question,
        answerParticipantId = ParticipantId $ pgInt4 $ unParticipantId $ participantId participant
    }
