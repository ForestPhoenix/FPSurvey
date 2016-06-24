{-# LANGUAGE Arrows              #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Query.Survey where

import           Control.Arrow
import qualified Data.List.NonEmpty as NE
import           Data.Monoid
import           Data.Tuple.Select
import           Opaleye
import           Prelude
import           Query.Model
import           Query.Transformers

type TakeSurvey = [(
        SectionData, [(
            QgroupData,
            [QuestionData],
            [RatingData]
        )]
    )]

takeSurvey :: ([(SectionData, (QgroupData, QuestionData))], [(QgroupData, RatingData)]) -> TakeSurvey
takeSurvey = proc (questions, ratings) -> do
    collQuestions <- fullCollapseRight -< questions
    returnA -< zip (groupedAs collQuestions) $
        curry surveyQgroups ratings <$> (fmap NE.toList . groupedBs) collQuestions

surveyQgroups :: ([(QgroupData, RatingData)], [(QgroupData, QuestionData)]) ->
    [(QgroupData, [QuestionData], [RatingData])]
surveyQgroups = proc (ratings, qgroups) -> do
    collQgroups <- fullCollapseRight -< qgroups
    collRatings <- fullCollapseRight -< ratings
    joinRatings <- uncurry dataLeftJoin -< (collRatings, groupedAs collQgroups)
    returnA -< zip3 (groupedAs collQgroups)
        ((fmap NE.toList . groupedBs) collQgroups)
        (groupedBs joinRatings)

queryQgroupRatings :: Query (QgroupColumns, RatingColumns)
queryQgroupRatings = proc () -> do
    qgroups <- queryTable qgroupTable -< ()

    ratings <- queryTable ratingTable -< ()
    restrict -< ratingQgroupId ratings .=== qgroupId qgroups
    returnA -< (qgroups, ratings)

querySurveyQuestions :: Query (SectionColumns, (QgroupColumns, QuestionColumns))
querySurveyQuestions = flip orderBy querySurveyQuestionsUnsorted $
    asc (sectionSort . fst) <>
    asc (qgroupSort . fst . snd) <>
    asc (questionSort . snd . snd)

querySurveyQuestionsUnsorted :: Query (SectionColumns, (QgroupColumns, QuestionColumns))
querySurveyQuestionsUnsorted = proc () -> do
    sections <- queryTable sectionTable -< ()

    qgroups <- queryTable qgroupTable -< ()
    restrict -< qgroupSectionId qgroups .=== sectionId sections

    questions <- queryTable questionTable -< ()
    restrict -< questionQgroupId questions .=== qgroupId qgroups

    returnA -< (sections, (qgroups, questions))
