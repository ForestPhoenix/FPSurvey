{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Query.Survey where

import Prelude -- not using Import, because Query conflicts
import Data.Tuple.Select
import Control.Arrow
import Data.Monoid

import Opaleye

import Query.Model

queryQgroupRatings :: Query (QgroupColumns, RatingColumns)
queryQgroupRatings = proc () -> do
    qgroups <- queryTable qgroupTable -< ()

    ratings <- queryTable ratingTable -< ()
    restrict -< ratingQgroupId ratings .=== qgroupId qgroups
    returnA -< (qgroups, ratings)

querySurveyQuestions :: Query (SectionColumns, QgroupColumns, QuestionColumns)
querySurveyQuestions = flip orderBy querySurveyQuestionsUnsorted $
    asc (sectionSort . sel1) <>
    asc (qgroupSort . sel2) <>
    asc (questionSort . sel3)

querySurveyQuestionsUnsorted :: Query (SectionColumns, QgroupColumns, QuestionColumns)
querySurveyQuestionsUnsorted = proc () -> do
    sections <- queryTable sectionTable -< ()

    qgroups <- queryTable qgroupTable -< ()
    restrict -< qgroupSectionId qgroups .=== sectionId sections

    questions <- queryTable questionTable -< ()
    restrict -< questionQgroupId questions .=== qgroupId qgroups

    returnA -< (sections, qgroups, questions)
