{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Query.Survey where

import Prelude -- not using Import, because Query conflicts
import Data.Tuple.Select
import Control.Arrow

import Opaleye

import Model
import Query.Transformers
import Query.Common

-- query survey

querySections :: Query
    (SectionColumns,
    QgroupColumnsWrapped ColArr,
    QtypeColumnsWrapped ColArr,
    RatingColumnsWrapped ColArrArr,
    QuestionColumnsWrapped ColArrArr)
querySections = proc () -> do
    sections <- queryTable sectionTable -< ()
    (sectionIds, (qgroups, qtypes, ratings, questions)) <- queryQgroupsArr -< ()
    restrict -< sectionIds .=== sectionId sections
    returnA -< (sections, qgroups, qtypes, ratings, questions)

-- query sections, [qgroups, qtypes, [ratings], [questions]]

queryQgroupsArr :: Query
    (SectionIdColumn,
    (QgroupColumnsWrapped ColArr,
    QtypeColumnsWrapped ColArr,
    RatingColumnsWrapped ColArrArr,
    QuestionColumnsWrapped ColArrArr))
queryQgroupsArr = proc () -> do
    (sectionIds, (_ :: SectionId' (ColNull PGInt4),
        (qgroups :: (
            QgroupColumnsWrapped ColNullArr,
            QtypeColumnsWrapped ColNullArr,
            RatingColumnsWrapped ColNullArrArr,
            QuestionColumnsWrapped ColNullArrArr)
            )))
        <- leftJoin querySectionIds queryQgroupsArrHelper
        (\(jSectionIds, jQqroupResults) -> fst jQqroupResults .=== jSectionIds) -< ()
    returnA -< (sectionIds, allFromNullableEmptyArray qgroups)

queryQgroupsArrHelper :: Query
    (SectionIdColumn,
    (QgroupColumnsWrapped ColArr,
    QtypeColumnsWrapped ColArr,
    RatingColumnsWrapped ColArrArr,
    QuestionColumnsWrapped ColArrArr))
queryQgroupsArrHelper = arrayAggSnd queryQgroupsSectionId

queryQgroupsSectionId :: Query
    (SectionIdColumn,
    (QgroupColumns,
    QtypeColumns,
    RatingColumnsWrapped ColArr,
    QuestionColumnsWrapped ColArr))
queryQgroupsSectionId = proc () -> do
    sectionIds <- querySectionIds -< ()
    qgroups <- queryQgroups -< ()
    restrict -< qgroupSectionId (sel1 qgroups) .=== sectionIds
    returnA -< (sectionIds, qgroups)

-- query qgroups, qtypes, [ratings], [questions]

queryQgroups :: Query
    (QgroupColumns,
    QtypeColumns,
    RatingColumnsWrapped ColArr,
    QuestionColumnsWrapped ColArr)
queryQgroups = proc () -> do
    qgroups <- queryTable qgroupTable -< ()
    qtypes <- queryTable qtypeTable -< ()
    restrict -< qtypeId qtypes .=== qgroupQtypeId qgroups
    (ratingQgroupIds, ratings) <- queryRatingsArr -< ()
    restrict -< ratingQgroupIds .=== qgroupId qgroups
    (questionQgroupIds, questions) <- queryQuestionsArr -< ()
    restrict -< questionQgroupIds .=== qgroupId qgroups
    returnA -< (qgroups, qtypes, ratings, questions)

-- query qgroups, questions

queryQuestionsArr :: Query (QgroupIdColumn, QuestionColumnsWrapped ColArr)
queryQuestionsArr = proc () -> do
    (qgroupIds, (_ :: QgroupId' (ColNull PGInt4),
        questions :: QuestionColumnsWrapped ColNullArr))
        <- leftJoin queryQgroupIds queryQuestionsArrHelper
        (\(jQgroupIds, jQuestionResults) -> fst jQuestionResults .=== jQgroupIds) -< ()
    returnA -< (qgroupIds, allFromNullableEmptyArray questions)

queryQuestionsArrHelper :: Query (QgroupIdColumn, QuestionColumnsWrapped ColArr)
queryQuestionsArrHelper = arrayAggSnd queryQuestionsQgroupId

queryQuestionsQgroupId :: Query (QgroupIdColumn, QuestionColumns)
queryQuestionsQgroupId = proc () -> do
    qgroupIds <- queryQgroupIds -< ()
    questions <- queryTable questionTable -< ()
    restrict -< questionQgroupId questions .=== qgroupIds
    returnA -< (qgroupIds, questions)

-- query qgroups, ratings

queryRatingsArr :: Query (QgroupIdColumn, RatingColumnsWrapped ColArr)
queryRatingsArr = proc () -> do
    (qgroupIds, (_ :: QgroupId' (ColNull PGInt4),
        ratings :: RatingColumnsWrapped ColNullArr))
        <- leftJoin queryQgroupIds queryRatingsArrHelper
        (\(jQgroupIds, jRatingResults) -> fst jRatingResults .=== jQgroupIds) -< ()
    returnA -< (qgroupIds,
        allFromNullableEmptyArray ratings)

queryRatingsArrHelper :: Query (QgroupIdColumn, RatingColumnsWrapped ColArr)
queryRatingsArrHelper = arrayAggSnd queryRatingsQgroupId

queryRatingsQgroupId :: Query (QgroupIdColumn, RatingColumns)
queryRatingsQgroupId = proc () -> do
    qgroupIds <- queryQgroupIds -< ()
    ratings <- queryTable ratingTable -< ()
    restrict -< ratingQgroupId ratings .=== qgroupIds
    returnA -< (qgroupIds, ratings)

-- query Id columns only only

querySectionIds :: Query SectionIdColumn
querySectionIds = sectionId <$> queryTable sectionTable

queryQgroupIds :: Query QgroupIdColumn
queryQgroupIds = qgroupId <$> queryTable qgroupTable
