module Handler.Survey where

import Import

import Query.Model
import Query.Survey
import Forms.Survey

getSurveyR :: Handler Html
getSurveyR = runSurveyR

postSurveyR :: Handler Html
postSurveyR = runSurveyR

type List a = [a]
type ListList a = List (List a)



runSurveyR :: Handler Html
runSurveyR = do fail "NY!"
{-    app <- getYesod
    let pool = appConnPool app
    rawSurvey <- liftIO $ withResource pool $
        (\conn -> (runQuery conn querySections) :: IO
         [(SectionData,
            QgroupDataWrapped List,
            QtypeDataWrapped List,
                RatingDataWrapped ListList,
                QuestionDataWrapped ListList)]
        )
    let survey = zipSurveySection <$> rawSurvey
    ((res, widget), enctype) <- runFormPost $ surveyForm survey
    case res of
        FormMissing -> do
            defaultLayout
                [whamlet|
                    <h1 class="survey_header"> Wie wohl fühlst du dich am OSZ IMT ?
                    <form method=post action=@{SurveyR} enctype=#{enctype}> ^{widget}
                |]
        FormFailure errorMessages -> do
            defaultLayout
                [whamlet|
                    $forall errorMessage <- errorMessages
                        <br> An error occured: #{errorMessage}
                |]
        FormSuccess surveyInput -> do
                defaultLayout $ [whamlet|Coming soon!|] -}
--             submitResult <- liftIO $ withResource pool $ submitSurvey surveyInput
--             case submitResult of
--                  Just err -> defaultLayout [whamlet| an error occured: #{err}|]
--                  Nothing -> defaultLayout
--                         [whamlet|
--                             Thanks for participating!
--                             You can overwrite you answers by re-using your user token.
--                         |]

-- runSurveyR :: Handler Html
-- runSurveyR = do
--     app <- getYesod
--     let pool = appConnPool app
--     dbSurvey <- liftIO $! withResource pool $ (\conn -> mapM (\x -> (uncurry queryAllRowsMap) x conn )
--         [("BEGIN;", []),
--         ("SELECT survey('sec_rc', 'qtype_rc', 'rating_rc')", []),
--         ("FETCH ALL IN \"sec_rc\"", []),
--         ("FETCH ALL IN \"qtype_rc\"", []),
--         ("FETCH ALL IN \"rating_rc\"", []),
--         ("COMMIT;", [])])
--     let surveyMaps = take 3 $ drop 2 dbSurvey
--     let surveySections =
--             map (\row -> (,) <$> ((,) <$> safeFromSqlRow row <*> safeFromSqlRow row) <*> safeFromSqlRow row)
--             (surveyMaps !! 0)
--     let surveyTypes = map (\row -> (,) <$> safeFromSqlRow row <*> safeFromSqlRow row)
--             (surveyMaps !! 1)
--     let surveyRatings = map (\row -> (,) <$> safeFromSqlRow row <*> safeFromSqlRow row)
--             (surveyMaps !! 2)
--     let survey = convertUntakenSurvey (rights surveySections) (rights surveyTypes) (rights surveyRatings)
--     let surveyErrors = (lefts surveySections) ++ (lefts surveyTypes) ++ (lefts surveyRatings)
--     when (surveyErrors /= []) $ $logWarn $ pack $ "errors when reading the survey: " ++ show surveyErrors
--     ((res, widget), enctype) <- runFormPost $ surveyForm survey
--     case res of
--         FormMissing -> do
--             defaultLayout
--                 [whamlet|
--                     <h1 class="survey_header"> Wie wohl fühlst du dich am OSZ IMT ?
--                     <form method=post action=@{SurveyR} enctype=#{enctype}> ^{widget}
--                 |]
--         FormFailure errorMessages -> do
--             defaultLayout
--                 [whamlet|
--                     $forall errorMessage <- errorMessages
--                         <br> An error occured: #{errorMessage}
--                 |]
--         FormSuccess surveyInput -> do
--             submitResult <- liftIO $ withResource pool $ submitSurvey surveyInput
--             case submitResult of
--                  Just err -> defaultLayout [whamlet| an error occured: #{err}|]
--                  Nothing -> defaultLayout
--                         [whamlet|
--                             Thanks for participating!
--                             You can overwrite you answers by re-using your user token.
--                         |]
--
-- submitSurvey :: (IConnection conn) =>
--     (Text, [(Question SqlValue, SurveyInput SqlValue)]) -> conn -> IO (Maybe String)
-- submitSurvey (userToken, survey) conn = do
--     participantRows <-
--         queryAllRowsMap "SELECT * FROM participant WHERE participant_token = ?" [toSql userToken] conn
--     let pre_participants = map safeFromSqlRow participantRows
--     let participants = rights pre_participants
--     -- TODO: handle errors
--     case participants of
--         [] -> return $ Just "Wrong user Token."
--         [participant] -> do
--            writeSurveyToDb (participant) survey conn
--            return $ Nothing
--         _ -> return $ Just "Multiple Paricipants?!"
--
-- writeSurveyToDb :: (IConnection conn) =>
--     Participant SqlValue -> [(Question SqlValue, SurveyInput SqlValue)] -> conn -> IO ()
-- writeSurveyToDb participant survey conn = do
--     _ <- mapM (\x -> (uncurry queryAllRowsMap) x conn)
--         (("BEGIN;", []):
--         ("DELETE FROM answer WHERE answer_participant_id = ?", [participant_id participant]):
--         (map (surveyRowInsert participant) survey) ++
--         [("COMMIT;", [])])
--     return ()
--
-- surveyRowInsert ::
--     Participant SqlValue ->
--     (Question SqlValue, SurveyInput SqlValue) ->
--     (String, [SqlValue])
--
-- surveyRowInsert participant (question, InputRating rating) =
--     ("INSERT INTO answer(answer_question_id, answer_rating_id, answer_participant_id) VALUES (?, ?, ?);",
--         [rowId question, rowId rating, rowId participant])
--
-- surveyRowInsert participant (question, InputOther value) =
--     ("WITH newrating AS ( " ++
--         "INSERT INTO rating (rating_value, rating_dev_given, rating_qgroup_id) " ++
--         "SELECT ?, 'false', question_qgroup_id FROM question " ++
--         "WHERE question_id = ? " ++
--         "RETURNING rating.* " ++
--     ") " ++
--     "INSERT INTO answer(answer_question_id, answer_rating_id, answer_participant_id) " ++
--     "SELECT ?, rating_id, ? FROM newrating;",
--         [toSql value, rowId question, rowId question, rowId participant])
