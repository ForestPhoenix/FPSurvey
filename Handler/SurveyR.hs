module Handler.SurveyR where

import Import

import Query.Model
import Query.ReadSurvey
import Query.InsertAnswers
import Forms.TakeSurvey

getSurveyR :: Handler Html
getSurveyR = runSurveyR

postSurveyR :: Handler Html
postSurveyR = runSurveyR

runSurveyR :: Handler Html
runSurveyR = do
    app <- getYesod
    let pool = appConnPool app
    rawQuestions <- liftIO $ withResource pool $ (\conn -> (runQuery conn querySurveyQuestions))
    rawRatings <- liftIO $ withResource pool $ (\conn -> (runQuery conn queryQgroupRatings))
    let survey = takeSurvey (rawQuestions, rawRatings)
    ((res, widget), enctype) <- runFormPost $ surveyForm survey
    case res of
        FormMissing -> do
            defaultLayout
                [whamlet|
                    <h1 class="survey_header"> Whats you opinion about programming
                    <form method=post action=@{SurveyR} enctype=#{enctype}> ^{widget}
                |]
        FormFailure errorMessages -> do
            defaultLayout
                [whamlet|
                    $forall errorMessage <- errorMessages
                        <br> An error occured: #{errorMessage}
                |]
        FormSuccess surveyInput -> do
            liftIO $ withResource pool $ insertAnswers (snd surveyInput)
            defaultLayout
                [whamlet|
                     <h1> Thanks for participating!
                 |]
