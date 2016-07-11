module Forms.TakeSurvey where

import Import

import Query.Model
import Query.Types
import Fields.Survey

style = $(widgetFile "survey")

surveyForm ::
    TakeSurvey ->
    Html ->
    MForm Handler (FormResult (Text, [(QuestionData, SurveyInput)]), Widget)

surveyForm survey extra = do
    let subForms = fmap sectionForm survey
    (userRes, userView) <- mreq textField "" Nothing
    subFormsRes <- sequence (fmap (\form -> form $ toHtml ("" :: Text)) subForms)
    let (subRes, subWidgets) = unzip subFormsRes
    let widget =
            [whamlet|
                #{extra}
                <div class="survey">
                    <div class="survey_group">
                        <span class="survey_group_header"> Dein Zugangscode:
                        <br>
                        ^{fvInput userView}
                    <ol>
                        $forall subWidget <- subWidgets
                            ^{subWidget}
                    <input type=submit value="Submit Survey">
            |]
    let result = (,) <$> userRes <*> mconcat subRes
    return (result, style >> widget)

sectionForm ::
    (SectionData, [(QgroupData, [QuestionData], [RatingData])] ) ->
    Html ->
    MForm Handler (FormResult [(QuestionData, SurveyInput)], Widget)

sectionForm (section, groups) _ = do
    subFormsRes <- forM groups groupForm
    let (subRes, subWidgets) = unzip subFormsRes
    let widget = [whamlet|
        <div class="sectionFrame">
            <h2> <li> #{sectionTitle section}
            <ol>
                $forall subWidget <- subWidgets
                    <li> ^{subWidget}
        |]
    let result = mconcat subRes
    return (result, widget)

groupForm ::
    (QgroupData, [QuestionData], [RatingData]) ->
    MForm Handler (FormResult [(QuestionData, SurveyInput)], Widget)
groupForm (qgroup, questions, ratings) = do
    let freeField = (freeInputForm . unEnumField . qgroupFreeField) qgroup
    (results, widget) <- case (unEnumField . qgroupQuestionDisplay) qgroup of
        RadioInline -> do
            (results, views) <- unzip <$> forM questions
                (\q -> first (fmap (q,)) <$> radioInput ratings (toHtml . ratingValue) freeField)
            let widget = case zip questions views of
                    ((question, view):[]) -> [whamlet|
                        <div class="qgroupFrame">
                            <span class="qgroupHeader"> #{questionText question}
                            <table class="RadioInline"> <tbody> <tr>
                                ^{fvInput view}
                        |]
                    moreThanOne -> [whamlet|
                        <div class="qgroupFrame">
                            <span class="qgroupHeader"> #{qgroupHeader qgroup}
                            <table class="RadioInline"> <tbody>
                                $forall (question, view) <- moreThanOne
                                    <tr>
                                        <td> #{questionText question}
                                        ^{fvInput view}
                        |]
            return (results, widget)
        RadioTable -> do
                (results, views) <- unzip <$> forM questions
                    (\q -> first (fmap (q,)) <$> radioInput ratings (const "") freeField)
                let widget = case zip questions views of
                        ((question, view):[]) -> [whamlet|
                            <div class="qgroupFrame">
                                <span class="qgroupHeader"> #{questionText question}
                                <table class="RadioTable"> <tbody> <tr>
                                    ^{fvInput view}
                            |]
                        moreThanOne -> [whamlet|
                            <div class="qgroupFrame">
                                <table class="RadioTable">
                                    <thead> <tr>
                                        <th> <span class="qgroupHeader"> #{qgroupHeader qgroup}
                                        $forall descr <- (toHtml . ratingValue) <$> ratings
                                            <th> #{descr}
                                        $if isJust freeField
                                            <th>
                                    <tbody>
                                        $forall (question, view) <- moreThanOne
                                            <tr> <td> #{questionText question}
                                                ^{fvInput view}
                            |]
                return (results, widget)
        RadioLines -> do
                (results, views) <- unzip <$> forM questions
                    (\q -> first (fmap (q,)) <$> radioLines ratings (toHtml . ratingValue) freeField)
                let widget = case zip questions views of
                        ((question, view):[]) -> [whamlet|
                            <div class="qgroupFrame">
                                <span class="qgroupHeader"> #{questionText question}
                                <table class="RadioLines"> <tbody>
                                    ^{fvInput view}
                            |]
                        moreThanOne -> [whamlet|
                            <div class="qgroupFrame">
                                <span class="qgroupHeader"> #{qgroupHeader qgroup}
                                $forall (question, view) <- moreThanOne
                                    <table class="RadioLines"> <tbody>
                                        <td> #{questionText question}
                                        ^{fvInput view}
                            |]
                return (results, widget)
    return (mconcat $ fmap (:[]) <$> results, widget)

freeInputForm :: FreeField -> Maybe (MForm Handler (FormResult (Maybe Text), FieldView App))

freeInputForm NoFreeField = Nothing
freeInputForm FreeText = Just $
    mopt textField "This is no used" Nothing
freeInputForm FreeInteger = Just $
    first (fmap . fmap $ pack . show) <$> mopt intField "This is not used" Nothing

radioInput ::
    [RatingData] ->
    (RatingData -> Html) ->
    Maybe (MForm Handler (FormResult (Maybe Text), FieldView App)) ->
    MForm Handler (FormResult SurveyInput, FieldView App)

radioInput ratings displayRating Nothing =
    first (fmap (InputData)) <$> mreq (radioTdField $ (id &&& displayRating) <$> ratings)
        "This is not used" Nothing

radioInput ratings displayRating (Just otherField) = do
    (subRes, subView) <- otherField
    (mainRes, mainView) <- mreq (radioTdFieldWithOther
            ((id &&& displayRating) <$> ratings)
            subView subRes)
        "This is not used" Nothing
    return (convertFreeForm mainRes, mainView)

radioLines ::
    [RatingData] ->
    (RatingData -> Html) ->
    Maybe (MForm Handler (FormResult (Maybe Text), FieldView App)) ->
    MForm Handler (FormResult SurveyInput, FieldView App)
radioLines ratings renderRating Nothing =
    first (fmap (InputData)) <$> mreq (genericRadioField (\inner -> [whamlet|<tr> <td> ^{inner}|]) $
        (id &&& renderRating) <$> ratings)
            "This is not used" Nothing
radioLines ratings renderRating (Just freeField) = do
    (subRes, subView) <- freeField
    (mainRes, mainView) <- mreq (genericRadioFieldWithOther (\inner -> [whamlet|<tr> <td> ^{inner}|])
            ((id &&& renderRating) <$> ratings)
            subView subRes)
        "This is not used" Nothing
    return (convertFreeForm mainRes, mainView)

convertFreeForm ::
    FormResult (InputWithOther RatingData (Maybe Text)) ->
    FormResult (InputWithOther RatingData Text)
convertFreeForm (FormSuccess value) = convert value
    where
        convert (InputData dat) = FormSuccess $ InputData dat
        convert (InputOther (Nothing)) = FormFailure ["Missing input in FreeForm"]
        convert (InputOther (Just text)) = FormSuccess $ InputOther text
convertFreeForm (FormFailure v) = FormFailure v
convertFreeForm FormMissing = FormMissing
