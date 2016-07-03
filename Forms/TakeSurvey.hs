module Forms.TakeSurvey where

import Import

import Query.Model
import Query.Survey (TakeSurvey)
import Query.Types
import Fields.Survey

type SurveyInput = InputWithOther RatingData Text

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
                                    <tbody>
                                        $forall (question, view) <- moreThanOne
                                            <tr> <td> #{questionText question}
                                                ^{fvInput view}
                            |]
                return (results, widget)
        _ -> undefined
    return (mconcat $ fmap (:[]) <$> results, widget)

freeInputForm :: FreeField -> Maybe (MForm Handler (FormResult Text, FieldView App))

freeInputForm NoFreeField = Nothing
freeInputForm _ = undefined

radioInput ::
    [RatingData] ->
    (RatingData -> Html) ->
    Maybe (MForm Handler (FormResult Text, FieldView App)) ->
    MForm Handler (FormResult SurveyInput, FieldView App)

radioInput ratings displayRating Nothing =
    first (fmap InputData) <$> mreq (radioTdField $ (id &&& displayRating) <$> ratings)
        "This is not used" Nothing

radioInput ratings displayRating (Just otherField) = do
    (subRes, subView) <- otherField
    mreq (radioTdFieldWithOther
            ((id &&& displayRating) <$> ratings)
            subView subRes)
        "This is not used" Nothing

{- questionForm (RadioWith IntegerInput) ratings question _ = do
    (subRes, subView) <- mreq intField "This is not used" Nothing
    let convertedSubRes = pack . show <$> (subRes :: FormResult Int)
    (ratingRes, ratingView) <- mreq (radioTdFieldWithOther ratings subView convertedSubRes) "This is not used" Nothing
    let widget = do
            [whamlet|
                <td> #{question_text question}
                ^{fvInput ratingView}
            |]
    let result = (,) question <$> (fieldSoToForm <$> ratingRes)
    return (result, widget) -}

{-groupForm (qGroup, qtype, questions, ratings) _ = do
    let dvar = qtype_display_variant qtype
    let subForms = map (questionForm dvar ratings) questions
    subFormsRes <- sequence (map (\form -> form $ toHtml $ Text.pack "") subForms)
    let (subRes, subWidgets) = unzip subFormsRes
    let widget = case qtype_display_variant qtype of
            Radio -> [whamlet|
                <li class="survey_group">
                    <div>
                        <span class="survey_group_header"> #{qgroup_header qGroup}
                        <table class="survey_radio">
                            <thead>
                                <tr>
                                    <th>
                                    $forall header <- map rating_value ratings
                                        <th> #{header}
                            <tbody>
                                $forall subWidget <- subWidgets
                                    <tr> ^{subWidget}
                |]
            RadioWith _ -> [whamlet|
                <li class="survey_group">
                    <div>
                        <span class="survey_group_header"> #{qgroup_header qGroup}
                        <table class="survey_radio">
                            <thead>
                                <tr>
                                    <th>
                                    $forall header <- map rating_value ratings
                                        <th> #{header}
                                    <th> other answer
                            <tbody>
                                $forall subWidget <- subWidgets
                                    <tr> ^{subWidget}
                |]
            _ -> [whamlet|
                <li class="survey_group">
                    <div>
                        $if (qgroup_header qGroup) == ""
                            $forall subWidget <- subWidgets
                                ^{subWidget}
                        $else
                            <h3> #{qgroup_header qGroup}
                            <ol>
                                $forall subWidget <- subWidgets
                                    <li> ^{subWidget}
                |]
    let result = foldFunctor subRes
    return (result, widget)

questionForm :: (SqlId a) =>
    DisplayVariant ->
    [Rating a] ->
    Question a ->
    Html ->
    MForm Handler (FormResult (Question a, SurveyInput a), Widget)

questionForm DropDown ratings question _ = do
    let ratingList = map (\r -> (Text.pack $ rating_value r, r)) ratings
    (ratingRes, ratingView) <- mreq (selectFieldList ratingList) "This is not used" Nothing
    let widget = do
            [whamlet|
                <span class="survey_group_header"> #{question_text question}
                <br>
                ^{fvInput ratingView}
            |]
    let result = (,) question <$> (InputRating <$> ratingRes)
    return (result, widget)

questionForm Radio ratings question _ = do
    (ratingRes, ratingView) <- mreq (radioTdField $ zip ratings $ repeat "") "This is not used" Nothing
    let widget = do
            [whamlet|
                <td> #{question_text question}
                ^{fvInput ratingView}
            |]
    let result = (,) question <$> (InputRating <$> ratingRes)
    return (result, widget)

questionForm RadioInline ratings question _ = do
    (ratingRes, ratingView) <- mreq (radioTdField $ zip ratings $ map (toHtml . rating_value) ratings) "This is not used" Nothing
    let widget = do
            [whamlet|
                <span class="survey_group_header"> #{question_text question}
                <table> <tbody> <tr>
                    ^{fvInput ratingView}
            |]
    let result = (,) question <$> (InputRating <$> ratingRes)
    return (result, widget)


questionForm IntegerInput _ question _ = do
    (intRes, ratingView) <- mreq intField "This is not used" Nothing
    let widget = do
            [whamlet|
                <span class="survey_group_header"> #{question_text question}
                <br>
                ^{fvInput ratingView}
            |]
    let result = (,) question <$> (InputOther . pack . show <$> (intRes :: FormResult Int))
    return (result, widget)

questionForm TextInput _ question _ = do
    (intRes, ratingView) <- mreq textField "This is not used" Nothing
    let widget = do
            [whamlet|
                <span class="survey_group_header"> #{question_text question}
                <br>
                ^{fvInput ratingView}
            |]
    let result = (,) question <$> (InputOther <$> intRes)
    return (result, widget)

questionForm (RadioWith IntegerInput) ratings question _ = do
    (subRes, subView) <- mreq intField "This is not used" Nothing
    let convertedSubRes = pack . show <$> (subRes :: FormResult Int)
    (ratingRes, ratingView) <- mreq (radioTdFieldWithOther ratings subView convertedSubRes) "This is not used" Nothing
    let widget = do
            [whamlet|
                <td> #{question_text question}
                ^{fvInput ratingView}
            |]
    let result = (,) question <$> (fieldSoToForm <$> ratingRes)
    return (result, widget)

questionForm (RadioWith TextInput) ratings question _ = do
    (subRes, subView) <- mreq textField "This is not used" Nothing
    (ratingRes, ratingView) <- mreq (radioTdFieldWithOther ratings subView subRes) "This is not used" Nothing
    let widget = do
            [whamlet|
                <td> #{question_text question}
                ^{fvInput ratingView}
            |]
    let result = (,) question <$> (fieldSoToForm <$> ratingRes)
    return (result, widget)

questionForm (RadioWith _) _ _ _ =
    error "RadioWith used with invalid sub-input"

fieldSoToForm :: (SqlId a) => SelectWithOther (Rating a) Text -> SurveyInput a
fieldSoToForm (SelectInput r) = InputRating r
fieldSoToForm (OtherInput  o) = InputOther o
-}
