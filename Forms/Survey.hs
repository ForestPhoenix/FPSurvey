module Forms.Survey where
{-
import Import

import Database.DbAbstraction
import Model.Survey
import qualified Data.Text as Text

import Fields.Survey

data (SqlId sqlId) => SurveyInput sqlId = InputRating (Rating sqlId) | InputOther Text

surveyForm :: (SqlId a) =>
    UntakenSurvey a ->
    Html ->
    MForm Handler (FormResult (Text, [(Question a, SurveyInput a)]), Widget)
    
surveyForm (sections) extra = do
    let subForms = map sectionForm sections
    (userRes, userView) <- mreq textField "" Nothing
    subFormsRes <- sequence (map (\form -> form $ toHtml $ Text.pack "") subForms)
    let (subRes, subWidgets) = unzip $ subFormsRes
    let widget = do
            $(widgetFile "survey")
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
    let result = (,) <$> userRes <*> shiftApplicativeList subRes
    return (result, widget)

sectionForm :: (SqlId a) =>
    UntakenSurveySection a ->
    Html ->
    MForm Handler (FormResult [(Question a, SurveyInput a)], Widget)
    
sectionForm (section, groups) _ = do
    let subForms = map groupForm groups
    subFormsRes <- sequence (map (\form -> form $ toHtml $ Text.pack "") subForms)
    let (subRes, subWidgets) = unzip subFormsRes
    let widget = [whamlet|
        <div class="survey_section">
            <h2> <li> #{section_title section}
            <ol> 
                $forall subWidget <- subWidgets
                    ^{subWidget}
        |]
    let result = shiftApplicativeList subRes
    return (result, widget)

groupForm :: (SqlId a) =>
    UntakenSurveyGroup a ->
    Html ->
    MForm Handler (FormResult [(Question a, SurveyInput a)], Widget)
    
groupForm (qGroup, qtype, questions, ratings) _ = do
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
