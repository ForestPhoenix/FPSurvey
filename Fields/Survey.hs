module Fields.Survey (
    radioTdField,
    genericRadioField,
    InputWithOther (..),
    radioTdFieldWithOther,
    genericRadioFieldWithOther
   ) where

import Import
import Data.Either.Utils
import Query.Types
import Text.Read (readMaybe)

radioTdField :: (Eq a) => [(a, Html)] -> Field Handler a
radioTdField = genericRadioField (\inner -> [whamlet|<td> ^{inner}|])

genericRadioField :: (Eq a) => (Widget -> Widget) -> [(a, Html)] -> Field Handler a
genericRadioField wrapper values = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [midx] ->
                case (readMaybe $ unpack midx) :: Maybe Int of
                    Just idx->
                        return $ maybeToEither "radioTdField: index out of range" $
                            Just <$> fst <$> lookup idx idxValAL
                    _ -> return $ Left "radioTdField: parsing failed"
            _ -> return $ Left "radioTdField: Value is required"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq -> do
        sequence $ (wrapper .
            (\(idx, (_, html)) ->
            [whamlet|
                <input id="#{idAttr}_#{idx}" name=#{nameAttr} *{otherAttrs} type="radio" value=#{idx} required="required">
                <label for="#{idAttr}_#{idx}"> #{html}
            |])) <$> idxValAL
        return ()
    , fieldEnctype = UrlEncoded
    }
    where
        idxValAL = zip ([1..] :: [Int]) $ values

radioTdFieldWithOther :: (Eq a) =>
    [(a, Html)] ->
    FieldView App ->
    FormResult b ->
    Field Handler (InputWithOther a b)
radioTdFieldWithOther = genericRadioFieldWithOther (\inner -> [whamlet|<td> ^{inner}|])

genericRadioFieldWithOther :: (Eq a) =>
    (Widget -> Widget) ->
    [(a, Html)] ->
    FieldView App ->
    FormResult b ->
    Field Handler (InputWithOther a b)

genericRadioFieldWithOther wrapper values otherView otherRes = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            ["otherwise"] ->
                return $ Right $ InputOther <$> formResultToMaybe otherRes
            [midx] ->
                case (readMaybe $ unpack midx) :: Maybe Int of
                    Just idx->
                        return $ maybeToEither "radioTdFieldWithOther: index out of range" $
                            Just <$> InputData . fst <$> lookup idx idxValAL
                    _ -> return $ Left "radioTdFieldWithOther: parsing failed"
            _ -> return $ Left "radioTdFieldWithOther: Value is required"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq -> do
        sequence $ (wrapper .
            (\(idx, (_, html)) ->
            [whamlet|
                <input id="#{idAttr}_#{idx}" name=#{nameAttr} *{otherAttrs} type="radio" value=#{idx} required="required">
                <label for="#{idAttr}_#{idx}"> #{html}
            |])) <$> idxValAL
        wrapper [whamlet|
            <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type="radio" value="otherwise">
            ^{fvInput otherView}
            |]
        return ()
    , fieldEnctype = UrlEncoded
    }
    where idxValAL = zip ([1..] :: [Int]) $ values

formResultToMaybe :: FormResult a -> Maybe a
formResultToMaybe (FormSuccess a) = Just a
formResultToMaybe _ = Nothing
