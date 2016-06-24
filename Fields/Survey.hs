module Fields.Survey (
    radioTdField,
    InputWithOther (..),
    radioTdFieldWithOther,
   ) where

import Import
import Data.Either.Utils
import Text.Read (readMaybe)

data InputWithOther a b = InputData a | InputOther b


radioTdField :: (Eq a) => [(a, Html)] -> Field Handler a
radioTdField values = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [midx] ->
                case (readMaybe $ unpack midx) :: Maybe Int of
                    Just idx->
                        return $ maybeToEither "radioTdField: index out of range" $
                            Just <$> fst <$> lookup idx idxValAL
                    _ -> return $ Left "radioTdField: parsing failed"
            _ -> return $ Left "radioTdField: Value is required"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            $forall (idx, (_, html)) <- idxValAL
                    <td>
                        <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type="radio" value=#{idx} required="required">
                        #{html}
        |]
    , fieldEnctype = UrlEncoded
    }
    where idxValAL = zip ([1..] :: [Int]) $ values

radioTdFieldWithOther :: (Eq a) =>
    [(a, Html)] ->
    FieldView App ->
    FormResult b ->
    Field Handler (InputWithOther a b)

radioTdFieldWithOther values otherView otherRes = Field
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
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            $forall (idx, (_, html)) <- idxValAL
                    <td>
                        <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type="radio" value=#{idx} required="required">
                        #{html}
            <td>
                <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type="radio" value="otherwise">
                ^{fvInput otherView}
        |]
    , fieldEnctype = UrlEncoded
    }
    where idxValAL = zip ([1..] :: [Int]) $ values

formResultToMaybe :: FormResult a -> Maybe a
formResultToMaybe (FormSuccess a) = Just a
formResultToMaybe _ = Nothing
