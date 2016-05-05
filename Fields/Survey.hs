module Fields.Survey (
    radioTdField,
   SelectWithOther (..),
   radioTdFieldWithOther,
) where

import Import
import Data.Either.Utils
import Text.Read (readMaybe)

radioTdField :: (Eq a) => [a] -> Field Handler a
radioTdField values = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [midx] -> 
                case (readMaybe $ unpack midx) :: Maybe Int of
                    Just idx->
                        return $ maybeToEither "radioTdField: index out of range" $ Just <$> lookup idx idxValAL
                    _ -> return $ Left "radioTdField: parsing failed"
            _ -> return $ Left "radioTdField: Value is required"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            $forall (idx, _) <- idxValAL
                    <td> 
                        <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type="radio" value=#{idx} required="required">
        |]
    , fieldEnctype = UrlEncoded
    }
    where idxValAL = zip ([1..] :: [Int]) $ values


data SelectWithOther a b = SelectInput a | OtherInput b

radioTdFieldWithOther :: (Eq a) =>
    [a] ->
    FieldView App ->
    FormResult b ->
    Field Handler (SelectWithOther a b)
    
radioTdFieldWithOther values otherView otherRes = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            ["otherwise"] ->
                return $ Right $ OtherInput <$> formResultToMaybe otherRes
            [midx] -> 
                case (readMaybe $ unpack midx) :: Maybe Int of
                    Just idx->
                        return $ maybeToEither "radioTdFieldWithOther: index out of range" $ 
                            fmap Just $ fmap SelectInput $ lookup idx idxValAL
                    _ -> return $ Left "radioTdFieldWithOther: parsing failed"
            _ -> return $ Left "radioTdFieldWithOther: Value is required"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            $forall (idx, _) <- idxValAL
                    <td> 
                        <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type="radio" value=#{idx} required="required">
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
