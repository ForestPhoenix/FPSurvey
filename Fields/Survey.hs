module Fields.Survey (
    radioTdField
) where

import Import
import Data.Either.Utils
import Text.Read (readMaybe)

radioTdField :: (Eq a, RenderMessage app FormMessage) => [a] -> Field (HandlerT app IO) a
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
