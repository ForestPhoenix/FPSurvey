module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X
import Foundation            as X
import Test.Hspec            as X
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X
import Test.QuickCheck       as X
import Test.QuickCheck.Poly  as X

import qualified Data.List.NonEmpty as NE

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
    arbitrary = (NE.:|) <$> arbitrary <*> arbitrary
    shrink (a NE.:| as) = (NE.:|) <$> shrink a <*> shrink as
