{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The object you use to delete web-usage data.
--
-- This class provides an easy way for you to delete web history, including:
--
-- - All history - History associated to a specific URL - History during a specific time interval
--
-- Generated bindings for @STWebHistory@.
module ObjC.ScreenTime.STWebHistory
  ( STWebHistory
  , IsSTWebHistory(..)
  , initWithBundleIdentifier_profileIdentifier_error
  , initWithProfileIdentifier
  , initWithBundleIdentifier_error
  , deleteHistoryForURL
  , deleteHistoryDuringInterval
  , deleteAllHistory
  , initWithBundleIdentifier_profileIdentifier_errorSelector
  , initWithProfileIdentifierSelector
  , initWithBundleIdentifier_errorSelector
  , deleteHistoryForURLSelector
  , deleteHistoryDuringIntervalSelector
  , deleteAllHistorySelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenTime.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a web history instance to delete web-usage data associated to the bundle identifier and profile identifier you specify.
--
-- The default value for @bundleIdentifier@ is @Bundle.main.bundleIdentifier@. This is the recommended identifier to use, except for example, if a helper process is presenting web UI and you want to group that web-usage under the main app’s bundle identifier.
--
-- The default value for @profileIdentifier@ is @nil@. This identifier can be used to delete browsing history for a specific profile. Using @nil@ will only delete web history reported without a profile identifier.
--
-- - Parameters:   - bundleIdentifier: The bundle identifier.   - profileIdentifier: The identifier of the current browsing profile.   - error: Any error that occurred while changing the bundle identifier.
--
-- ObjC selector: @- initWithBundleIdentifier:profileIdentifier:error:@
initWithBundleIdentifier_profileIdentifier_error :: (IsSTWebHistory stWebHistory, IsNSString bundleIdentifier, IsNSString profileIdentifier, IsNSError error_) => stWebHistory -> bundleIdentifier -> profileIdentifier -> error_ -> IO (Id STWebHistory)
initWithBundleIdentifier_profileIdentifier_error stWebHistory  bundleIdentifier profileIdentifier error_ =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
    withObjCPtr profileIdentifier $ \raw_profileIdentifier ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg stWebHistory (mkSelector "initWithBundleIdentifier:profileIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ()), argPtr (castPtr raw_profileIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a web history instance to delete web-usage data associated to the profile identifier you specify.
--
-- The default value for @profileIdentifier@ is @nil@. This identifier can be used to delete browsing history for a specific profile. Using @nil@ will only delete web history reported without a profile identifier.
--
-- - Parameters:   - profileIdentifier: The identifier of the current browsing profile.
--
-- ObjC selector: @- initWithProfileIdentifier:@
initWithProfileIdentifier :: (IsSTWebHistory stWebHistory, IsNSString profileIdentifier) => stWebHistory -> profileIdentifier -> IO (Id STWebHistory)
initWithProfileIdentifier stWebHistory  profileIdentifier =
  withObjCPtr profileIdentifier $ \raw_profileIdentifier ->
      sendMsg stWebHistory (mkSelector "initWithProfileIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_profileIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a web history instance to delete web-usage data associated to the bundle identifier you specify.
--
-- The default value for @bundleIdentifier@ is @Bundle.main.bundleIdentifier@. This is the recommended identifier to use, except for example, if a helper process is presenting web UI and you want to group that web-usage under the main app’s bundle identifier.
--
-- - Parameters:   - bundleIdentifier: The bundle identifier.   - error: Any error that occurred while changing the bundle identifier.
--
-- ObjC selector: @- initWithBundleIdentifier:error:@
initWithBundleIdentifier_error :: (IsSTWebHistory stWebHistory, IsNSString bundleIdentifier, IsNSError error_) => stWebHistory -> bundleIdentifier -> error_ -> IO (Id STWebHistory)
initWithBundleIdentifier_error stWebHistory  bundleIdentifier error_ =
  withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg stWebHistory (mkSelector "initWithBundleIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Deletes all the web history for the URL you specify.
--
-- The framework references the entire URL to determine which web-usage data to delete.
--
-- - Parameters:   - url: The URL associated with the web history to delete.
--
-- ObjC selector: @- deleteHistoryForURL:@
deleteHistoryForURL :: (IsSTWebHistory stWebHistory, IsNSURL url) => stWebHistory -> url -> IO ()
deleteHistoryForURL stWebHistory  url =
  withObjCPtr url $ \raw_url ->
      sendMsg stWebHistory (mkSelector "deleteHistoryForURL:") retVoid [argPtr (castPtr raw_url :: Ptr ())]

-- | Deletes web history that occurred during the date interval you specify.
--
-- - Parameters:   - interval: The date interval of web history you want to delete.
--
-- ObjC selector: @- deleteHistoryDuringInterval:@
deleteHistoryDuringInterval :: (IsSTWebHistory stWebHistory, IsNSDateInterval interval) => stWebHistory -> interval -> IO ()
deleteHistoryDuringInterval stWebHistory  interval =
  withObjCPtr interval $ \raw_interval ->
      sendMsg stWebHistory (mkSelector "deleteHistoryDuringInterval:") retVoid [argPtr (castPtr raw_interval :: Ptr ())]

-- | Deletes all web history associated with the bundle identifier you specified during initialization.
--
-- ObjC selector: @- deleteAllHistory@
deleteAllHistory :: IsSTWebHistory stWebHistory => stWebHistory -> IO ()
deleteAllHistory stWebHistory  =
    sendMsg stWebHistory (mkSelector "deleteAllHistory") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundleIdentifier:profileIdentifier:error:@
initWithBundleIdentifier_profileIdentifier_errorSelector :: Selector
initWithBundleIdentifier_profileIdentifier_errorSelector = mkSelector "initWithBundleIdentifier:profileIdentifier:error:"

-- | @Selector@ for @initWithProfileIdentifier:@
initWithProfileIdentifierSelector :: Selector
initWithProfileIdentifierSelector = mkSelector "initWithProfileIdentifier:"

-- | @Selector@ for @initWithBundleIdentifier:error:@
initWithBundleIdentifier_errorSelector :: Selector
initWithBundleIdentifier_errorSelector = mkSelector "initWithBundleIdentifier:error:"

-- | @Selector@ for @deleteHistoryForURL:@
deleteHistoryForURLSelector :: Selector
deleteHistoryForURLSelector = mkSelector "deleteHistoryForURL:"

-- | @Selector@ for @deleteHistoryDuringInterval:@
deleteHistoryDuringIntervalSelector :: Selector
deleteHistoryDuringIntervalSelector = mkSelector "deleteHistoryDuringInterval:"

-- | @Selector@ for @deleteAllHistory@
deleteAllHistorySelector :: Selector
deleteAllHistorySelector = mkSelector "deleteAllHistory"

