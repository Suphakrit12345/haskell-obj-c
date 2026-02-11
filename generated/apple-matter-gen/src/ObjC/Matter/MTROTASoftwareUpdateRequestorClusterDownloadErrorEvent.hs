{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent@.
module ObjC.Matter.MTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent
  ( MTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent
  , IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent(..)
  , softwareVersion
  , setSoftwareVersion
  , bytesDownloaded
  , setBytesDownloaded
  , progressPercent
  , setProgressPercent
  , platformCode
  , setPlatformCode
  , softwareVersionSelector
  , setSoftwareVersionSelector
  , bytesDownloadedSelector
  , setBytesDownloadedSelector
  , progressPercentSelector
  , setProgressPercentSelector
  , platformCodeSelector
  , setPlatformCodeSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- softwareVersion@
softwareVersion :: IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent => mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent -> IO (Id NSNumber)
softwareVersion mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent (mkSelector "softwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent -> value -> IO ()
setSoftwareVersion mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent (mkSelector "setSoftwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bytesDownloaded@
bytesDownloaded :: IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent => mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent -> IO (Id NSNumber)
bytesDownloaded mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent (mkSelector "bytesDownloaded") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBytesDownloaded:@
setBytesDownloaded :: (IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent -> value -> IO ()
setBytesDownloaded mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent (mkSelector "setBytesDownloaded:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- progressPercent@
progressPercent :: IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent => mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent -> IO (Id NSNumber)
progressPercent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent (mkSelector "progressPercent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProgressPercent:@
setProgressPercent :: (IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent -> value -> IO ()
setProgressPercent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent (mkSelector "setProgressPercent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- platformCode@
platformCode :: IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent => mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent -> IO (Id NSNumber)
platformCode mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent (mkSelector "platformCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlatformCode:@
setPlatformCode :: (IsMTROtaSoftwareUpdateRequestorClusterDownloadErrorEvent mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent -> value -> IO ()
setPlatformCode mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterDownloadErrorEvent (mkSelector "setPlatformCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @bytesDownloaded@
bytesDownloadedSelector :: Selector
bytesDownloadedSelector = mkSelector "bytesDownloaded"

-- | @Selector@ for @setBytesDownloaded:@
setBytesDownloadedSelector :: Selector
setBytesDownloadedSelector = mkSelector "setBytesDownloaded:"

-- | @Selector@ for @progressPercent@
progressPercentSelector :: Selector
progressPercentSelector = mkSelector "progressPercent"

-- | @Selector@ for @setProgressPercent:@
setProgressPercentSelector :: Selector
setProgressPercentSelector = mkSelector "setProgressPercent:"

-- | @Selector@ for @platformCode@
platformCodeSelector :: Selector
platformCodeSelector = mkSelector "platformCode"

-- | @Selector@ for @setPlatformCode:@
setPlatformCodeSelector :: Selector
setPlatformCodeSelector = mkSelector "setPlatformCode:"

