{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSoftwareDiagnosticsClusterSoftwareFaultEvent@.
module ObjC.Matter.MTRSoftwareDiagnosticsClusterSoftwareFaultEvent
  ( MTRSoftwareDiagnosticsClusterSoftwareFaultEvent
  , IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent(..)
  , id_
  , setId
  , name
  , setName
  , faultRecording
  , setFaultRecording
  , idSelector
  , setIdSelector
  , nameSelector
  , setNameSelector
  , faultRecordingSelector
  , setFaultRecordingSelector


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

-- | @- id@
id_ :: IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> IO (Id NSNumber)
id_ mtrSoftwareDiagnosticsClusterSoftwareFaultEvent  =
    sendMsg mtrSoftwareDiagnosticsClusterSoftwareFaultEvent (mkSelector "id") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setId:@
setId :: (IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent, IsNSNumber value) => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> value -> IO ()
setId mtrSoftwareDiagnosticsClusterSoftwareFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterSoftwareFaultEvent (mkSelector "setId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> IO (Id NSString)
name mtrSoftwareDiagnosticsClusterSoftwareFaultEvent  =
    sendMsg mtrSoftwareDiagnosticsClusterSoftwareFaultEvent (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent, IsNSString value) => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> value -> IO ()
setName mtrSoftwareDiagnosticsClusterSoftwareFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterSoftwareFaultEvent (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- faultRecording@
faultRecording :: IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> IO (Id NSData)
faultRecording mtrSoftwareDiagnosticsClusterSoftwareFaultEvent  =
    sendMsg mtrSoftwareDiagnosticsClusterSoftwareFaultEvent (mkSelector "faultRecording") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFaultRecording:@
setFaultRecording :: (IsMTRSoftwareDiagnosticsClusterSoftwareFaultEvent mtrSoftwareDiagnosticsClusterSoftwareFaultEvent, IsNSData value) => mtrSoftwareDiagnosticsClusterSoftwareFaultEvent -> value -> IO ()
setFaultRecording mtrSoftwareDiagnosticsClusterSoftwareFaultEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterSoftwareFaultEvent (mkSelector "setFaultRecording:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @faultRecording@
faultRecordingSelector :: Selector
faultRecordingSelector = mkSelector "faultRecording"

-- | @Selector@ for @setFaultRecording:@
setFaultRecordingSelector :: Selector
setFaultRecordingSelector = mkSelector "setFaultRecording:"

