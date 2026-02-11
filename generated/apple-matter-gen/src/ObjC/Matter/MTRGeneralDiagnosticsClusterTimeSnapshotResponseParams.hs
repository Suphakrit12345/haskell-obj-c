{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams
  ( MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams
  , IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams(..)
  , initWithResponseValue_error
  , systemTimeMs
  , setSystemTimeMs
  , posixTimeMs
  , setPosixTimeMs
  , initWithResponseValue_errorSelector
  , systemTimeMsSelector
  , setSystemTimeMsSelector
  , posixTimeMsSelector
  , setPosixTimeMsSelector


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

-- | Initialize an MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> responseValue -> error_ -> IO (Id MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams)
initWithResponseValue_error mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- systemTimeMs@
systemTimeMs :: IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> IO (Id NSNumber)
systemTimeMs mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams  =
    sendMsg mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams (mkSelector "systemTimeMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSystemTimeMs:@
setSystemTimeMs :: (IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> value -> IO ()
setSystemTimeMs mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams (mkSelector "setSystemTimeMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- posixTimeMs@
posixTimeMs :: IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> IO (Id NSNumber)
posixTimeMs mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams  =
    sendMsg mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams (mkSelector "posixTimeMs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPosixTimeMs:@
setPosixTimeMs :: (IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> value -> IO ()
setPosixTimeMs mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams (mkSelector "setPosixTimeMs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @systemTimeMs@
systemTimeMsSelector :: Selector
systemTimeMsSelector = mkSelector "systemTimeMs"

-- | @Selector@ for @setSystemTimeMs:@
setSystemTimeMsSelector :: Selector
setSystemTimeMsSelector = mkSelector "setSystemTimeMs:"

-- | @Selector@ for @posixTimeMs@
posixTimeMsSelector :: Selector
posixTimeMsSelector = mkSelector "posixTimeMs"

-- | @Selector@ for @setPosixTimeMs:@
setPosixTimeMsSelector :: Selector
setPosixTimeMsSelector = mkSelector "setPosixTimeMs:"

