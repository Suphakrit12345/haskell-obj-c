{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Time Synchronization    Accurate time is required for a number of reasons, including scheduling, display and validating security materials.
--
-- Generated bindings for @MTRClusterTimeSynchronization@.
module ObjC.Matter.MTRClusterTimeSynchronization
  ( MTRClusterTimeSynchronization
  , IsMTRClusterTimeSynchronization(..)
  , setUTCTimeWithParams_expectedValues_expectedValueInterval_completion
  , setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completion
  , setTimeZoneWithParams_expectedValues_expectedValueInterval_completion
  , setDSTOffsetWithParams_expectedValues_expectedValueInterval_completion
  , setDefaultNTPWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeUTCTimeWithParams
  , readAttributeGranularityWithParams
  , readAttributeTimeSourceWithParams
  , readAttributeTrustedTimeSourceWithParams
  , readAttributeDefaultNTPWithParams
  , readAttributeTimeZoneWithParams
  , readAttributeDSTOffsetWithParams
  , readAttributeLocalTimeWithParams
  , readAttributeTimeZoneDatabaseWithParams
  , readAttributeNTPServerAvailableWithParams
  , readAttributeTimeZoneListMaxSizeWithParams
  , readAttributeDSTOffsetListMaxSizeWithParams
  , readAttributeSupportsDNSResolveWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , setUTCTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTimeZoneWithParams_expectedValues_expectedValueInterval_completionSelector
  , setDSTOffsetWithParams_expectedValues_expectedValueInterval_completionSelector
  , setDefaultNTPWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeUTCTimeWithParamsSelector
  , readAttributeGranularityWithParamsSelector
  , readAttributeTimeSourceWithParamsSelector
  , readAttributeTrustedTimeSourceWithParamsSelector
  , readAttributeDefaultNTPWithParamsSelector
  , readAttributeTimeZoneWithParamsSelector
  , readAttributeDSTOffsetWithParamsSelector
  , readAttributeLocalTimeWithParamsSelector
  , readAttributeTimeZoneDatabaseWithParamsSelector
  , readAttributeNTPServerAvailableWithParamsSelector
  , readAttributeTimeZoneListMaxSizeWithParamsSelector
  , readAttributeDSTOffsetListMaxSizeWithParamsSelector
  , readAttributeSupportsDNSResolveWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpointID_queueSelector


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

-- | @- setUTCTimeWithParams:expectedValues:expectedValueInterval:completion:@
setUTCTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetUTCTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setUTCTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimeSynchronization (mkSelector "setUTCTimeWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTrustedTimeSourceWithParams:expectedValues:expectedValueInterval:completion:@
setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetTrustedTimeSourceParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimeSynchronization (mkSelector "setTrustedTimeSourceWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setTimeZoneWithParams:expectedValues:expectedValueInterval:completion:@
setTimeZoneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetTimeZoneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTimeZoneWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimeSynchronization (mkSelector "setTimeZoneWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setDSTOffsetWithParams:expectedValues:expectedValueInterval:completion:@
setDSTOffsetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetDSTOffsetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setDSTOffsetWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimeSynchronization (mkSelector "setDSTOffsetWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- setDefaultNTPWithParams:expectedValues:expectedValueInterval:completion:@
setDefaultNTPWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetDefaultNTPParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setDefaultNTPWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterTimeSynchronization (mkSelector "setDefaultNTPWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeUTCTimeWithParams:@
readAttributeUTCTimeWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeUTCTimeWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeUTCTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGranularityWithParams:@
readAttributeGranularityWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeGranularityWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeGranularityWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTimeSourceWithParams:@
readAttributeTimeSourceWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTimeSourceWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeTimeSourceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTrustedTimeSourceWithParams:@
readAttributeTrustedTimeSourceWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTrustedTimeSourceWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeTrustedTimeSourceWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDefaultNTPWithParams:@
readAttributeDefaultNTPWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeDefaultNTPWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeDefaultNTPWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTimeZoneWithParams:@
readAttributeTimeZoneWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTimeZoneWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeTimeZoneWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDSTOffsetWithParams:@
readAttributeDSTOffsetWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeDSTOffsetWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeDSTOffsetWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLocalTimeWithParams:@
readAttributeLocalTimeWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeLocalTimeWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeLocalTimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTimeZoneDatabaseWithParams:@
readAttributeTimeZoneDatabaseWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTimeZoneDatabaseWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeTimeZoneDatabaseWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNTPServerAvailableWithParams:@
readAttributeNTPServerAvailableWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeNTPServerAvailableWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeNTPServerAvailableWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTimeZoneListMaxSizeWithParams:@
readAttributeTimeZoneListMaxSizeWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTimeZoneListMaxSizeWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeTimeZoneListMaxSizeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDSTOffsetListMaxSizeWithParams:@
readAttributeDSTOffsetListMaxSizeWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeDSTOffsetListMaxSizeWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeDSTOffsetListMaxSizeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSupportsDNSResolveWithParams:@
readAttributeSupportsDNSResolveWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeSupportsDNSResolveWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeSupportsDNSResolveWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTimeSynchronization  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterTimeSynchronization (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization => mtrClusterTimeSynchronization -> IO (Id MTRClusterTimeSynchronization)
init_ mtrClusterTimeSynchronization  =
    sendMsg mtrClusterTimeSynchronization (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterTimeSynchronization)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTimeSynchronization"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTimeSynchronization -> device -> endpointID -> queue -> IO (Id MTRClusterTimeSynchronization)
initWithDevice_endpointID_queue mtrClusterTimeSynchronization  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterTimeSynchronization (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setUTCTimeWithParams:expectedValues:expectedValueInterval:completion:@
setUTCTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setUTCTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setUTCTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTrustedTimeSourceWithParams:expectedValues:expectedValueInterval:completion:@
setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTrustedTimeSourceWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTimeZoneWithParams:expectedValues:expectedValueInterval:completion:@
setTimeZoneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setTimeZoneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTimeZoneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setDSTOffsetWithParams:expectedValues:expectedValueInterval:completion:@
setDSTOffsetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setDSTOffsetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setDSTOffsetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setDefaultNTPWithParams:expectedValues:expectedValueInterval:completion:@
setDefaultNTPWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
setDefaultNTPWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setDefaultNTPWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeUTCTimeWithParams:@
readAttributeUTCTimeWithParamsSelector :: Selector
readAttributeUTCTimeWithParamsSelector = mkSelector "readAttributeUTCTimeWithParams:"

-- | @Selector@ for @readAttributeGranularityWithParams:@
readAttributeGranularityWithParamsSelector :: Selector
readAttributeGranularityWithParamsSelector = mkSelector "readAttributeGranularityWithParams:"

-- | @Selector@ for @readAttributeTimeSourceWithParams:@
readAttributeTimeSourceWithParamsSelector :: Selector
readAttributeTimeSourceWithParamsSelector = mkSelector "readAttributeTimeSourceWithParams:"

-- | @Selector@ for @readAttributeTrustedTimeSourceWithParams:@
readAttributeTrustedTimeSourceWithParamsSelector :: Selector
readAttributeTrustedTimeSourceWithParamsSelector = mkSelector "readAttributeTrustedTimeSourceWithParams:"

-- | @Selector@ for @readAttributeDefaultNTPWithParams:@
readAttributeDefaultNTPWithParamsSelector :: Selector
readAttributeDefaultNTPWithParamsSelector = mkSelector "readAttributeDefaultNTPWithParams:"

-- | @Selector@ for @readAttributeTimeZoneWithParams:@
readAttributeTimeZoneWithParamsSelector :: Selector
readAttributeTimeZoneWithParamsSelector = mkSelector "readAttributeTimeZoneWithParams:"

-- | @Selector@ for @readAttributeDSTOffsetWithParams:@
readAttributeDSTOffsetWithParamsSelector :: Selector
readAttributeDSTOffsetWithParamsSelector = mkSelector "readAttributeDSTOffsetWithParams:"

-- | @Selector@ for @readAttributeLocalTimeWithParams:@
readAttributeLocalTimeWithParamsSelector :: Selector
readAttributeLocalTimeWithParamsSelector = mkSelector "readAttributeLocalTimeWithParams:"

-- | @Selector@ for @readAttributeTimeZoneDatabaseWithParams:@
readAttributeTimeZoneDatabaseWithParamsSelector :: Selector
readAttributeTimeZoneDatabaseWithParamsSelector = mkSelector "readAttributeTimeZoneDatabaseWithParams:"

-- | @Selector@ for @readAttributeNTPServerAvailableWithParams:@
readAttributeNTPServerAvailableWithParamsSelector :: Selector
readAttributeNTPServerAvailableWithParamsSelector = mkSelector "readAttributeNTPServerAvailableWithParams:"

-- | @Selector@ for @readAttributeTimeZoneListMaxSizeWithParams:@
readAttributeTimeZoneListMaxSizeWithParamsSelector :: Selector
readAttributeTimeZoneListMaxSizeWithParamsSelector = mkSelector "readAttributeTimeZoneListMaxSizeWithParams:"

-- | @Selector@ for @readAttributeDSTOffsetListMaxSizeWithParams:@
readAttributeDSTOffsetListMaxSizeWithParamsSelector :: Selector
readAttributeDSTOffsetListMaxSizeWithParamsSelector = mkSelector "readAttributeDSTOffsetListMaxSizeWithParams:"

-- | @Selector@ for @readAttributeSupportsDNSResolveWithParams:@
readAttributeSupportsDNSResolveWithParamsSelector :: Selector
readAttributeSupportsDNSResolveWithParamsSelector = mkSelector "readAttributeSupportsDNSResolveWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

