{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Chime    This cluster provides facilities to configure and play Chime sounds, such as those used in a doorbell.
--
-- Generated bindings for @MTRClusterChime@.
module ObjC.Matter.MTRClusterChime
  ( MTRClusterChime
  , IsMTRClusterChime(..)
  , playChimeSoundWithParams_expectedValues_expectedValueInterval_completion
  , playChimeSoundWithExpectedValues_expectedValueInterval_completion
  , readAttributeInstalledChimeSoundsWithParams
  , readAttributeSelectedChimeWithParams
  , writeAttributeSelectedChimeWithValue_expectedValueInterval
  , writeAttributeSelectedChimeWithValue_expectedValueInterval_params
  , readAttributeEnabledWithParams
  , writeAttributeEnabledWithValue_expectedValueInterval
  , writeAttributeEnabledWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , playChimeSoundWithParams_expectedValues_expectedValueInterval_completionSelector
  , playChimeSoundWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeInstalledChimeSoundsWithParamsSelector
  , readAttributeSelectedChimeWithParamsSelector
  , writeAttributeSelectedChimeWithValue_expectedValueIntervalSelector
  , writeAttributeSelectedChimeWithValue_expectedValueInterval_paramsSelector
  , readAttributeEnabledWithParamsSelector
  , writeAttributeEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeEnabledWithValue_expectedValueInterval_paramsSelector
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

-- | @- playChimeSoundWithParams:expectedValues:expectedValueInterval:completion:@
playChimeSoundWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChime mtrClusterChime, IsMTRChimeClusterPlayChimeSoundParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChime -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
playChimeSoundWithParams_expectedValues_expectedValueInterval_completion mtrClusterChime  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterChime (mkSelector "playChimeSoundWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- playChimeSoundWithExpectedValues:expectedValueInterval:completion:@
playChimeSoundWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterChime mtrClusterChime, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterChime -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
playChimeSoundWithExpectedValues_expectedValueInterval_completion mtrClusterChime  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterChime (mkSelector "playChimeSoundWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeInstalledChimeSoundsWithParams:@
readAttributeInstalledChimeSoundsWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeInstalledChimeSoundsWithParams mtrClusterChime  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChime (mkSelector "readAttributeInstalledChimeSoundsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSelectedChimeWithParams:@
readAttributeSelectedChimeWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeSelectedChimeWithParams mtrClusterChime  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChime (mkSelector "readAttributeSelectedChimeWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeSelectedChimeWithValue:expectedValueInterval:@
writeAttributeSelectedChimeWithValue_expectedValueInterval :: (IsMTRClusterChime mtrClusterChime, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterChime -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSelectedChimeWithValue_expectedValueInterval mtrClusterChime  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterChime (mkSelector "writeAttributeSelectedChimeWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeSelectedChimeWithValue:expectedValueInterval:params:@
writeAttributeSelectedChimeWithValue_expectedValueInterval_params :: (IsMTRClusterChime mtrClusterChime, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterChime -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSelectedChimeWithValue_expectedValueInterval_params mtrClusterChime  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterChime (mkSelector "writeAttributeSelectedChimeWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeEnabledWithParams:@
readAttributeEnabledWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeEnabledWithParams mtrClusterChime  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChime (mkSelector "readAttributeEnabledWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeAttributeEnabledWithValue:expectedValueInterval:@
writeAttributeEnabledWithValue_expectedValueInterval :: (IsMTRClusterChime mtrClusterChime, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterChime -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeEnabledWithValue_expectedValueInterval mtrClusterChime  dataValueDictionary expectedValueIntervalMs =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterChime (mkSelector "writeAttributeEnabledWithValue:expectedValueInterval:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ())]

-- | @- writeAttributeEnabledWithValue:expectedValueInterval:params:@
writeAttributeEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterChime mtrClusterChime, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterChime -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeEnabledWithValue_expectedValueInterval_params mtrClusterChime  dataValueDictionary expectedValueIntervalMs params =
  withObjCPtr dataValueDictionary $ \raw_dataValueDictionary ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
      withObjCPtr params $ \raw_params ->
          sendMsg mtrClusterChime (mkSelector "writeAttributeEnabledWithValue:expectedValueInterval:params:") retVoid [argPtr (castPtr raw_dataValueDictionary :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())]

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterChime  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChime (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterChime  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChime (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterChime  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChime (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterChime  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChime (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterChime  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterChime (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterChime mtrClusterChime => mtrClusterChime -> IO (Id MTRClusterChime)
init_ mtrClusterChime  =
    sendMsg mtrClusterChime (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterChime)
new  =
  do
    cls' <- getRequiredClass "MTRClusterChime"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterChime mtrClusterChime, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterChime -> device -> endpointID -> queue -> IO (Id MTRClusterChime)
initWithDevice_endpointID_queue mtrClusterChime  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterChime (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playChimeSoundWithParams:expectedValues:expectedValueInterval:completion:@
playChimeSoundWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
playChimeSoundWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "playChimeSoundWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @playChimeSoundWithExpectedValues:expectedValueInterval:completion:@
playChimeSoundWithExpectedValues_expectedValueInterval_completionSelector :: Selector
playChimeSoundWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "playChimeSoundWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeInstalledChimeSoundsWithParams:@
readAttributeInstalledChimeSoundsWithParamsSelector :: Selector
readAttributeInstalledChimeSoundsWithParamsSelector = mkSelector "readAttributeInstalledChimeSoundsWithParams:"

-- | @Selector@ for @readAttributeSelectedChimeWithParams:@
readAttributeSelectedChimeWithParamsSelector :: Selector
readAttributeSelectedChimeWithParamsSelector = mkSelector "readAttributeSelectedChimeWithParams:"

-- | @Selector@ for @writeAttributeSelectedChimeWithValue:expectedValueInterval:@
writeAttributeSelectedChimeWithValue_expectedValueIntervalSelector :: Selector
writeAttributeSelectedChimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSelectedChimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSelectedChimeWithValue:expectedValueInterval:params:@
writeAttributeSelectedChimeWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeSelectedChimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSelectedChimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeEnabledWithParams:@
readAttributeEnabledWithParamsSelector :: Selector
readAttributeEnabledWithParamsSelector = mkSelector "readAttributeEnabledWithParams:"

-- | @Selector@ for @writeAttributeEnabledWithValue:expectedValueInterval:@
writeAttributeEnabledWithValue_expectedValueIntervalSelector :: Selector
writeAttributeEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeEnabledWithValue:expectedValueInterval:params:@
writeAttributeEnabledWithValue_expectedValueInterval_paramsSelector :: Selector
writeAttributeEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeEnabledWithValue:expectedValueInterval:params:"

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

