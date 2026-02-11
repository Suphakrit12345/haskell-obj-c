{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterNeighborTableStruct@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterNeighborTableStruct
  ( MTRThreadNetworkDiagnosticsClusterNeighborTableStruct
  , IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct(..)
  , extAddress
  , setExtAddress
  , age
  , setAge
  , rloc16
  , setRloc16
  , linkFrameCounter
  , setLinkFrameCounter
  , mleFrameCounter
  , setMleFrameCounter
  , lqi
  , setLqi
  , averageRssi
  , setAverageRssi
  , lastRssi
  , setLastRssi
  , frameErrorRate
  , setFrameErrorRate
  , messageErrorRate
  , setMessageErrorRate
  , rxOnWhenIdle
  , setRxOnWhenIdle
  , fullThreadDevice
  , setFullThreadDevice
  , fullNetworkData
  , setFullNetworkData
  , isChild
  , setIsChild
  , extAddressSelector
  , setExtAddressSelector
  , ageSelector
  , setAgeSelector
  , rloc16Selector
  , setRloc16Selector
  , linkFrameCounterSelector
  , setLinkFrameCounterSelector
  , mleFrameCounterSelector
  , setMleFrameCounterSelector
  , lqiSelector
  , setLqiSelector
  , averageRssiSelector
  , setAverageRssiSelector
  , lastRssiSelector
  , setLastRssiSelector
  , frameErrorRateSelector
  , setFrameErrorRateSelector
  , messageErrorRateSelector
  , setMessageErrorRateSelector
  , rxOnWhenIdleSelector
  , setRxOnWhenIdleSelector
  , fullThreadDeviceSelector
  , setFullThreadDeviceSelector
  , fullNetworkDataSelector
  , setFullNetworkDataSelector
  , isChildSelector
  , setIsChildSelector


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

-- | @- extAddress@
extAddress :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
extAddress mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "extAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtAddress:@
setExtAddress :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setExtAddress mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setExtAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- age@
age :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
age mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "age") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAge:@
setAge :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setAge mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setAge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rloc16@
rloc16 :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
rloc16 mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "rloc16") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRloc16:@
setRloc16 :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setRloc16 mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setRloc16:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- linkFrameCounter@
linkFrameCounter :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
linkFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "linkFrameCounter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLinkFrameCounter:@
setLinkFrameCounter :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setLinkFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setLinkFrameCounter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mleFrameCounter@
mleFrameCounter :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
mleFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "mleFrameCounter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMleFrameCounter:@
setMleFrameCounter :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setMleFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setMleFrameCounter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lqi@
lqi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
lqi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "lqi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLqi:@
setLqi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setLqi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setLqi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- averageRssi@
averageRssi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
averageRssi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "averageRssi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAverageRssi:@
setAverageRssi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setAverageRssi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setAverageRssi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lastRssi@
lastRssi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
lastRssi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "lastRssi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLastRssi:@
setLastRssi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setLastRssi mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setLastRssi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- frameErrorRate@
frameErrorRate :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
frameErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "frameErrorRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFrameErrorRate:@
setFrameErrorRate :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setFrameErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setFrameErrorRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- messageErrorRate@
messageErrorRate :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
messageErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "messageErrorRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageErrorRate:@
setMessageErrorRate :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setMessageErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setMessageErrorRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rxOnWhenIdle@
rxOnWhenIdle :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
rxOnWhenIdle mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "rxOnWhenIdle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRxOnWhenIdle:@
setRxOnWhenIdle :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setRxOnWhenIdle mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setRxOnWhenIdle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fullThreadDevice@
fullThreadDevice :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
fullThreadDevice mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "fullThreadDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFullThreadDevice:@
setFullThreadDevice :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setFullThreadDevice mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setFullThreadDevice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fullNetworkData@
fullNetworkData :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
fullNetworkData mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "fullNetworkData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFullNetworkData:@
setFullNetworkData :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setFullNetworkData mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setFullNetworkData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isChild@
isChild :: IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> IO (Id NSNumber)
isChild mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "isChild") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIsChild:@
setIsChild :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTableStruct mtrThreadNetworkDiagnosticsClusterNeighborTableStruct, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTableStruct -> value -> IO ()
setIsChild mtrThreadNetworkDiagnosticsClusterNeighborTableStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTableStruct (mkSelector "setIsChild:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @extAddress@
extAddressSelector :: Selector
extAddressSelector = mkSelector "extAddress"

-- | @Selector@ for @setExtAddress:@
setExtAddressSelector :: Selector
setExtAddressSelector = mkSelector "setExtAddress:"

-- | @Selector@ for @age@
ageSelector :: Selector
ageSelector = mkSelector "age"

-- | @Selector@ for @setAge:@
setAgeSelector :: Selector
setAgeSelector = mkSelector "setAge:"

-- | @Selector@ for @rloc16@
rloc16Selector :: Selector
rloc16Selector = mkSelector "rloc16"

-- | @Selector@ for @setRloc16:@
setRloc16Selector :: Selector
setRloc16Selector = mkSelector "setRloc16:"

-- | @Selector@ for @linkFrameCounter@
linkFrameCounterSelector :: Selector
linkFrameCounterSelector = mkSelector "linkFrameCounter"

-- | @Selector@ for @setLinkFrameCounter:@
setLinkFrameCounterSelector :: Selector
setLinkFrameCounterSelector = mkSelector "setLinkFrameCounter:"

-- | @Selector@ for @mleFrameCounter@
mleFrameCounterSelector :: Selector
mleFrameCounterSelector = mkSelector "mleFrameCounter"

-- | @Selector@ for @setMleFrameCounter:@
setMleFrameCounterSelector :: Selector
setMleFrameCounterSelector = mkSelector "setMleFrameCounter:"

-- | @Selector@ for @lqi@
lqiSelector :: Selector
lqiSelector = mkSelector "lqi"

-- | @Selector@ for @setLqi:@
setLqiSelector :: Selector
setLqiSelector = mkSelector "setLqi:"

-- | @Selector@ for @averageRssi@
averageRssiSelector :: Selector
averageRssiSelector = mkSelector "averageRssi"

-- | @Selector@ for @setAverageRssi:@
setAverageRssiSelector :: Selector
setAverageRssiSelector = mkSelector "setAverageRssi:"

-- | @Selector@ for @lastRssi@
lastRssiSelector :: Selector
lastRssiSelector = mkSelector "lastRssi"

-- | @Selector@ for @setLastRssi:@
setLastRssiSelector :: Selector
setLastRssiSelector = mkSelector "setLastRssi:"

-- | @Selector@ for @frameErrorRate@
frameErrorRateSelector :: Selector
frameErrorRateSelector = mkSelector "frameErrorRate"

-- | @Selector@ for @setFrameErrorRate:@
setFrameErrorRateSelector :: Selector
setFrameErrorRateSelector = mkSelector "setFrameErrorRate:"

-- | @Selector@ for @messageErrorRate@
messageErrorRateSelector :: Selector
messageErrorRateSelector = mkSelector "messageErrorRate"

-- | @Selector@ for @setMessageErrorRate:@
setMessageErrorRateSelector :: Selector
setMessageErrorRateSelector = mkSelector "setMessageErrorRate:"

-- | @Selector@ for @rxOnWhenIdle@
rxOnWhenIdleSelector :: Selector
rxOnWhenIdleSelector = mkSelector "rxOnWhenIdle"

-- | @Selector@ for @setRxOnWhenIdle:@
setRxOnWhenIdleSelector :: Selector
setRxOnWhenIdleSelector = mkSelector "setRxOnWhenIdle:"

-- | @Selector@ for @fullThreadDevice@
fullThreadDeviceSelector :: Selector
fullThreadDeviceSelector = mkSelector "fullThreadDevice"

-- | @Selector@ for @setFullThreadDevice:@
setFullThreadDeviceSelector :: Selector
setFullThreadDeviceSelector = mkSelector "setFullThreadDevice:"

-- | @Selector@ for @fullNetworkData@
fullNetworkDataSelector :: Selector
fullNetworkDataSelector = mkSelector "fullNetworkData"

-- | @Selector@ for @setFullNetworkData:@
setFullNetworkDataSelector :: Selector
setFullNetworkDataSelector = mkSelector "setFullNetworkData:"

-- | @Selector@ for @isChild@
isChildSelector :: Selector
isChildSelector = mkSelector "isChild"

-- | @Selector@ for @setIsChild:@
setIsChildSelector :: Selector
setIsChildSelector = mkSelector "setIsChild:"

