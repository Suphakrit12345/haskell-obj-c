{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDiagnosticsClusterNeighborTable@.
module ObjC.Matter.MTRThreadNetworkDiagnosticsClusterNeighborTable
  ( MTRThreadNetworkDiagnosticsClusterNeighborTable
  , IsMTRThreadNetworkDiagnosticsClusterNeighborTable(..)
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
extAddress :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
extAddress mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "extAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtAddress:@
setExtAddress :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setExtAddress mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setExtAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- age@
age :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
age mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "age") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAge:@
setAge :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setAge mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setAge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rloc16@
rloc16 :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
rloc16 mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "rloc16") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRloc16:@
setRloc16 :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setRloc16 mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setRloc16:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- linkFrameCounter@
linkFrameCounter :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
linkFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "linkFrameCounter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLinkFrameCounter:@
setLinkFrameCounter :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setLinkFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setLinkFrameCounter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mleFrameCounter@
mleFrameCounter :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
mleFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "mleFrameCounter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMleFrameCounter:@
setMleFrameCounter :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setMleFrameCounter mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setMleFrameCounter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lqi@
lqi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
lqi mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "lqi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLqi:@
setLqi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setLqi mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setLqi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- averageRssi@
averageRssi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
averageRssi mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "averageRssi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAverageRssi:@
setAverageRssi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setAverageRssi mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setAverageRssi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lastRssi@
lastRssi :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
lastRssi mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "lastRssi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLastRssi:@
setLastRssi :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setLastRssi mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setLastRssi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- frameErrorRate@
frameErrorRate :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
frameErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "frameErrorRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFrameErrorRate:@
setFrameErrorRate :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setFrameErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setFrameErrorRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- messageErrorRate@
messageErrorRate :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
messageErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "messageErrorRate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageErrorRate:@
setMessageErrorRate :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setMessageErrorRate mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setMessageErrorRate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rxOnWhenIdle@
rxOnWhenIdle :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
rxOnWhenIdle mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "rxOnWhenIdle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRxOnWhenIdle:@
setRxOnWhenIdle :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setRxOnWhenIdle mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setRxOnWhenIdle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fullThreadDevice@
fullThreadDevice :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
fullThreadDevice mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "fullThreadDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFullThreadDevice:@
setFullThreadDevice :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setFullThreadDevice mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setFullThreadDevice:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fullNetworkData@
fullNetworkData :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
fullNetworkData mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "fullNetworkData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFullNetworkData:@
setFullNetworkData :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setFullNetworkData mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setFullNetworkData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isChild@
isChild :: IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable => mtrThreadNetworkDiagnosticsClusterNeighborTable -> IO (Id NSNumber)
isChild mtrThreadNetworkDiagnosticsClusterNeighborTable  =
    sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "isChild") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIsChild:@
setIsChild :: (IsMTRThreadNetworkDiagnosticsClusterNeighborTable mtrThreadNetworkDiagnosticsClusterNeighborTable, IsNSNumber value) => mtrThreadNetworkDiagnosticsClusterNeighborTable -> value -> IO ()
setIsChild mtrThreadNetworkDiagnosticsClusterNeighborTable  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrThreadNetworkDiagnosticsClusterNeighborTable (mkSelector "setIsChild:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

