{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterThreadInterfaceScanResult@.
module ObjC.Matter.MTRNetworkCommissioningClusterThreadInterfaceScanResult
  ( MTRNetworkCommissioningClusterThreadInterfaceScanResult
  , IsMTRNetworkCommissioningClusterThreadInterfaceScanResult(..)
  , panId
  , setPanId
  , extendedPanId
  , setExtendedPanId
  , networkName
  , setNetworkName
  , channel
  , setChannel
  , version
  , setVersion
  , extendedAddress
  , setExtendedAddress
  , rssi
  , setRssi
  , lqi
  , setLqi
  , panIdSelector
  , setPanIdSelector
  , extendedPanIdSelector
  , setExtendedPanIdSelector
  , networkNameSelector
  , setNetworkNameSelector
  , channelSelector
  , setChannelSelector
  , versionSelector
  , setVersionSelector
  , extendedAddressSelector
  , setExtendedAddressSelector
  , rssiSelector
  , setRssiSelector
  , lqiSelector
  , setLqiSelector


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

-- | @- panId@
panId :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
panId mtrNetworkCommissioningClusterThreadInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "panId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPanId:@
setPanId :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setPanId mtrNetworkCommissioningClusterThreadInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "setPanId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extendedPanId@
extendedPanId :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
extendedPanId mtrNetworkCommissioningClusterThreadInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "extendedPanId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtendedPanId:@
setExtendedPanId :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setExtendedPanId mtrNetworkCommissioningClusterThreadInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "setExtendedPanId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- networkName@
networkName :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSString)
networkName mtrNetworkCommissioningClusterThreadInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "networkName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkName:@
setNetworkName :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSString value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setNetworkName mtrNetworkCommissioningClusterThreadInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "setNetworkName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channel@
channel :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
channel mtrNetworkCommissioningClusterThreadInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "channel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannel:@
setChannel :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setChannel mtrNetworkCommissioningClusterThreadInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "setChannel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- version@
version :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
version mtrNetworkCommissioningClusterThreadInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVersion:@
setVersion :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setVersion mtrNetworkCommissioningClusterThreadInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "setVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extendedAddress@
extendedAddress :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSData)
extendedAddress mtrNetworkCommissioningClusterThreadInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "extendedAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtendedAddress:@
setExtendedAddress :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSData value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setExtendedAddress mtrNetworkCommissioningClusterThreadInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "setExtendedAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rssi@
rssi :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
rssi mtrNetworkCommissioningClusterThreadInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "rssi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRssi:@
setRssi :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setRssi mtrNetworkCommissioningClusterThreadInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "setRssi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lqi@
lqi :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> IO (Id NSNumber)
lqi mtrNetworkCommissioningClusterThreadInterfaceScanResult  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "lqi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLqi:@
setLqi :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResult mtrNetworkCommissioningClusterThreadInterfaceScanResult, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResult -> value -> IO ()
setLqi mtrNetworkCommissioningClusterThreadInterfaceScanResult  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResult (mkSelector "setLqi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @panId@
panIdSelector :: Selector
panIdSelector = mkSelector "panId"

-- | @Selector@ for @setPanId:@
setPanIdSelector :: Selector
setPanIdSelector = mkSelector "setPanId:"

-- | @Selector@ for @extendedPanId@
extendedPanIdSelector :: Selector
extendedPanIdSelector = mkSelector "extendedPanId"

-- | @Selector@ for @setExtendedPanId:@
setExtendedPanIdSelector :: Selector
setExtendedPanIdSelector = mkSelector "setExtendedPanId:"

-- | @Selector@ for @networkName@
networkNameSelector :: Selector
networkNameSelector = mkSelector "networkName"

-- | @Selector@ for @setNetworkName:@
setNetworkNameSelector :: Selector
setNetworkNameSelector = mkSelector "setNetworkName:"

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @extendedAddress@
extendedAddressSelector :: Selector
extendedAddressSelector = mkSelector "extendedAddress"

-- | @Selector@ for @setExtendedAddress:@
setExtendedAddressSelector :: Selector
setExtendedAddressSelector = mkSelector "setExtendedAddress:"

-- | @Selector@ for @rssi@
rssiSelector :: Selector
rssiSelector = mkSelector "rssi"

-- | @Selector@ for @setRssi:@
setRssiSelector :: Selector
setRssiSelector = mkSelector "setRssi:"

-- | @Selector@ for @lqi@
lqiSelector :: Selector
lqiSelector = mkSelector "lqi"

-- | @Selector@ for @setLqi:@
setLqiSelector :: Selector
setLqiSelector = mkSelector "setLqi:"

