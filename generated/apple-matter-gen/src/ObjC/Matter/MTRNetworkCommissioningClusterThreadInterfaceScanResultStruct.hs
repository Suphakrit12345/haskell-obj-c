{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterThreadInterfaceScanResultStruct@.
module ObjC.Matter.MTRNetworkCommissioningClusterThreadInterfaceScanResultStruct
  ( MTRNetworkCommissioningClusterThreadInterfaceScanResultStruct
  , IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct(..)
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
panId :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
panId mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "panId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPanId:@
setPanId :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setPanId mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "setPanId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extendedPanId@
extendedPanId :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
extendedPanId mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "extendedPanId") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtendedPanId:@
setExtendedPanId :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setExtendedPanId mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "setExtendedPanId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- networkName@
networkName :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSString)
networkName mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "networkName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkName:@
setNetworkName :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSString value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setNetworkName mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "setNetworkName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- channel@
channel :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
channel mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "channel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChannel:@
setChannel :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setChannel mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "setChannel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- version@
version :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
version mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVersion:@
setVersion :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setVersion mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "setVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- extendedAddress@
extendedAddress :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSData)
extendedAddress mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "extendedAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExtendedAddress:@
setExtendedAddress :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSData value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setExtendedAddress mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "setExtendedAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rssi@
rssi :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
rssi mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "rssi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRssi:@
setRssi :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setRssi mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "setRssi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lqi@
lqi :: IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> IO (Id NSNumber)
lqi mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  =
    sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "lqi") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLqi:@
setLqi :: (IsMTRNetworkCommissioningClusterThreadInterfaceScanResultStruct mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct, IsNSNumber value) => mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct -> value -> IO ()
setLqi mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterThreadInterfaceScanResultStruct (mkSelector "setLqi:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

