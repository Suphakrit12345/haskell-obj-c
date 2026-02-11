{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterNetworkInterface@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterNetworkInterface
  ( MTRGeneralDiagnosticsClusterNetworkInterface
  , IsMTRGeneralDiagnosticsClusterNetworkInterface(..)
  , name
  , setName
  , isOperational
  , setIsOperational
  , offPremiseServicesReachableIPv4
  , setOffPremiseServicesReachableIPv4
  , offPremiseServicesReachableIPv6
  , setOffPremiseServicesReachableIPv6
  , hardwareAddress
  , setHardwareAddress
  , iPv4Addresses
  , setIPv4Addresses
  , iPv6Addresses
  , setIPv6Addresses
  , type_
  , setType
  , nameSelector
  , setNameSelector
  , isOperationalSelector
  , setIsOperationalSelector
  , offPremiseServicesReachableIPv4Selector
  , setOffPremiseServicesReachableIPv4Selector
  , offPremiseServicesReachableIPv6Selector
  , setOffPremiseServicesReachableIPv6Selector
  , hardwareAddressSelector
  , setHardwareAddressSelector
  , iPv4AddressesSelector
  , setIPv4AddressesSelector
  , iPv6AddressesSelector
  , setIPv6AddressesSelector
  , typeSelector
  , setTypeSelector


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

-- | @- name@
name :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSString)
name mtrGeneralDiagnosticsClusterNetworkInterface  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSString value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setName mtrGeneralDiagnosticsClusterNetworkInterface  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isOperational@
isOperational :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSNumber)
isOperational mtrGeneralDiagnosticsClusterNetworkInterface  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "isOperational") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIsOperational:@
setIsOperational :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setIsOperational mtrGeneralDiagnosticsClusterNetworkInterface  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "setIsOperational:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- offPremiseServicesReachableIPv4@
offPremiseServicesReachableIPv4 :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSNumber)
offPremiseServicesReachableIPv4 mtrGeneralDiagnosticsClusterNetworkInterface  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "offPremiseServicesReachableIPv4") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffPremiseServicesReachableIPv4:@
setOffPremiseServicesReachableIPv4 :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setOffPremiseServicesReachableIPv4 mtrGeneralDiagnosticsClusterNetworkInterface  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "setOffPremiseServicesReachableIPv4:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- offPremiseServicesReachableIPv6@
offPremiseServicesReachableIPv6 :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSNumber)
offPremiseServicesReachableIPv6 mtrGeneralDiagnosticsClusterNetworkInterface  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "offPremiseServicesReachableIPv6") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffPremiseServicesReachableIPv6:@
setOffPremiseServicesReachableIPv6 :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setOffPremiseServicesReachableIPv6 mtrGeneralDiagnosticsClusterNetworkInterface  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "setOffPremiseServicesReachableIPv6:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hardwareAddress@
hardwareAddress :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSData)
hardwareAddress mtrGeneralDiagnosticsClusterNetworkInterface  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "hardwareAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHardwareAddress:@
setHardwareAddress :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSData value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setHardwareAddress mtrGeneralDiagnosticsClusterNetworkInterface  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "setHardwareAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iPv4Addresses@
iPv4Addresses :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSArray)
iPv4Addresses mtrGeneralDiagnosticsClusterNetworkInterface  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "iPv4Addresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIPv4Addresses:@
setIPv4Addresses :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setIPv4Addresses mtrGeneralDiagnosticsClusterNetworkInterface  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "setIPv4Addresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iPv6Addresses@
iPv6Addresses :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSArray)
iPv6Addresses mtrGeneralDiagnosticsClusterNetworkInterface  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "iPv6Addresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIPv6Addresses:@
setIPv6Addresses :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSArray value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setIPv6Addresses mtrGeneralDiagnosticsClusterNetworkInterface  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "setIPv6Addresses:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface => mtrGeneralDiagnosticsClusterNetworkInterface -> IO (Id NSNumber)
type_ mtrGeneralDiagnosticsClusterNetworkInterface  =
    sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsMTRGeneralDiagnosticsClusterNetworkInterface mtrGeneralDiagnosticsClusterNetworkInterface, IsNSNumber value) => mtrGeneralDiagnosticsClusterNetworkInterface -> value -> IO ()
setType mtrGeneralDiagnosticsClusterNetworkInterface  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrGeneralDiagnosticsClusterNetworkInterface (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @isOperational@
isOperationalSelector :: Selector
isOperationalSelector = mkSelector "isOperational"

-- | @Selector@ for @setIsOperational:@
setIsOperationalSelector :: Selector
setIsOperationalSelector = mkSelector "setIsOperational:"

-- | @Selector@ for @offPremiseServicesReachableIPv4@
offPremiseServicesReachableIPv4Selector :: Selector
offPremiseServicesReachableIPv4Selector = mkSelector "offPremiseServicesReachableIPv4"

-- | @Selector@ for @setOffPremiseServicesReachableIPv4:@
setOffPremiseServicesReachableIPv4Selector :: Selector
setOffPremiseServicesReachableIPv4Selector = mkSelector "setOffPremiseServicesReachableIPv4:"

-- | @Selector@ for @offPremiseServicesReachableIPv6@
offPremiseServicesReachableIPv6Selector :: Selector
offPremiseServicesReachableIPv6Selector = mkSelector "offPremiseServicesReachableIPv6"

-- | @Selector@ for @setOffPremiseServicesReachableIPv6:@
setOffPremiseServicesReachableIPv6Selector :: Selector
setOffPremiseServicesReachableIPv6Selector = mkSelector "setOffPremiseServicesReachableIPv6:"

-- | @Selector@ for @hardwareAddress@
hardwareAddressSelector :: Selector
hardwareAddressSelector = mkSelector "hardwareAddress"

-- | @Selector@ for @setHardwareAddress:@
setHardwareAddressSelector :: Selector
setHardwareAddressSelector = mkSelector "setHardwareAddress:"

-- | @Selector@ for @iPv4Addresses@
iPv4AddressesSelector :: Selector
iPv4AddressesSelector = mkSelector "iPv4Addresses"

-- | @Selector@ for @setIPv4Addresses:@
setIPv4AddressesSelector :: Selector
setIPv4AddressesSelector = mkSelector "setIPv4Addresses:"

-- | @Selector@ for @iPv6Addresses@
iPv6AddressesSelector :: Selector
iPv6AddressesSelector = mkSelector "iPv6Addresses"

-- | @Selector@ for @setIPv6Addresses:@
setIPv6AddressesSelector :: Selector
setIPv6AddressesSelector = mkSelector "setIPv6Addresses:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

