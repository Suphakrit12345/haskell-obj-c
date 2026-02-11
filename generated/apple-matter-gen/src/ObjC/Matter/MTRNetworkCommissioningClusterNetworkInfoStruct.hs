{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterNetworkInfoStruct@.
module ObjC.Matter.MTRNetworkCommissioningClusterNetworkInfoStruct
  ( MTRNetworkCommissioningClusterNetworkInfoStruct
  , IsMTRNetworkCommissioningClusterNetworkInfoStruct(..)
  , networkID
  , setNetworkID
  , connected
  , setConnected
  , networkIdentifier
  , setNetworkIdentifier
  , clientIdentifier
  , setClientIdentifier
  , networkIDSelector
  , setNetworkIDSelector
  , connectedSelector
  , setConnectedSelector
  , networkIdentifierSelector
  , setNetworkIdentifierSelector
  , clientIdentifierSelector
  , setClientIdentifierSelector


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

-- | @- networkID@
networkID :: IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct => mtrNetworkCommissioningClusterNetworkInfoStruct -> IO (Id NSData)
networkID mtrNetworkCommissioningClusterNetworkInfoStruct  =
    sendMsg mtrNetworkCommissioningClusterNetworkInfoStruct (mkSelector "networkID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkID:@
setNetworkID :: (IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct, IsNSData value) => mtrNetworkCommissioningClusterNetworkInfoStruct -> value -> IO ()
setNetworkID mtrNetworkCommissioningClusterNetworkInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterNetworkInfoStruct (mkSelector "setNetworkID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- connected@
connected :: IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct => mtrNetworkCommissioningClusterNetworkInfoStruct -> IO (Id NSNumber)
connected mtrNetworkCommissioningClusterNetworkInfoStruct  =
    sendMsg mtrNetworkCommissioningClusterNetworkInfoStruct (mkSelector "connected") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnected:@
setConnected :: (IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct, IsNSNumber value) => mtrNetworkCommissioningClusterNetworkInfoStruct -> value -> IO ()
setConnected mtrNetworkCommissioningClusterNetworkInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterNetworkInfoStruct (mkSelector "setConnected:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- networkIdentifier@
networkIdentifier :: IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct => mtrNetworkCommissioningClusterNetworkInfoStruct -> IO (Id NSData)
networkIdentifier mtrNetworkCommissioningClusterNetworkInfoStruct  =
    sendMsg mtrNetworkCommissioningClusterNetworkInfoStruct (mkSelector "networkIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkIdentifier:@
setNetworkIdentifier :: (IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct, IsNSData value) => mtrNetworkCommissioningClusterNetworkInfoStruct -> value -> IO ()
setNetworkIdentifier mtrNetworkCommissioningClusterNetworkInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterNetworkInfoStruct (mkSelector "setNetworkIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- clientIdentifier@
clientIdentifier :: IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct => mtrNetworkCommissioningClusterNetworkInfoStruct -> IO (Id NSData)
clientIdentifier mtrNetworkCommissioningClusterNetworkInfoStruct  =
    sendMsg mtrNetworkCommissioningClusterNetworkInfoStruct (mkSelector "clientIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClientIdentifier:@
setClientIdentifier :: (IsMTRNetworkCommissioningClusterNetworkInfoStruct mtrNetworkCommissioningClusterNetworkInfoStruct, IsNSData value) => mtrNetworkCommissioningClusterNetworkInfoStruct -> value -> IO ()
setClientIdentifier mtrNetworkCommissioningClusterNetworkInfoStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterNetworkInfoStruct (mkSelector "setClientIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkID@
networkIDSelector :: Selector
networkIDSelector = mkSelector "networkID"

-- | @Selector@ for @setNetworkID:@
setNetworkIDSelector :: Selector
setNetworkIDSelector = mkSelector "setNetworkID:"

-- | @Selector@ for @connected@
connectedSelector :: Selector
connectedSelector = mkSelector "connected"

-- | @Selector@ for @setConnected:@
setConnectedSelector :: Selector
setConnectedSelector = mkSelector "setConnected:"

-- | @Selector@ for @networkIdentifier@
networkIdentifierSelector :: Selector
networkIdentifierSelector = mkSelector "networkIdentifier"

-- | @Selector@ for @setNetworkIdentifier:@
setNetworkIdentifierSelector :: Selector
setNetworkIdentifierSelector = mkSelector "setNetworkIdentifier:"

-- | @Selector@ for @clientIdentifier@
clientIdentifierSelector :: Selector
clientIdentifierSelector = mkSelector "clientIdentifier"

-- | @Selector@ for @setClientIdentifier:@
setClientIdentifierSelector :: Selector
setClientIdentifierSelector = mkSelector "setClientIdentifier:"

