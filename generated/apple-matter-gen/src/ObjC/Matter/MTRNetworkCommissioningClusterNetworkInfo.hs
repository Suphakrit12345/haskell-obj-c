{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterNetworkInfo@.
module ObjC.Matter.MTRNetworkCommissioningClusterNetworkInfo
  ( MTRNetworkCommissioningClusterNetworkInfo
  , IsMTRNetworkCommissioningClusterNetworkInfo(..)
  , networkID
  , setNetworkID
  , connected
  , setConnected
  , networkIDSelector
  , setNetworkIDSelector
  , connectedSelector
  , setConnectedSelector


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
networkID :: IsMTRNetworkCommissioningClusterNetworkInfo mtrNetworkCommissioningClusterNetworkInfo => mtrNetworkCommissioningClusterNetworkInfo -> IO (Id NSData)
networkID mtrNetworkCommissioningClusterNetworkInfo  =
    sendMsg mtrNetworkCommissioningClusterNetworkInfo (mkSelector "networkID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNetworkID:@
setNetworkID :: (IsMTRNetworkCommissioningClusterNetworkInfo mtrNetworkCommissioningClusterNetworkInfo, IsNSData value) => mtrNetworkCommissioningClusterNetworkInfo -> value -> IO ()
setNetworkID mtrNetworkCommissioningClusterNetworkInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterNetworkInfo (mkSelector "setNetworkID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- connected@
connected :: IsMTRNetworkCommissioningClusterNetworkInfo mtrNetworkCommissioningClusterNetworkInfo => mtrNetworkCommissioningClusterNetworkInfo -> IO (Id NSNumber)
connected mtrNetworkCommissioningClusterNetworkInfo  =
    sendMsg mtrNetworkCommissioningClusterNetworkInfo (mkSelector "connected") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConnected:@
setConnected :: (IsMTRNetworkCommissioningClusterNetworkInfo mtrNetworkCommissioningClusterNetworkInfo, IsNSNumber value) => mtrNetworkCommissioningClusterNetworkInfo -> value -> IO ()
setConnected mtrNetworkCommissioningClusterNetworkInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrNetworkCommissioningClusterNetworkInfo (mkSelector "setConnected:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

