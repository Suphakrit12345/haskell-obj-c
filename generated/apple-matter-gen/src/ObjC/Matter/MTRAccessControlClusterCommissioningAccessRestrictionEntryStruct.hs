{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterCommissioningAccessRestrictionEntryStruct@.
module ObjC.Matter.MTRAccessControlClusterCommissioningAccessRestrictionEntryStruct
  ( MTRAccessControlClusterCommissioningAccessRestrictionEntryStruct
  , IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct(..)
  , endpoint
  , setEndpoint
  , cluster
  , setCluster
  , restrictions
  , setRestrictions
  , endpointSelector
  , setEndpointSelector
  , clusterSelector
  , setClusterSelector
  , restrictionsSelector
  , setRestrictionsSelector


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

-- | @- endpoint@
endpoint :: IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> IO (Id NSNumber)
endpoint mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct  =
    sendMsg mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> value -> IO ()
setEndpoint mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cluster@
cluster :: IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> IO (Id NSNumber)
cluster mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct  =
    sendMsg mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCluster:@
setCluster :: (IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> value -> IO ()
setCluster mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct (mkSelector "setCluster:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- restrictions@
restrictions :: IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> IO (Id NSArray)
restrictions mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct  =
    sendMsg mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct (mkSelector "restrictions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRestrictions:@
setRestrictions :: (IsMTRAccessControlClusterCommissioningAccessRestrictionEntryStruct mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct, IsNSArray value) => mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct -> value -> IO ()
setRestrictions mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterCommissioningAccessRestrictionEntryStruct (mkSelector "setRestrictions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

-- | @Selector@ for @cluster@
clusterSelector :: Selector
clusterSelector = mkSelector "cluster"

-- | @Selector@ for @setCluster:@
setClusterSelector :: Selector
setClusterSelector = mkSelector "setCluster:"

-- | @Selector@ for @restrictions@
restrictionsSelector :: Selector
restrictionsSelector = mkSelector "restrictions"

-- | @Selector@ for @setRestrictions:@
setRestrictionsSelector :: Selector
setRestrictionsSelector = mkSelector "setRestrictions:"

