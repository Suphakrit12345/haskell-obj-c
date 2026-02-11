{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessRestrictionEntryStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessRestrictionEntryStruct
  ( MTRAccessControlClusterAccessRestrictionEntryStruct
  , IsMTRAccessControlClusterAccessRestrictionEntryStruct(..)
  , endpoint
  , setEndpoint
  , cluster
  , setCluster
  , restrictions
  , setRestrictions
  , fabricIndex
  , setFabricIndex
  , endpointSelector
  , setEndpointSelector
  , clusterSelector
  , setClusterSelector
  , restrictionsSelector
  , setRestrictionsSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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
endpoint :: IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct => mtrAccessControlClusterAccessRestrictionEntryStruct -> IO (Id NSNumber)
endpoint mtrAccessControlClusterAccessRestrictionEntryStruct  =
    sendMsg mtrAccessControlClusterAccessRestrictionEntryStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionEntryStruct -> value -> IO ()
setEndpoint mtrAccessControlClusterAccessRestrictionEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessRestrictionEntryStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cluster@
cluster :: IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct => mtrAccessControlClusterAccessRestrictionEntryStruct -> IO (Id NSNumber)
cluster mtrAccessControlClusterAccessRestrictionEntryStruct  =
    sendMsg mtrAccessControlClusterAccessRestrictionEntryStruct (mkSelector "cluster") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCluster:@
setCluster :: (IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionEntryStruct -> value -> IO ()
setCluster mtrAccessControlClusterAccessRestrictionEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessRestrictionEntryStruct (mkSelector "setCluster:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- restrictions@
restrictions :: IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct => mtrAccessControlClusterAccessRestrictionEntryStruct -> IO (Id NSArray)
restrictions mtrAccessControlClusterAccessRestrictionEntryStruct  =
    sendMsg mtrAccessControlClusterAccessRestrictionEntryStruct (mkSelector "restrictions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRestrictions:@
setRestrictions :: (IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct, IsNSArray value) => mtrAccessControlClusterAccessRestrictionEntryStruct -> value -> IO ()
setRestrictions mtrAccessControlClusterAccessRestrictionEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessRestrictionEntryStruct (mkSelector "setRestrictions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct => mtrAccessControlClusterAccessRestrictionEntryStruct -> IO (Id NSNumber)
fabricIndex mtrAccessControlClusterAccessRestrictionEntryStruct  =
    sendMsg mtrAccessControlClusterAccessRestrictionEntryStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRAccessControlClusterAccessRestrictionEntryStruct mtrAccessControlClusterAccessRestrictionEntryStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionEntryStruct -> value -> IO ()
setFabricIndex mtrAccessControlClusterAccessRestrictionEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrAccessControlClusterAccessRestrictionEntryStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

