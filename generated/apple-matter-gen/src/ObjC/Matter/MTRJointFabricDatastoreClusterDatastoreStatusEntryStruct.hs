{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct
  ( MTRJointFabricDatastoreClusterDatastoreStatusEntryStruct
  , IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct(..)
  , state
  , setState
  , updateTimestamp
  , setUpdateTimestamp
  , failureCode
  , setFailureCode
  , stateSelector
  , setStateSelector
  , updateTimestampSelector
  , setUpdateTimestampSelector
  , failureCodeSelector
  , setFailureCodeSelector


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

-- | @- state@
state :: IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> IO (Id NSNumber)
state mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> value -> IO ()
setState mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- updateTimestamp@
updateTimestamp :: IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> IO (Id NSNumber)
updateTimestamp mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct (mkSelector "updateTimestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUpdateTimestamp:@
setUpdateTimestamp :: (IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> value -> IO ()
setUpdateTimestamp mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct (mkSelector "setUpdateTimestamp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- failureCode@
failureCode :: IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> IO (Id NSNumber)
failureCode mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct  =
    sendMsg mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct (mkSelector "failureCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFailureCode:@
setFailureCode :: (IsMTRJointFabricDatastoreClusterDatastoreStatusEntryStruct mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct -> value -> IO ()
setFailureCode mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrJointFabricDatastoreClusterDatastoreStatusEntryStruct (mkSelector "setFailureCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @updateTimestamp@
updateTimestampSelector :: Selector
updateTimestampSelector = mkSelector "updateTimestamp"

-- | @Selector@ for @setUpdateTimestamp:@
setUpdateTimestampSelector :: Selector
setUpdateTimestampSelector = mkSelector "setUpdateTimestamp:"

-- | @Selector@ for @failureCode@
failureCodeSelector :: Selector
failureCodeSelector = mkSelector "failureCode"

-- | @Selector@ for @setFailureCode:@
setFailureCodeSelector :: Selector
setFailureCodeSelector = mkSelector "setFailureCode:"

