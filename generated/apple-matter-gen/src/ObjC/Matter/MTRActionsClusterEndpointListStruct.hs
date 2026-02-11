{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterEndpointListStruct@.
module ObjC.Matter.MTRActionsClusterEndpointListStruct
  ( MTRActionsClusterEndpointListStruct
  , IsMTRActionsClusterEndpointListStruct(..)
  , endpointListID
  , setEndpointListID
  , name
  , setName
  , type_
  , setType
  , endpoints
  , setEndpoints
  , endpointListIDSelector
  , setEndpointListIDSelector
  , nameSelector
  , setNameSelector
  , typeSelector
  , setTypeSelector
  , endpointsSelector
  , setEndpointsSelector


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

-- | @- endpointListID@
endpointListID :: IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct => mtrActionsClusterEndpointListStruct -> IO (Id NSNumber)
endpointListID mtrActionsClusterEndpointListStruct  =
    sendMsg mtrActionsClusterEndpointListStruct (mkSelector "endpointListID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointListID:@
setEndpointListID :: (IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct, IsNSNumber value) => mtrActionsClusterEndpointListStruct -> value -> IO ()
setEndpointListID mtrActionsClusterEndpointListStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterEndpointListStruct (mkSelector "setEndpointListID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct => mtrActionsClusterEndpointListStruct -> IO (Id NSString)
name mtrActionsClusterEndpointListStruct  =
    sendMsg mtrActionsClusterEndpointListStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct, IsNSString value) => mtrActionsClusterEndpointListStruct -> value -> IO ()
setName mtrActionsClusterEndpointListStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterEndpointListStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct => mtrActionsClusterEndpointListStruct -> IO (Id NSNumber)
type_ mtrActionsClusterEndpointListStruct  =
    sendMsg mtrActionsClusterEndpointListStruct (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct, IsNSNumber value) => mtrActionsClusterEndpointListStruct -> value -> IO ()
setType mtrActionsClusterEndpointListStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterEndpointListStruct (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoints@
endpoints :: IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct => mtrActionsClusterEndpointListStruct -> IO (Id NSArray)
endpoints mtrActionsClusterEndpointListStruct  =
    sendMsg mtrActionsClusterEndpointListStruct (mkSelector "endpoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoints:@
setEndpoints :: (IsMTRActionsClusterEndpointListStruct mtrActionsClusterEndpointListStruct, IsNSArray value) => mtrActionsClusterEndpointListStruct -> value -> IO ()
setEndpoints mtrActionsClusterEndpointListStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterEndpointListStruct (mkSelector "setEndpoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @endpointListID@
endpointListIDSelector :: Selector
endpointListIDSelector = mkSelector "endpointListID"

-- | @Selector@ for @setEndpointListID:@
setEndpointListIDSelector :: Selector
setEndpointListIDSelector = mkSelector "setEndpointListID:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector
setEndpointsSelector = mkSelector "setEndpoints:"

