{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActionsClusterActionStruct@.
module ObjC.Matter.MTRActionsClusterActionStruct
  ( MTRActionsClusterActionStruct
  , IsMTRActionsClusterActionStruct(..)
  , actionID
  , setActionID
  , name
  , setName
  , type_
  , setType
  , endpointListID
  , setEndpointListID
  , supportedCommands
  , setSupportedCommands
  , state
  , setState
  , actionIDSelector
  , setActionIDSelector
  , nameSelector
  , setNameSelector
  , typeSelector
  , setTypeSelector
  , endpointListIDSelector
  , setEndpointListIDSelector
  , supportedCommandsSelector
  , setSupportedCommandsSelector
  , stateSelector
  , setStateSelector


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

-- | @- actionID@
actionID :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
actionID mtrActionsClusterActionStruct  =
    sendMsg mtrActionsClusterActionStruct (mkSelector "actionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setActionID:@
setActionID :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setActionID mtrActionsClusterActionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionStruct (mkSelector "setActionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSString)
name mtrActionsClusterActionStruct  =
    sendMsg mtrActionsClusterActionStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSString value) => mtrActionsClusterActionStruct -> value -> IO ()
setName mtrActionsClusterActionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
type_ mtrActionsClusterActionStruct  =
    sendMsg mtrActionsClusterActionStruct (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setType mtrActionsClusterActionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionStruct (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpointListID@
endpointListID :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
endpointListID mtrActionsClusterActionStruct  =
    sendMsg mtrActionsClusterActionStruct (mkSelector "endpointListID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpointListID:@
setEndpointListID :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setEndpointListID mtrActionsClusterActionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionStruct (mkSelector "setEndpointListID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedCommands@
supportedCommands :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
supportedCommands mtrActionsClusterActionStruct  =
    sendMsg mtrActionsClusterActionStruct (mkSelector "supportedCommands") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupportedCommands:@
setSupportedCommands :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setSupportedCommands mtrActionsClusterActionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionStruct (mkSelector "setSupportedCommands:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct => mtrActionsClusterActionStruct -> IO (Id NSNumber)
state mtrActionsClusterActionStruct  =
    sendMsg mtrActionsClusterActionStruct (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsMTRActionsClusterActionStruct mtrActionsClusterActionStruct, IsNSNumber value) => mtrActionsClusterActionStruct -> value -> IO ()
setState mtrActionsClusterActionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActionsClusterActionStruct (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @actionID@
actionIDSelector :: Selector
actionIDSelector = mkSelector "actionID"

-- | @Selector@ for @setActionID:@
setActionIDSelector :: Selector
setActionIDSelector = mkSelector "setActionID:"

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

-- | @Selector@ for @endpointListID@
endpointListIDSelector :: Selector
endpointListIDSelector = mkSelector "endpointListID"

-- | @Selector@ for @setEndpointListID:@
setEndpointListIDSelector :: Selector
setEndpointListIDSelector = mkSelector "setEndpointListID:"

-- | @Selector@ for @supportedCommands@
supportedCommandsSelector :: Selector
supportedCommandsSelector = mkSelector "supportedCommands"

-- | @Selector@ for @setSupportedCommands:@
setSupportedCommandsSelector :: Selector
setSupportedCommandsSelector = mkSelector "setSupportedCommands:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

