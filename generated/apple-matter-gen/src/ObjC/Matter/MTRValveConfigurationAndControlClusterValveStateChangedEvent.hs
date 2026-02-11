{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRValveConfigurationAndControlClusterValveStateChangedEvent@.
module ObjC.Matter.MTRValveConfigurationAndControlClusterValveStateChangedEvent
  ( MTRValveConfigurationAndControlClusterValveStateChangedEvent
  , IsMTRValveConfigurationAndControlClusterValveStateChangedEvent(..)
  , valveState
  , setValveState
  , valveLevel
  , setValveLevel
  , valveStateSelector
  , setValveStateSelector
  , valveLevelSelector
  , setValveLevelSelector


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

-- | @- valveState@
valveState :: IsMTRValveConfigurationAndControlClusterValveStateChangedEvent mtrValveConfigurationAndControlClusterValveStateChangedEvent => mtrValveConfigurationAndControlClusterValveStateChangedEvent -> IO (Id NSNumber)
valveState mtrValveConfigurationAndControlClusterValveStateChangedEvent  =
    sendMsg mtrValveConfigurationAndControlClusterValveStateChangedEvent (mkSelector "valveState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValveState:@
setValveState :: (IsMTRValveConfigurationAndControlClusterValveStateChangedEvent mtrValveConfigurationAndControlClusterValveStateChangedEvent, IsNSNumber value) => mtrValveConfigurationAndControlClusterValveStateChangedEvent -> value -> IO ()
setValveState mtrValveConfigurationAndControlClusterValveStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrValveConfigurationAndControlClusterValveStateChangedEvent (mkSelector "setValveState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valveLevel@
valveLevel :: IsMTRValveConfigurationAndControlClusterValveStateChangedEvent mtrValveConfigurationAndControlClusterValveStateChangedEvent => mtrValveConfigurationAndControlClusterValveStateChangedEvent -> IO (Id NSNumber)
valveLevel mtrValveConfigurationAndControlClusterValveStateChangedEvent  =
    sendMsg mtrValveConfigurationAndControlClusterValveStateChangedEvent (mkSelector "valveLevel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValveLevel:@
setValveLevel :: (IsMTRValveConfigurationAndControlClusterValveStateChangedEvent mtrValveConfigurationAndControlClusterValveStateChangedEvent, IsNSNumber value) => mtrValveConfigurationAndControlClusterValveStateChangedEvent -> value -> IO ()
setValveLevel mtrValveConfigurationAndControlClusterValveStateChangedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrValveConfigurationAndControlClusterValveStateChangedEvent (mkSelector "setValveLevel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valveState@
valveStateSelector :: Selector
valveStateSelector = mkSelector "valveState"

-- | @Selector@ for @setValveState:@
setValveStateSelector :: Selector
setValveStateSelector = mkSelector "setValveState:"

-- | @Selector@ for @valveLevel@
valveLevelSelector :: Selector
valveLevelSelector = mkSelector "valveLevel"

-- | @Selector@ for @setValveLevel:@
setValveLevelSelector :: Selector
setValveLevelSelector = mkSelector "setValveLevel:"

