{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSoftwareDiagnosticsClusterThreadMetrics@.
module ObjC.Matter.MTRSoftwareDiagnosticsClusterThreadMetrics
  ( MTRSoftwareDiagnosticsClusterThreadMetrics
  , IsMTRSoftwareDiagnosticsClusterThreadMetrics(..)
  , id_
  , setId
  , name
  , setName
  , stackFreeCurrent
  , setStackFreeCurrent
  , stackFreeMinimum
  , setStackFreeMinimum
  , stackSize
  , setStackSize
  , idSelector
  , setIdSelector
  , nameSelector
  , setNameSelector
  , stackFreeCurrentSelector
  , setStackFreeCurrentSelector
  , stackFreeMinimumSelector
  , setStackFreeMinimumSelector
  , stackSizeSelector
  , setStackSizeSelector


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

-- | @- id@
id_ :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSNumber)
id_ mtrSoftwareDiagnosticsClusterThreadMetrics  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "id") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setId:@
setId :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setId mtrSoftwareDiagnosticsClusterThreadMetrics  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "setId:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSString)
name mtrSoftwareDiagnosticsClusterThreadMetrics  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSString value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setName mtrSoftwareDiagnosticsClusterThreadMetrics  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stackFreeCurrent@
stackFreeCurrent :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSNumber)
stackFreeCurrent mtrSoftwareDiagnosticsClusterThreadMetrics  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "stackFreeCurrent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStackFreeCurrent:@
setStackFreeCurrent :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setStackFreeCurrent mtrSoftwareDiagnosticsClusterThreadMetrics  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "setStackFreeCurrent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stackFreeMinimum@
stackFreeMinimum :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSNumber)
stackFreeMinimum mtrSoftwareDiagnosticsClusterThreadMetrics  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "stackFreeMinimum") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStackFreeMinimum:@
setStackFreeMinimum :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setStackFreeMinimum mtrSoftwareDiagnosticsClusterThreadMetrics  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "setStackFreeMinimum:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stackSize@
stackSize :: IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics => mtrSoftwareDiagnosticsClusterThreadMetrics -> IO (Id NSNumber)
stackSize mtrSoftwareDiagnosticsClusterThreadMetrics  =
    sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "stackSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStackSize:@
setStackSize :: (IsMTRSoftwareDiagnosticsClusterThreadMetrics mtrSoftwareDiagnosticsClusterThreadMetrics, IsNSNumber value) => mtrSoftwareDiagnosticsClusterThreadMetrics -> value -> IO ()
setStackSize mtrSoftwareDiagnosticsClusterThreadMetrics  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrSoftwareDiagnosticsClusterThreadMetrics (mkSelector "setStackSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @stackFreeCurrent@
stackFreeCurrentSelector :: Selector
stackFreeCurrentSelector = mkSelector "stackFreeCurrent"

-- | @Selector@ for @setStackFreeCurrent:@
setStackFreeCurrentSelector :: Selector
setStackFreeCurrentSelector = mkSelector "setStackFreeCurrent:"

-- | @Selector@ for @stackFreeMinimum@
stackFreeMinimumSelector :: Selector
stackFreeMinimumSelector = mkSelector "stackFreeMinimum"

-- | @Selector@ for @setStackFreeMinimum:@
setStackFreeMinimumSelector :: Selector
setStackFreeMinimumSelector = mkSelector "setStackFreeMinimum:"

-- | @Selector@ for @stackSize@
stackSizeSelector :: Selector
stackSizeSelector = mkSelector "stackSize"

-- | @Selector@ for @setStackSize:@
setStackSizeSelector :: Selector
setStackSizeSelector = mkSelector "setStackSize:"

