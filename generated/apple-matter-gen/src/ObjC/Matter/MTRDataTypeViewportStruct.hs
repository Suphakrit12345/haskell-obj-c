{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeViewportStruct@.
module ObjC.Matter.MTRDataTypeViewportStruct
  ( MTRDataTypeViewportStruct
  , IsMTRDataTypeViewportStruct(..)
  , x1
  , setX1
  , y1
  , setY1
  , x2
  , setX2
  , y2
  , setY2
  , x1Selector
  , setX1Selector
  , y1Selector
  , setY1Selector
  , x2Selector
  , setX2Selector
  , y2Selector
  , setY2Selector


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

-- | @- x1@
x1 :: IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct => mtrDataTypeViewportStruct -> IO (Id NSNumber)
x1 mtrDataTypeViewportStruct  =
    sendMsg mtrDataTypeViewportStruct (mkSelector "x1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setX1:@
setX1 :: (IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct, IsNSNumber value) => mtrDataTypeViewportStruct -> value -> IO ()
setX1 mtrDataTypeViewportStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeViewportStruct (mkSelector "setX1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- y1@
y1 :: IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct => mtrDataTypeViewportStruct -> IO (Id NSNumber)
y1 mtrDataTypeViewportStruct  =
    sendMsg mtrDataTypeViewportStruct (mkSelector "y1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setY1:@
setY1 :: (IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct, IsNSNumber value) => mtrDataTypeViewportStruct -> value -> IO ()
setY1 mtrDataTypeViewportStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeViewportStruct (mkSelector "setY1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- x2@
x2 :: IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct => mtrDataTypeViewportStruct -> IO (Id NSNumber)
x2 mtrDataTypeViewportStruct  =
    sendMsg mtrDataTypeViewportStruct (mkSelector "x2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setX2:@
setX2 :: (IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct, IsNSNumber value) => mtrDataTypeViewportStruct -> value -> IO ()
setX2 mtrDataTypeViewportStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeViewportStruct (mkSelector "setX2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- y2@
y2 :: IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct => mtrDataTypeViewportStruct -> IO (Id NSNumber)
y2 mtrDataTypeViewportStruct  =
    sendMsg mtrDataTypeViewportStruct (mkSelector "y2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setY2:@
setY2 :: (IsMTRDataTypeViewportStruct mtrDataTypeViewportStruct, IsNSNumber value) => mtrDataTypeViewportStruct -> value -> IO ()
setY2 mtrDataTypeViewportStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeViewportStruct (mkSelector "setY2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @x1@
x1Selector :: Selector
x1Selector = mkSelector "x1"

-- | @Selector@ for @setX1:@
setX1Selector :: Selector
setX1Selector = mkSelector "setX1:"

-- | @Selector@ for @y1@
y1Selector :: Selector
y1Selector = mkSelector "y1"

-- | @Selector@ for @setY1:@
setY1Selector :: Selector
setY1Selector = mkSelector "setY1:"

-- | @Selector@ for @x2@
x2Selector :: Selector
x2Selector = mkSelector "x2"

-- | @Selector@ for @setX2:@
setX2Selector :: Selector
setX2Selector = mkSelector "setX2:"

-- | @Selector@ for @y2@
y2Selector :: Selector
y2Selector = mkSelector "y2"

-- | @Selector@ for @setY2:@
setY2Selector :: Selector
setY2Selector = mkSelector "setY2:"

