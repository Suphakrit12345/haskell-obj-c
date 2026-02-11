{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestEventEvent@.
module ObjC.Matter.MTRTestClusterClusterTestEventEvent
  ( MTRTestClusterClusterTestEventEvent
  , IsMTRTestClusterClusterTestEventEvent(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , arg3
  , setArg3
  , arg4
  , setArg4
  , arg5
  , setArg5
  , arg6
  , setArg6
  , arg1Selector
  , setArg1Selector
  , arg2Selector
  , setArg2Selector
  , arg3Selector
  , setArg3Selector
  , arg4Selector
  , setArg4Selector
  , arg5Selector
  , setArg5Selector
  , arg6Selector
  , setArg6Selector


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

-- | @- arg1@
arg1 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSNumber)
arg1 mtrTestClusterClusterTestEventEvent  =
    sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "arg1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSNumber value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg1 mtrTestClusterClusterTestEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "setArg1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSNumber)
arg2 mtrTestClusterClusterTestEventEvent  =
    sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "arg2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSNumber value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg2 mtrTestClusterClusterTestEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "setArg2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg3@
arg3 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSNumber)
arg3 mtrTestClusterClusterTestEventEvent  =
    sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "arg3") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg3:@
setArg3 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSNumber value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg3 mtrTestClusterClusterTestEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "setArg3:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg4@
arg4 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id MTRTestClusterClusterSimpleStruct)
arg4 mtrTestClusterClusterTestEventEvent  =
    sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "arg4") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg4:@
setArg4 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg4 mtrTestClusterClusterTestEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "setArg4:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg5@
arg5 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSArray)
arg5 mtrTestClusterClusterTestEventEvent  =
    sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "arg5") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg5:@
setArg5 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSArray value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg5 mtrTestClusterClusterTestEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "setArg5:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- arg6@
arg6 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSArray)
arg6 mtrTestClusterClusterTestEventEvent  =
    sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "arg6") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArg6:@
setArg6 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSArray value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg6 mtrTestClusterClusterTestEventEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestEventEvent (mkSelector "setArg6:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @arg2@
arg2Selector :: Selector
arg2Selector = mkSelector "arg2"

-- | @Selector@ for @setArg2:@
setArg2Selector :: Selector
setArg2Selector = mkSelector "setArg2:"

-- | @Selector@ for @arg3@
arg3Selector :: Selector
arg3Selector = mkSelector "arg3"

-- | @Selector@ for @setArg3:@
setArg3Selector :: Selector
setArg3Selector = mkSelector "setArg3:"

-- | @Selector@ for @arg4@
arg4Selector :: Selector
arg4Selector = mkSelector "arg4"

-- | @Selector@ for @setArg4:@
setArg4Selector :: Selector
setArg4Selector = mkSelector "setArg4:"

-- | @Selector@ for @arg5@
arg5Selector :: Selector
arg5Selector = mkSelector "arg5"

-- | @Selector@ for @setArg5:@
setArg5Selector :: Selector
setArg5Selector = mkSelector "setArg5:"

-- | @Selector@ for @arg6@
arg6Selector :: Selector
arg6Selector = mkSelector "arg6"

-- | @Selector@ for @setArg6:@
setArg6Selector :: Selector
setArg6Selector = mkSelector "setArg6:"

