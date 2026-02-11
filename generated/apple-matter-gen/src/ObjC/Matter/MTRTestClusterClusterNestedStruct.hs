{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterNestedStruct@.
module ObjC.Matter.MTRTestClusterClusterNestedStruct
  ( MTRTestClusterClusterNestedStruct
  , IsMTRTestClusterClusterNestedStruct(..)
  , a
  , setA
  , b
  , setB
  , c
  , setC
  , aSelector
  , setASelector
  , bSelector
  , setBSelector
  , cSelector
  , setCSelector


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

-- | @- a@
a :: IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct => mtrTestClusterClusterNestedStruct -> IO (Id NSNumber)
a mtrTestClusterClusterNestedStruct  =
    sendMsg mtrTestClusterClusterNestedStruct (mkSelector "a") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setA:@
setA :: (IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct, IsNSNumber value) => mtrTestClusterClusterNestedStruct -> value -> IO ()
setA mtrTestClusterClusterNestedStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStruct (mkSelector "setA:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- b@
b :: IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct => mtrTestClusterClusterNestedStruct -> IO (Id NSNumber)
b mtrTestClusterClusterNestedStruct  =
    sendMsg mtrTestClusterClusterNestedStruct (mkSelector "b") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setB:@
setB :: (IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct, IsNSNumber value) => mtrTestClusterClusterNestedStruct -> value -> IO ()
setB mtrTestClusterClusterNestedStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStruct (mkSelector "setB:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- c@
c :: IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct => mtrTestClusterClusterNestedStruct -> IO (Id MTRTestClusterClusterSimpleStruct)
c mtrTestClusterClusterNestedStruct  =
    sendMsg mtrTestClusterClusterNestedStruct (mkSelector "c") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setC:@
setC :: (IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNestedStruct -> value -> IO ()
setC mtrTestClusterClusterNestedStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStruct (mkSelector "setC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @a@
aSelector :: Selector
aSelector = mkSelector "a"

-- | @Selector@ for @setA:@
setASelector :: Selector
setASelector = mkSelector "setA:"

-- | @Selector@ for @b@
bSelector :: Selector
bSelector = mkSelector "b"

-- | @Selector@ for @setB:@
setBSelector :: Selector
setBSelector = mkSelector "setB:"

-- | @Selector@ for @c@
cSelector :: Selector
cSelector = mkSelector "c"

-- | @Selector@ for @setC:@
setCSelector :: Selector
setCSelector = mkSelector "setC:"

