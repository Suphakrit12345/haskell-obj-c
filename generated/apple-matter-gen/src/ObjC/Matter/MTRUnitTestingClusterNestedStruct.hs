{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterNestedStruct@.
module ObjC.Matter.MTRUnitTestingClusterNestedStruct
  ( MTRUnitTestingClusterNestedStruct
  , IsMTRUnitTestingClusterNestedStruct(..)
  , a
  , setA
  , b
  , setB
  , c
  , setC
  , d
  , setD
  , aSelector
  , setASelector
  , bSelector
  , setBSelector
  , cSelector
  , setCSelector
  , dSelector
  , setDSelector


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
a :: IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct => mtrUnitTestingClusterNestedStruct -> IO (Id NSNumber)
a mtrUnitTestingClusterNestedStruct  =
    sendMsg mtrUnitTestingClusterNestedStruct (mkSelector "a") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setA:@
setA :: (IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct, IsNSNumber value) => mtrUnitTestingClusterNestedStruct -> value -> IO ()
setA mtrUnitTestingClusterNestedStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStruct (mkSelector "setA:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- b@
b :: IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct => mtrUnitTestingClusterNestedStruct -> IO (Id NSNumber)
b mtrUnitTestingClusterNestedStruct  =
    sendMsg mtrUnitTestingClusterNestedStruct (mkSelector "b") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setB:@
setB :: (IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct, IsNSNumber value) => mtrUnitTestingClusterNestedStruct -> value -> IO ()
setB mtrUnitTestingClusterNestedStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStruct (mkSelector "setB:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- c@
c :: IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct => mtrUnitTestingClusterNestedStruct -> IO (Id MTRUnitTestingClusterSimpleStruct)
c mtrUnitTestingClusterNestedStruct  =
    sendMsg mtrUnitTestingClusterNestedStruct (mkSelector "c") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setC:@
setC :: (IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNestedStruct -> value -> IO ()
setC mtrUnitTestingClusterNestedStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStruct (mkSelector "setC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- d@
d :: IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct => mtrUnitTestingClusterNestedStruct -> IO (Id MTRDataTypeTestGlobalStruct)
d mtrUnitTestingClusterNestedStruct  =
    sendMsg mtrUnitTestingClusterNestedStruct (mkSelector "d") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setD:@
setD :: (IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct, IsMTRDataTypeTestGlobalStruct value) => mtrUnitTestingClusterNestedStruct -> value -> IO ()
setD mtrUnitTestingClusterNestedStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStruct (mkSelector "setD:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @d@
dSelector :: Selector
dSelector = mkSelector "d"

-- | @Selector@ for @setD:@
setDSelector :: Selector
setDSelector = mkSelector "setD:"

