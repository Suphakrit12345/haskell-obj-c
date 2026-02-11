{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterNestedStructList@.
module ObjC.Matter.MTRUnitTestingClusterNestedStructList
  ( MTRUnitTestingClusterNestedStructList
  , IsMTRUnitTestingClusterNestedStructList(..)
  , a
  , setA
  , b
  , setB
  , c
  , setC
  , d
  , setD
  , e
  , setE
  , f
  , setF
  , g
  , setG
  , aSelector
  , setASelector
  , bSelector
  , setBSelector
  , cSelector
  , setCSelector
  , dSelector
  , setDSelector
  , eSelector
  , setESelector
  , fSelector
  , setFSelector
  , gSelector
  , setGSelector


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
a :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSNumber)
a mtrUnitTestingClusterNestedStructList  =
    sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "a") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setA:@
setA :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSNumber value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setA mtrUnitTestingClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "setA:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- b@
b :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSNumber)
b mtrUnitTestingClusterNestedStructList  =
    sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "b") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setB:@
setB :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSNumber value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setB mtrUnitTestingClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "setB:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- c@
c :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id MTRUnitTestingClusterSimpleStruct)
c mtrUnitTestingClusterNestedStructList  =
    sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "c") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setC:@
setC :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setC mtrUnitTestingClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "setC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- d@
d :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSArray)
d mtrUnitTestingClusterNestedStructList  =
    sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "d") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setD:@
setD :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSArray value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setD mtrUnitTestingClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "setD:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- e@
e :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSArray)
e mtrUnitTestingClusterNestedStructList  =
    sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "e") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setE:@
setE :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSArray value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setE mtrUnitTestingClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "setE:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- f@
f :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSArray)
f mtrUnitTestingClusterNestedStructList  =
    sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "f") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setF:@
setF :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSArray value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setF mtrUnitTestingClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "setF:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- g@
g :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSArray)
g mtrUnitTestingClusterNestedStructList  =
    sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "g") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setG:@
setG :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSArray value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setG mtrUnitTestingClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterNestedStructList (mkSelector "setG:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @e@
eSelector :: Selector
eSelector = mkSelector "e"

-- | @Selector@ for @setE:@
setESelector :: Selector
setESelector = mkSelector "setE:"

-- | @Selector@ for @f@
fSelector :: Selector
fSelector = mkSelector "f"

-- | @Selector@ for @setF:@
setFSelector :: Selector
setFSelector = mkSelector "setF:"

-- | @Selector@ for @g@
gSelector :: Selector
gSelector = mkSelector "g"

-- | @Selector@ for @setG:@
setGSelector :: Selector
setGSelector = mkSelector "setG:"

