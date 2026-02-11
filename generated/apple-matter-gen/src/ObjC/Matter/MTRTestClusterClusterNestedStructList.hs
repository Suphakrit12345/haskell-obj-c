{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterNestedStructList@.
module ObjC.Matter.MTRTestClusterClusterNestedStructList
  ( MTRTestClusterClusterNestedStructList
  , IsMTRTestClusterClusterNestedStructList(..)
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
a :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSNumber)
a mtrTestClusterClusterNestedStructList  =
    sendMsg mtrTestClusterClusterNestedStructList (mkSelector "a") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setA:@
setA :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSNumber value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setA mtrTestClusterClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStructList (mkSelector "setA:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- b@
b :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSNumber)
b mtrTestClusterClusterNestedStructList  =
    sendMsg mtrTestClusterClusterNestedStructList (mkSelector "b") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setB:@
setB :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSNumber value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setB mtrTestClusterClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStructList (mkSelector "setB:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- c@
c :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id MTRTestClusterClusterSimpleStruct)
c mtrTestClusterClusterNestedStructList  =
    sendMsg mtrTestClusterClusterNestedStructList (mkSelector "c") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setC:@
setC :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setC mtrTestClusterClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStructList (mkSelector "setC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- d@
d :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSArray)
d mtrTestClusterClusterNestedStructList  =
    sendMsg mtrTestClusterClusterNestedStructList (mkSelector "d") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setD:@
setD :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSArray value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setD mtrTestClusterClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStructList (mkSelector "setD:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- e@
e :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSArray)
e mtrTestClusterClusterNestedStructList  =
    sendMsg mtrTestClusterClusterNestedStructList (mkSelector "e") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setE:@
setE :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSArray value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setE mtrTestClusterClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStructList (mkSelector "setE:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- f@
f :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSArray)
f mtrTestClusterClusterNestedStructList  =
    sendMsg mtrTestClusterClusterNestedStructList (mkSelector "f") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setF:@
setF :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSArray value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setF mtrTestClusterClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStructList (mkSelector "setF:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- g@
g :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSArray)
g mtrTestClusterClusterNestedStructList  =
    sendMsg mtrTestClusterClusterNestedStructList (mkSelector "g") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setG:@
setG :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSArray value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setG mtrTestClusterClusterNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterNestedStructList (mkSelector "setG:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

