{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterSimpleStruct@.
module ObjC.Matter.MTRTestClusterClusterSimpleStruct
  ( MTRTestClusterClusterSimpleStruct
  , IsMTRTestClusterClusterSimpleStruct(..)
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
  , h
  , setH
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
  , hSelector
  , setHSelector


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
a :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
a mtrTestClusterClusterSimpleStruct  =
    sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "a") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setA:@
setA :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setA mtrTestClusterClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "setA:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- b@
b :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
b mtrTestClusterClusterSimpleStruct  =
    sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "b") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setB:@
setB :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setB mtrTestClusterClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "setB:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- c@
c :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
c mtrTestClusterClusterSimpleStruct  =
    sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "c") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setC:@
setC :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setC mtrTestClusterClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "setC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- d@
d :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSData)
d mtrTestClusterClusterSimpleStruct  =
    sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "d") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setD:@
setD :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSData value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setD mtrTestClusterClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "setD:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- e@
e :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSString)
e mtrTestClusterClusterSimpleStruct  =
    sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "e") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setE:@
setE :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSString value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setE mtrTestClusterClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "setE:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- f@
f :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
f mtrTestClusterClusterSimpleStruct  =
    sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "f") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setF:@
setF :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setF mtrTestClusterClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "setF:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- g@
g :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
g mtrTestClusterClusterSimpleStruct  =
    sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "g") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setG:@
setG :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setG mtrTestClusterClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "setG:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- h@
h :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
h mtrTestClusterClusterSimpleStruct  =
    sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "h") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setH:@
setH :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setH mtrTestClusterClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterSimpleStruct (mkSelector "setH:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @h@
hSelector :: Selector
hSelector = mkSelector "h"

-- | @Selector@ for @setH:@
setHSelector :: Selector
setHSelector = mkSelector "setH:"

