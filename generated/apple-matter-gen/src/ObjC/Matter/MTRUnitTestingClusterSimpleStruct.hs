{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterSimpleStruct@.
module ObjC.Matter.MTRUnitTestingClusterSimpleStruct
  ( MTRUnitTestingClusterSimpleStruct
  , IsMTRUnitTestingClusterSimpleStruct(..)
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
  , i
  , setI
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
  , iSelector
  , setISelector


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
a :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
a mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "a") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setA:@
setA :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setA mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setA:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- b@
b :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
b mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "b") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setB:@
setB :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setB mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setB:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- c@
c :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
c mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "c") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setC:@
setC :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setC mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setC:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- d@
d :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSData)
d mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "d") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setD:@
setD :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSData value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setD mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setD:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- e@
e :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSString)
e mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "e") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setE:@
setE :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSString value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setE mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setE:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- f@
f :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
f mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "f") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setF:@
setF :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setF mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setF:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- g@
g :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
g mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "g") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setG:@
setG :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setG mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setG:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- h@
h :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
h mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "h") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setH:@
setH :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setH mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setH:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- i@
i :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
i mtrUnitTestingClusterSimpleStruct  =
    sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "i") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setI:@
setI :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setI mtrUnitTestingClusterSimpleStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrUnitTestingClusterSimpleStruct (mkSelector "setI:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @i@
iSelector :: Selector
iSelector = mkSelector "i"

-- | @Selector@ for @setI:@
setISelector :: Selector
setISelector = mkSelector "setI:"

