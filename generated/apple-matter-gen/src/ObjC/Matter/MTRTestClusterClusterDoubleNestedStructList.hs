{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterDoubleNestedStructList@.
module ObjC.Matter.MTRTestClusterClusterDoubleNestedStructList
  ( MTRTestClusterClusterDoubleNestedStructList
  , IsMTRTestClusterClusterDoubleNestedStructList(..)
  , a
  , setA
  , aSelector
  , setASelector


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
a :: IsMTRTestClusterClusterDoubleNestedStructList mtrTestClusterClusterDoubleNestedStructList => mtrTestClusterClusterDoubleNestedStructList -> IO (Id NSArray)
a mtrTestClusterClusterDoubleNestedStructList  =
    sendMsg mtrTestClusterClusterDoubleNestedStructList (mkSelector "a") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setA:@
setA :: (IsMTRTestClusterClusterDoubleNestedStructList mtrTestClusterClusterDoubleNestedStructList, IsNSArray value) => mtrTestClusterClusterDoubleNestedStructList -> value -> IO ()
setA mtrTestClusterClusterDoubleNestedStructList  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterDoubleNestedStructList (mkSelector "setA:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @a@
aSelector :: Selector
aSelector = mkSelector "a"

-- | @Selector@ for @setA:@
setASelector :: Selector
setASelector = mkSelector "setA:"

