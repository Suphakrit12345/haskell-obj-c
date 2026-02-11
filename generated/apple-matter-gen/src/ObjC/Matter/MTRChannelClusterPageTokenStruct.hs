{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterPageTokenStruct@.
module ObjC.Matter.MTRChannelClusterPageTokenStruct
  ( MTRChannelClusterPageTokenStruct
  , IsMTRChannelClusterPageTokenStruct(..)
  , limit
  , setLimit
  , after
  , setAfter
  , before
  , setBefore
  , limitSelector
  , setLimitSelector
  , afterSelector
  , setAfterSelector
  , beforeSelector
  , setBeforeSelector


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

-- | @- limit@
limit :: IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct => mtrChannelClusterPageTokenStruct -> IO (Id NSNumber)
limit mtrChannelClusterPageTokenStruct  =
    sendMsg mtrChannelClusterPageTokenStruct (mkSelector "limit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLimit:@
setLimit :: (IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct, IsNSNumber value) => mtrChannelClusterPageTokenStruct -> value -> IO ()
setLimit mtrChannelClusterPageTokenStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterPageTokenStruct (mkSelector "setLimit:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- after@
after :: IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct => mtrChannelClusterPageTokenStruct -> IO (Id NSString)
after mtrChannelClusterPageTokenStruct  =
    sendMsg mtrChannelClusterPageTokenStruct (mkSelector "after") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAfter:@
setAfter :: (IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct, IsNSString value) => mtrChannelClusterPageTokenStruct -> value -> IO ()
setAfter mtrChannelClusterPageTokenStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterPageTokenStruct (mkSelector "setAfter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- before@
before :: IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct => mtrChannelClusterPageTokenStruct -> IO (Id NSString)
before mtrChannelClusterPageTokenStruct  =
    sendMsg mtrChannelClusterPageTokenStruct (mkSelector "before") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBefore:@
setBefore :: (IsMTRChannelClusterPageTokenStruct mtrChannelClusterPageTokenStruct, IsNSString value) => mtrChannelClusterPageTokenStruct -> value -> IO ()
setBefore mtrChannelClusterPageTokenStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterPageTokenStruct (mkSelector "setBefore:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @limit@
limitSelector :: Selector
limitSelector = mkSelector "limit"

-- | @Selector@ for @setLimit:@
setLimitSelector :: Selector
setLimitSelector = mkSelector "setLimit:"

-- | @Selector@ for @after@
afterSelector :: Selector
afterSelector = mkSelector "after"

-- | @Selector@ for @setAfter:@
setAfterSelector :: Selector
setAfterSelector = mkSelector "setAfter:"

-- | @Selector@ for @before@
beforeSelector :: Selector
beforeSelector = mkSelector "before"

-- | @Selector@ for @setBefore:@
setBeforeSelector :: Selector
setBeforeSelector = mkSelector "setBefore:"

