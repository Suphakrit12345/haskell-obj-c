{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterChannelPagingStruct@.
module ObjC.Matter.MTRChannelClusterChannelPagingStruct
  ( MTRChannelClusterChannelPagingStruct
  , IsMTRChannelClusterChannelPagingStruct(..)
  , previousToken
  , setPreviousToken
  , nextToken
  , setNextToken
  , previousTokenSelector
  , setPreviousTokenSelector
  , nextTokenSelector
  , setNextTokenSelector


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

-- | @- previousToken@
previousToken :: IsMTRChannelClusterChannelPagingStruct mtrChannelClusterChannelPagingStruct => mtrChannelClusterChannelPagingStruct -> IO (Id MTRChannelClusterPageTokenStruct)
previousToken mtrChannelClusterChannelPagingStruct  =
    sendMsg mtrChannelClusterChannelPagingStruct (mkSelector "previousToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousToken:@
setPreviousToken :: (IsMTRChannelClusterChannelPagingStruct mtrChannelClusterChannelPagingStruct, IsMTRChannelClusterPageTokenStruct value) => mtrChannelClusterChannelPagingStruct -> value -> IO ()
setPreviousToken mtrChannelClusterChannelPagingStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelPagingStruct (mkSelector "setPreviousToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nextToken@
nextToken :: IsMTRChannelClusterChannelPagingStruct mtrChannelClusterChannelPagingStruct => mtrChannelClusterChannelPagingStruct -> IO (Id MTRChannelClusterPageTokenStruct)
nextToken mtrChannelClusterChannelPagingStruct  =
    sendMsg mtrChannelClusterChannelPagingStruct (mkSelector "nextToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNextToken:@
setNextToken :: (IsMTRChannelClusterChannelPagingStruct mtrChannelClusterChannelPagingStruct, IsMTRChannelClusterPageTokenStruct value) => mtrChannelClusterChannelPagingStruct -> value -> IO ()
setNextToken mtrChannelClusterChannelPagingStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterChannelPagingStruct (mkSelector "setNextToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousToken@
previousTokenSelector :: Selector
previousTokenSelector = mkSelector "previousToken"

-- | @Selector@ for @setPreviousToken:@
setPreviousTokenSelector :: Selector
setPreviousTokenSelector = mkSelector "setPreviousToken:"

-- | @Selector@ for @nextToken@
nextTokenSelector :: Selector
nextTokenSelector = mkSelector "nextToken"

-- | @Selector@ for @setNextToken:@
setNextTokenSelector :: Selector
setNextTokenSelector = mkSelector "setNextToken:"

