{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentControlClusterRatingNameStruct@.
module ObjC.Matter.MTRContentControlClusterRatingNameStruct
  ( MTRContentControlClusterRatingNameStruct
  , IsMTRContentControlClusterRatingNameStruct(..)
  , ratingName
  , setRatingName
  , ratingNameDesc
  , setRatingNameDesc
  , ratingNameSelector
  , setRatingNameSelector
  , ratingNameDescSelector
  , setRatingNameDescSelector


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

-- | @- ratingName@
ratingName :: IsMTRContentControlClusterRatingNameStruct mtrContentControlClusterRatingNameStruct => mtrContentControlClusterRatingNameStruct -> IO (Id NSString)
ratingName mtrContentControlClusterRatingNameStruct  =
    sendMsg mtrContentControlClusterRatingNameStruct (mkSelector "ratingName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRatingName:@
setRatingName :: (IsMTRContentControlClusterRatingNameStruct mtrContentControlClusterRatingNameStruct, IsNSString value) => mtrContentControlClusterRatingNameStruct -> value -> IO ()
setRatingName mtrContentControlClusterRatingNameStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentControlClusterRatingNameStruct (mkSelector "setRatingName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ratingNameDesc@
ratingNameDesc :: IsMTRContentControlClusterRatingNameStruct mtrContentControlClusterRatingNameStruct => mtrContentControlClusterRatingNameStruct -> IO (Id NSString)
ratingNameDesc mtrContentControlClusterRatingNameStruct  =
    sendMsg mtrContentControlClusterRatingNameStruct (mkSelector "ratingNameDesc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRatingNameDesc:@
setRatingNameDesc :: (IsMTRContentControlClusterRatingNameStruct mtrContentControlClusterRatingNameStruct, IsNSString value) => mtrContentControlClusterRatingNameStruct -> value -> IO ()
setRatingNameDesc mtrContentControlClusterRatingNameStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrContentControlClusterRatingNameStruct (mkSelector "setRatingNameDesc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ratingName@
ratingNameSelector :: Selector
ratingNameSelector = mkSelector "ratingName"

-- | @Selector@ for @setRatingName:@
setRatingNameSelector :: Selector
setRatingNameSelector = mkSelector "setRatingName:"

-- | @Selector@ for @ratingNameDesc@
ratingNameDescSelector :: Selector
ratingNameDescSelector = mkSelector "ratingNameDesc"

-- | @Selector@ for @setRatingNameDesc:@
setRatingNameDescSelector :: Selector
setRatingNameDescSelector = mkSelector "setRatingNameDesc:"

