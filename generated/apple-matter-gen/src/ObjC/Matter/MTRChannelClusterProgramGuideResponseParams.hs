{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterProgramGuideResponseParams@.
module ObjC.Matter.MTRChannelClusterProgramGuideResponseParams
  ( MTRChannelClusterProgramGuideResponseParams
  , IsMTRChannelClusterProgramGuideResponseParams(..)
  , initWithResponseValue_error
  , paging
  , setPaging
  , programList
  , setProgramList
  , initWithResponseValue_errorSelector
  , pagingSelector
  , setPagingSelector
  , programListSelector
  , setProgramListSelector


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

-- | Initialize an MTRChannelClusterProgramGuideResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrChannelClusterProgramGuideResponseParams -> responseValue -> error_ -> IO (Id MTRChannelClusterProgramGuideResponseParams)
initWithResponseValue_error mtrChannelClusterProgramGuideResponseParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrChannelClusterProgramGuideResponseParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- paging@
paging :: IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams => mtrChannelClusterProgramGuideResponseParams -> IO (Id MTRChannelClusterChannelPagingStruct)
paging mtrChannelClusterProgramGuideResponseParams  =
    sendMsg mtrChannelClusterProgramGuideResponseParams (mkSelector "paging") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaging:@
setPaging :: (IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams, IsMTRChannelClusterChannelPagingStruct value) => mtrChannelClusterProgramGuideResponseParams -> value -> IO ()
setPaging mtrChannelClusterProgramGuideResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramGuideResponseParams (mkSelector "setPaging:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- programList@
programList :: IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams => mtrChannelClusterProgramGuideResponseParams -> IO (Id NSArray)
programList mtrChannelClusterProgramGuideResponseParams  =
    sendMsg mtrChannelClusterProgramGuideResponseParams (mkSelector "programList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProgramList:@
setProgramList :: (IsMTRChannelClusterProgramGuideResponseParams mtrChannelClusterProgramGuideResponseParams, IsNSArray value) => mtrChannelClusterProgramGuideResponseParams -> value -> IO ()
setProgramList mtrChannelClusterProgramGuideResponseParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChannelClusterProgramGuideResponseParams (mkSelector "setProgramList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @paging@
pagingSelector :: Selector
pagingSelector = mkSelector "paging"

-- | @Selector@ for @setPaging:@
setPagingSelector :: Selector
setPagingSelector = mkSelector "setPaging:"

-- | @Selector@ for @programList@
programListSelector :: Selector
programListSelector = mkSelector "programList"

-- | @Selector@ for @setProgramList:@
setProgramListSelector :: Selector
setProgramListSelector = mkSelector "setProgramList:"

