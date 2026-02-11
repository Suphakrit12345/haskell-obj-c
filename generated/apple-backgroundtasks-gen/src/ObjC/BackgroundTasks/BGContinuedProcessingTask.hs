{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A task meant to perform processing on behalf of a user initiated request.
--
-- Continued processing tasks will present UI while in progress to provide awareness to the user. ``BGContinuedProcessingTask``s _must_ report progress via the ``NSProgressReporting`` protocol conformance during runtime and are subject to expiration based on changing system conditions and user input. Tasks that appear stalled may be forcibly expired by the scheduler to preserve system resources.
--
-- Generated bindings for @BGContinuedProcessingTask@.
module ObjC.BackgroundTasks.BGContinuedProcessingTask
  ( BGContinuedProcessingTask
  , IsBGContinuedProcessingTask(..)
  , updateTitle_subtitle
  , title
  , subtitle
  , updateTitle_subtitleSelector
  , titleSelector
  , subtitleSelector


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

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Update the title and subtitle displayed in the live activity displayed to the user.
--
-- - Parameters:   - title: The localized title displayed to the user.   - subtitle: The localized subtitle displayed to the user.
--
-- ObjC selector: @- updateTitle:subtitle:@
updateTitle_subtitle :: (IsBGContinuedProcessingTask bgContinuedProcessingTask, IsNSString title, IsNSString subtitle) => bgContinuedProcessingTask -> title -> subtitle -> IO ()
updateTitle_subtitle bgContinuedProcessingTask  title subtitle =
  withObjCPtr title $ \raw_title ->
    withObjCPtr subtitle $ \raw_subtitle ->
        sendMsg bgContinuedProcessingTask (mkSelector "updateTitle:subtitle:") retVoid [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_subtitle :: Ptr ())]

-- | The localized title displayed to the user.
--
-- ObjC selector: @- title@
title :: IsBGContinuedProcessingTask bgContinuedProcessingTask => bgContinuedProcessingTask -> IO (Id NSString)
title bgContinuedProcessingTask  =
    sendMsg bgContinuedProcessingTask (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized subtitle displayed to the user.
--
-- ObjC selector: @- subtitle@
subtitle :: IsBGContinuedProcessingTask bgContinuedProcessingTask => bgContinuedProcessingTask -> IO (Id NSString)
subtitle bgContinuedProcessingTask  =
    sendMsg bgContinuedProcessingTask (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateTitle:subtitle:@
updateTitle_subtitleSelector :: Selector
updateTitle_subtitleSelector = mkSelector "updateTitle:subtitle:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

