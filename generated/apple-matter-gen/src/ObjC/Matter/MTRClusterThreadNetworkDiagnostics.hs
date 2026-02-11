{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Network Diagnostics    The Thread Network Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems
--
-- Generated bindings for @MTRClusterThreadNetworkDiagnostics@.
module ObjC.Matter.MTRClusterThreadNetworkDiagnostics
  ( MTRClusterThreadNetworkDiagnostics
  , IsMTRClusterThreadNetworkDiagnostics(..)
  , resetCountsWithParams_expectedValues_expectedValueInterval_completion
  , resetCountsWithExpectedValues_expectedValueInterval_completion
  , readAttributeChannelWithParams
  , readAttributeRoutingRoleWithParams
  , readAttributeNetworkNameWithParams
  , readAttributePanIdWithParams
  , readAttributeExtendedPanIdWithParams
  , readAttributeMeshLocalPrefixWithParams
  , readAttributeOverrunCountWithParams
  , readAttributeNeighborTableWithParams
  , readAttributeRouteTableWithParams
  , readAttributePartitionIdWithParams
  , readAttributeWeightingWithParams
  , readAttributeDataVersionWithParams
  , readAttributeStableDataVersionWithParams
  , readAttributeLeaderRouterIdWithParams
  , readAttributeDetachedRoleCountWithParams
  , readAttributeChildRoleCountWithParams
  , readAttributeRouterRoleCountWithParams
  , readAttributeLeaderRoleCountWithParams
  , readAttributeAttachAttemptCountWithParams
  , readAttributePartitionIdChangeCountWithParams
  , readAttributeBetterPartitionAttachAttemptCountWithParams
  , readAttributeParentChangeCountWithParams
  , readAttributeTxTotalCountWithParams
  , readAttributeTxUnicastCountWithParams
  , readAttributeTxBroadcastCountWithParams
  , readAttributeTxAckRequestedCountWithParams
  , readAttributeTxAckedCountWithParams
  , readAttributeTxNoAckRequestedCountWithParams
  , readAttributeTxDataCountWithParams
  , readAttributeTxDataPollCountWithParams
  , readAttributeTxBeaconCountWithParams
  , readAttributeTxBeaconRequestCountWithParams
  , readAttributeTxOtherCountWithParams
  , readAttributeTxRetryCountWithParams
  , readAttributeTxDirectMaxRetryExpiryCountWithParams
  , readAttributeTxIndirectMaxRetryExpiryCountWithParams
  , readAttributeTxErrCcaCountWithParams
  , readAttributeTxErrAbortCountWithParams
  , readAttributeTxErrBusyChannelCountWithParams
  , readAttributeRxTotalCountWithParams
  , readAttributeRxUnicastCountWithParams
  , readAttributeRxBroadcastCountWithParams
  , readAttributeRxDataCountWithParams
  , readAttributeRxDataPollCountWithParams
  , readAttributeRxBeaconCountWithParams
  , readAttributeRxBeaconRequestCountWithParams
  , readAttributeRxOtherCountWithParams
  , readAttributeRxAddressFilteredCountWithParams
  , readAttributeRxDestAddrFilteredCountWithParams
  , readAttributeRxDuplicatedCountWithParams
  , readAttributeRxErrNoFrameCountWithParams
  , readAttributeRxErrUnknownNeighborCountWithParams
  , readAttributeRxErrInvalidSrcAddrCountWithParams
  , readAttributeRxErrSecCountWithParams
  , readAttributeRxErrFcsCountWithParams
  , readAttributeRxErrOtherCountWithParams
  , readAttributeActiveTimestampWithParams
  , readAttributePendingTimestampWithParams
  , readAttributeDelayWithParams
  , readAttributeSecurityPolicyWithParams
  , readAttributeChannelPage0MaskWithParams
  , readAttributeOperationalDatasetComponentsWithParams
  , readAttributeActiveNetworkFaultsListWithParams
  , readAttributeExtAddressWithParams
  , readAttributeRloc16WithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler
  , resetCountsWithExpectedValues_expectedValueInterval_completionHandler
  , readAttributeNeighborTableListWithParams
  , readAttributeRouteTableListWithParams
  , initWithDevice_endpointID_queue
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionSelector
  , readAttributeChannelWithParamsSelector
  , readAttributeRoutingRoleWithParamsSelector
  , readAttributeNetworkNameWithParamsSelector
  , readAttributePanIdWithParamsSelector
  , readAttributeExtendedPanIdWithParamsSelector
  , readAttributeMeshLocalPrefixWithParamsSelector
  , readAttributeOverrunCountWithParamsSelector
  , readAttributeNeighborTableWithParamsSelector
  , readAttributeRouteTableWithParamsSelector
  , readAttributePartitionIdWithParamsSelector
  , readAttributeWeightingWithParamsSelector
  , readAttributeDataVersionWithParamsSelector
  , readAttributeStableDataVersionWithParamsSelector
  , readAttributeLeaderRouterIdWithParamsSelector
  , readAttributeDetachedRoleCountWithParamsSelector
  , readAttributeChildRoleCountWithParamsSelector
  , readAttributeRouterRoleCountWithParamsSelector
  , readAttributeLeaderRoleCountWithParamsSelector
  , readAttributeAttachAttemptCountWithParamsSelector
  , readAttributePartitionIdChangeCountWithParamsSelector
  , readAttributeBetterPartitionAttachAttemptCountWithParamsSelector
  , readAttributeParentChangeCountWithParamsSelector
  , readAttributeTxTotalCountWithParamsSelector
  , readAttributeTxUnicastCountWithParamsSelector
  , readAttributeTxBroadcastCountWithParamsSelector
  , readAttributeTxAckRequestedCountWithParamsSelector
  , readAttributeTxAckedCountWithParamsSelector
  , readAttributeTxNoAckRequestedCountWithParamsSelector
  , readAttributeTxDataCountWithParamsSelector
  , readAttributeTxDataPollCountWithParamsSelector
  , readAttributeTxBeaconCountWithParamsSelector
  , readAttributeTxBeaconRequestCountWithParamsSelector
  , readAttributeTxOtherCountWithParamsSelector
  , readAttributeTxRetryCountWithParamsSelector
  , readAttributeTxDirectMaxRetryExpiryCountWithParamsSelector
  , readAttributeTxIndirectMaxRetryExpiryCountWithParamsSelector
  , readAttributeTxErrCcaCountWithParamsSelector
  , readAttributeTxErrAbortCountWithParamsSelector
  , readAttributeTxErrBusyChannelCountWithParamsSelector
  , readAttributeRxTotalCountWithParamsSelector
  , readAttributeRxUnicastCountWithParamsSelector
  , readAttributeRxBroadcastCountWithParamsSelector
  , readAttributeRxDataCountWithParamsSelector
  , readAttributeRxDataPollCountWithParamsSelector
  , readAttributeRxBeaconCountWithParamsSelector
  , readAttributeRxBeaconRequestCountWithParamsSelector
  , readAttributeRxOtherCountWithParamsSelector
  , readAttributeRxAddressFilteredCountWithParamsSelector
  , readAttributeRxDestAddrFilteredCountWithParamsSelector
  , readAttributeRxDuplicatedCountWithParamsSelector
  , readAttributeRxErrNoFrameCountWithParamsSelector
  , readAttributeRxErrUnknownNeighborCountWithParamsSelector
  , readAttributeRxErrInvalidSrcAddrCountWithParamsSelector
  , readAttributeRxErrSecCountWithParamsSelector
  , readAttributeRxErrFcsCountWithParamsSelector
  , readAttributeRxErrOtherCountWithParamsSelector
  , readAttributeActiveTimestampWithParamsSelector
  , readAttributePendingTimestampWithParamsSelector
  , readAttributeDelayWithParamsSelector
  , readAttributeSecurityPolicyWithParamsSelector
  , readAttributeChannelPage0MaskWithParamsSelector
  , readAttributeOperationalDatasetComponentsWithParamsSelector
  , readAttributeActiveNetworkFaultsListWithParamsSelector
  , readAttributeExtAddressWithParamsSelector
  , readAttributeRloc16WithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , initSelector
  , newSelector
  , initWithDevice_endpoint_queueSelector
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , readAttributeNeighborTableListWithParamsSelector
  , readAttributeRouteTableListWithParamsSelector
  , initWithDevice_endpointID_queueSelector


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

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRThreadNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completion =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDiagnostics  expectedValues expectedValueIntervalMs completion =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completion:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- readAttributeChannelWithParams:@
readAttributeChannelWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeChannelWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeChannelWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRoutingRoleWithParams:@
readAttributeRoutingRoleWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRoutingRoleWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRoutingRoleWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNetworkNameWithParams:@
readAttributeNetworkNameWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeNetworkNameWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeNetworkNameWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePanIdWithParams:@
readAttributePanIdWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePanIdWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributePanIdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeExtendedPanIdWithParams:@
readAttributeExtendedPanIdWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeExtendedPanIdWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeExtendedPanIdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeMeshLocalPrefixWithParams:@
readAttributeMeshLocalPrefixWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeMeshLocalPrefixWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeMeshLocalPrefixWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeOverrunCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeOverrunCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeNeighborTableWithParams:@
readAttributeNeighborTableWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeNeighborTableWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeNeighborTableWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRouteTableWithParams:@
readAttributeRouteTableWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRouteTableWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRouteTableWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePartitionIdWithParams:@
readAttributePartitionIdWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePartitionIdWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributePartitionIdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeWeightingWithParams:@
readAttributeWeightingWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeWeightingWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeWeightingWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDataVersionWithParams:@
readAttributeDataVersionWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeDataVersionWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeDataVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeStableDataVersionWithParams:@
readAttributeStableDataVersionWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeStableDataVersionWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeStableDataVersionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLeaderRouterIdWithParams:@
readAttributeLeaderRouterIdWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeLeaderRouterIdWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeLeaderRouterIdWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDetachedRoleCountWithParams:@
readAttributeDetachedRoleCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeDetachedRoleCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeDetachedRoleCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeChildRoleCountWithParams:@
readAttributeChildRoleCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeChildRoleCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeChildRoleCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRouterRoleCountWithParams:@
readAttributeRouterRoleCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRouterRoleCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRouterRoleCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeLeaderRoleCountWithParams:@
readAttributeLeaderRoleCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeLeaderRoleCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeLeaderRoleCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttachAttemptCountWithParams:@
readAttributeAttachAttemptCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttachAttemptCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeAttachAttemptCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePartitionIdChangeCountWithParams:@
readAttributePartitionIdChangeCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePartitionIdChangeCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributePartitionIdChangeCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeBetterPartitionAttachAttemptCountWithParams:@
readAttributeBetterPartitionAttachAttemptCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeBetterPartitionAttachAttemptCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeBetterPartitionAttachAttemptCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeParentChangeCountWithParams:@
readAttributeParentChangeCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeParentChangeCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeParentChangeCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxTotalCountWithParams:@
readAttributeTxTotalCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxTotalCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxTotalCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxUnicastCountWithParams:@
readAttributeTxUnicastCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxUnicastCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxUnicastCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxBroadcastCountWithParams:@
readAttributeTxBroadcastCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxBroadcastCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxBroadcastCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxAckRequestedCountWithParams:@
readAttributeTxAckRequestedCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxAckRequestedCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxAckRequestedCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxAckedCountWithParams:@
readAttributeTxAckedCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxAckedCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxAckedCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxNoAckRequestedCountWithParams:@
readAttributeTxNoAckRequestedCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxNoAckRequestedCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxNoAckRequestedCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxDataCountWithParams:@
readAttributeTxDataCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxDataCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxDataCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxDataPollCountWithParams:@
readAttributeTxDataPollCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxDataPollCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxDataPollCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxBeaconCountWithParams:@
readAttributeTxBeaconCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxBeaconCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxBeaconCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxBeaconRequestCountWithParams:@
readAttributeTxBeaconRequestCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxBeaconRequestCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxBeaconRequestCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxOtherCountWithParams:@
readAttributeTxOtherCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxOtherCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxOtherCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxRetryCountWithParams:@
readAttributeTxRetryCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxRetryCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxRetryCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxDirectMaxRetryExpiryCountWithParams:@
readAttributeTxDirectMaxRetryExpiryCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxDirectMaxRetryExpiryCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxDirectMaxRetryExpiryCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxIndirectMaxRetryExpiryCountWithParams:@
readAttributeTxIndirectMaxRetryExpiryCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxIndirectMaxRetryExpiryCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxIndirectMaxRetryExpiryCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxErrCcaCountWithParams:@
readAttributeTxErrCcaCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxErrCcaCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxErrCcaCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxErrAbortCountWithParams:@
readAttributeTxErrAbortCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxErrAbortCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxErrAbortCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeTxErrBusyChannelCountWithParams:@
readAttributeTxErrBusyChannelCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxErrBusyChannelCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeTxErrBusyChannelCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxTotalCountWithParams:@
readAttributeRxTotalCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxTotalCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxTotalCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxUnicastCountWithParams:@
readAttributeRxUnicastCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxUnicastCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxUnicastCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxBroadcastCountWithParams:@
readAttributeRxBroadcastCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxBroadcastCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxBroadcastCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxDataCountWithParams:@
readAttributeRxDataCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxDataCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxDataCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxDataPollCountWithParams:@
readAttributeRxDataPollCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxDataPollCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxDataPollCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxBeaconCountWithParams:@
readAttributeRxBeaconCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxBeaconCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxBeaconCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxBeaconRequestCountWithParams:@
readAttributeRxBeaconRequestCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxBeaconRequestCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxBeaconRequestCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxOtherCountWithParams:@
readAttributeRxOtherCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxOtherCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxOtherCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxAddressFilteredCountWithParams:@
readAttributeRxAddressFilteredCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxAddressFilteredCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxAddressFilteredCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxDestAddrFilteredCountWithParams:@
readAttributeRxDestAddrFilteredCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxDestAddrFilteredCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxDestAddrFilteredCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxDuplicatedCountWithParams:@
readAttributeRxDuplicatedCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxDuplicatedCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxDuplicatedCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxErrNoFrameCountWithParams:@
readAttributeRxErrNoFrameCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrNoFrameCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxErrNoFrameCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxErrUnknownNeighborCountWithParams:@
readAttributeRxErrUnknownNeighborCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrUnknownNeighborCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxErrUnknownNeighborCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxErrInvalidSrcAddrCountWithParams:@
readAttributeRxErrInvalidSrcAddrCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrInvalidSrcAddrCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxErrInvalidSrcAddrCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxErrSecCountWithParams:@
readAttributeRxErrSecCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrSecCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxErrSecCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxErrFcsCountWithParams:@
readAttributeRxErrFcsCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrFcsCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxErrFcsCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRxErrOtherCountWithParams:@
readAttributeRxErrOtherCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrOtherCountWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRxErrOtherCountWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveTimestampWithParams:@
readAttributeActiveTimestampWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeActiveTimestampWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeActiveTimestampWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributePendingTimestampWithParams:@
readAttributePendingTimestampWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePendingTimestampWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributePendingTimestampWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeDelayWithParams:@
readAttributeDelayWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeDelayWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeDelayWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeSecurityPolicyWithParams:@
readAttributeSecurityPolicyWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeSecurityPolicyWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeSecurityPolicyWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeChannelPage0MaskWithParams:@
readAttributeChannelPage0MaskWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeChannelPage0MaskWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeChannelPage0MaskWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeOperationalDatasetComponentsWithParams:@
readAttributeOperationalDatasetComponentsWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeOperationalDatasetComponentsWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeOperationalDatasetComponentsWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeActiveNetworkFaultsListWithParams:@
readAttributeActiveNetworkFaultsListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeActiveNetworkFaultsListWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeActiveNetworkFaultsListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeExtAddressWithParams:@
readAttributeExtAddressWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeExtAddressWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeExtAddressWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRloc16WithParams:@
readAttributeRloc16WithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRloc16WithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRloc16WithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeGeneratedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeAcceptedCommandListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeAttributeListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeFeatureMapWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeClusterRevisionWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics => mtrClusterThreadNetworkDiagnostics -> IO (Id MTRClusterThreadNetworkDiagnostics)
init_ mtrClusterThreadNetworkDiagnostics  =
    sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRClusterThreadNetworkDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThreadNetworkDiagnostics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRDevice device, IsNSObject queue) => mtrClusterThreadNetworkDiagnostics -> device -> CUShort -> queue -> IO (Id MTRClusterThreadNetworkDiagnostics)
initWithDevice_endpoint_queue mtrClusterThreadNetworkDiagnostics  device endpoint queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr queue $ \raw_queue ->
        sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "initWithDevice:endpoint:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argCUInt (fromIntegral endpoint), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRThreadNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterThreadNetworkDiagnostics  params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  withObjCPtr params $ \raw_params ->
    withObjCPtr expectedDataValueDictionaries $ \raw_expectedDataValueDictionaries ->
      withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
          sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_params :: Ptr ()), argPtr (castPtr raw_expectedDataValueDictionaries :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completionHandler mtrClusterThreadNetworkDiagnostics  expectedValues expectedValueIntervalMs completionHandler =
  withObjCPtr expectedValues $ \raw_expectedValues ->
    withObjCPtr expectedValueIntervalMs $ \raw_expectedValueIntervalMs ->
        sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completionHandler:") retVoid [argPtr (castPtr raw_expectedValues :: Ptr ()), argPtr (castPtr raw_expectedValueIntervalMs :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- readAttributeNeighborTableListWithParams:@
readAttributeNeighborTableListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeNeighborTableListWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeNeighborTableListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- readAttributeRouteTableListWithParams:@
readAttributeRouteTableListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRouteTableListWithParams mtrClusterThreadNetworkDiagnostics  params =
  withObjCPtr params $ \raw_params ->
      sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "readAttributeRouteTableListWithParams:") (retPtr retVoid) [argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThreadNetworkDiagnostics -> device -> endpointID -> queue -> IO (Id MTRClusterThreadNetworkDiagnostics)
initWithDevice_endpointID_queue mtrClusterThreadNetworkDiagnostics  device endpointID queue =
  withObjCPtr device $ \raw_device ->
    withObjCPtr endpointID $ \raw_endpointID ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrClusterThreadNetworkDiagnostics (mkSelector "initWithDevice:endpointID:queue:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_endpointID :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completionSelector :: Selector
resetCountsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeChannelWithParams:@
readAttributeChannelWithParamsSelector :: Selector
readAttributeChannelWithParamsSelector = mkSelector "readAttributeChannelWithParams:"

-- | @Selector@ for @readAttributeRoutingRoleWithParams:@
readAttributeRoutingRoleWithParamsSelector :: Selector
readAttributeRoutingRoleWithParamsSelector = mkSelector "readAttributeRoutingRoleWithParams:"

-- | @Selector@ for @readAttributeNetworkNameWithParams:@
readAttributeNetworkNameWithParamsSelector :: Selector
readAttributeNetworkNameWithParamsSelector = mkSelector "readAttributeNetworkNameWithParams:"

-- | @Selector@ for @readAttributePanIdWithParams:@
readAttributePanIdWithParamsSelector :: Selector
readAttributePanIdWithParamsSelector = mkSelector "readAttributePanIdWithParams:"

-- | @Selector@ for @readAttributeExtendedPanIdWithParams:@
readAttributeExtendedPanIdWithParamsSelector :: Selector
readAttributeExtendedPanIdWithParamsSelector = mkSelector "readAttributeExtendedPanIdWithParams:"

-- | @Selector@ for @readAttributeMeshLocalPrefixWithParams:@
readAttributeMeshLocalPrefixWithParamsSelector :: Selector
readAttributeMeshLocalPrefixWithParamsSelector = mkSelector "readAttributeMeshLocalPrefixWithParams:"

-- | @Selector@ for @readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParamsSelector :: Selector
readAttributeOverrunCountWithParamsSelector = mkSelector "readAttributeOverrunCountWithParams:"

-- | @Selector@ for @readAttributeNeighborTableWithParams:@
readAttributeNeighborTableWithParamsSelector :: Selector
readAttributeNeighborTableWithParamsSelector = mkSelector "readAttributeNeighborTableWithParams:"

-- | @Selector@ for @readAttributeRouteTableWithParams:@
readAttributeRouteTableWithParamsSelector :: Selector
readAttributeRouteTableWithParamsSelector = mkSelector "readAttributeRouteTableWithParams:"

-- | @Selector@ for @readAttributePartitionIdWithParams:@
readAttributePartitionIdWithParamsSelector :: Selector
readAttributePartitionIdWithParamsSelector = mkSelector "readAttributePartitionIdWithParams:"

-- | @Selector@ for @readAttributeWeightingWithParams:@
readAttributeWeightingWithParamsSelector :: Selector
readAttributeWeightingWithParamsSelector = mkSelector "readAttributeWeightingWithParams:"

-- | @Selector@ for @readAttributeDataVersionWithParams:@
readAttributeDataVersionWithParamsSelector :: Selector
readAttributeDataVersionWithParamsSelector = mkSelector "readAttributeDataVersionWithParams:"

-- | @Selector@ for @readAttributeStableDataVersionWithParams:@
readAttributeStableDataVersionWithParamsSelector :: Selector
readAttributeStableDataVersionWithParamsSelector = mkSelector "readAttributeStableDataVersionWithParams:"

-- | @Selector@ for @readAttributeLeaderRouterIdWithParams:@
readAttributeLeaderRouterIdWithParamsSelector :: Selector
readAttributeLeaderRouterIdWithParamsSelector = mkSelector "readAttributeLeaderRouterIdWithParams:"

-- | @Selector@ for @readAttributeDetachedRoleCountWithParams:@
readAttributeDetachedRoleCountWithParamsSelector :: Selector
readAttributeDetachedRoleCountWithParamsSelector = mkSelector "readAttributeDetachedRoleCountWithParams:"

-- | @Selector@ for @readAttributeChildRoleCountWithParams:@
readAttributeChildRoleCountWithParamsSelector :: Selector
readAttributeChildRoleCountWithParamsSelector = mkSelector "readAttributeChildRoleCountWithParams:"

-- | @Selector@ for @readAttributeRouterRoleCountWithParams:@
readAttributeRouterRoleCountWithParamsSelector :: Selector
readAttributeRouterRoleCountWithParamsSelector = mkSelector "readAttributeRouterRoleCountWithParams:"

-- | @Selector@ for @readAttributeLeaderRoleCountWithParams:@
readAttributeLeaderRoleCountWithParamsSelector :: Selector
readAttributeLeaderRoleCountWithParamsSelector = mkSelector "readAttributeLeaderRoleCountWithParams:"

-- | @Selector@ for @readAttributeAttachAttemptCountWithParams:@
readAttributeAttachAttemptCountWithParamsSelector :: Selector
readAttributeAttachAttemptCountWithParamsSelector = mkSelector "readAttributeAttachAttemptCountWithParams:"

-- | @Selector@ for @readAttributePartitionIdChangeCountWithParams:@
readAttributePartitionIdChangeCountWithParamsSelector :: Selector
readAttributePartitionIdChangeCountWithParamsSelector = mkSelector "readAttributePartitionIdChangeCountWithParams:"

-- | @Selector@ for @readAttributeBetterPartitionAttachAttemptCountWithParams:@
readAttributeBetterPartitionAttachAttemptCountWithParamsSelector :: Selector
readAttributeBetterPartitionAttachAttemptCountWithParamsSelector = mkSelector "readAttributeBetterPartitionAttachAttemptCountWithParams:"

-- | @Selector@ for @readAttributeParentChangeCountWithParams:@
readAttributeParentChangeCountWithParamsSelector :: Selector
readAttributeParentChangeCountWithParamsSelector = mkSelector "readAttributeParentChangeCountWithParams:"

-- | @Selector@ for @readAttributeTxTotalCountWithParams:@
readAttributeTxTotalCountWithParamsSelector :: Selector
readAttributeTxTotalCountWithParamsSelector = mkSelector "readAttributeTxTotalCountWithParams:"

-- | @Selector@ for @readAttributeTxUnicastCountWithParams:@
readAttributeTxUnicastCountWithParamsSelector :: Selector
readAttributeTxUnicastCountWithParamsSelector = mkSelector "readAttributeTxUnicastCountWithParams:"

-- | @Selector@ for @readAttributeTxBroadcastCountWithParams:@
readAttributeTxBroadcastCountWithParamsSelector :: Selector
readAttributeTxBroadcastCountWithParamsSelector = mkSelector "readAttributeTxBroadcastCountWithParams:"

-- | @Selector@ for @readAttributeTxAckRequestedCountWithParams:@
readAttributeTxAckRequestedCountWithParamsSelector :: Selector
readAttributeTxAckRequestedCountWithParamsSelector = mkSelector "readAttributeTxAckRequestedCountWithParams:"

-- | @Selector@ for @readAttributeTxAckedCountWithParams:@
readAttributeTxAckedCountWithParamsSelector :: Selector
readAttributeTxAckedCountWithParamsSelector = mkSelector "readAttributeTxAckedCountWithParams:"

-- | @Selector@ for @readAttributeTxNoAckRequestedCountWithParams:@
readAttributeTxNoAckRequestedCountWithParamsSelector :: Selector
readAttributeTxNoAckRequestedCountWithParamsSelector = mkSelector "readAttributeTxNoAckRequestedCountWithParams:"

-- | @Selector@ for @readAttributeTxDataCountWithParams:@
readAttributeTxDataCountWithParamsSelector :: Selector
readAttributeTxDataCountWithParamsSelector = mkSelector "readAttributeTxDataCountWithParams:"

-- | @Selector@ for @readAttributeTxDataPollCountWithParams:@
readAttributeTxDataPollCountWithParamsSelector :: Selector
readAttributeTxDataPollCountWithParamsSelector = mkSelector "readAttributeTxDataPollCountWithParams:"

-- | @Selector@ for @readAttributeTxBeaconCountWithParams:@
readAttributeTxBeaconCountWithParamsSelector :: Selector
readAttributeTxBeaconCountWithParamsSelector = mkSelector "readAttributeTxBeaconCountWithParams:"

-- | @Selector@ for @readAttributeTxBeaconRequestCountWithParams:@
readAttributeTxBeaconRequestCountWithParamsSelector :: Selector
readAttributeTxBeaconRequestCountWithParamsSelector = mkSelector "readAttributeTxBeaconRequestCountWithParams:"

-- | @Selector@ for @readAttributeTxOtherCountWithParams:@
readAttributeTxOtherCountWithParamsSelector :: Selector
readAttributeTxOtherCountWithParamsSelector = mkSelector "readAttributeTxOtherCountWithParams:"

-- | @Selector@ for @readAttributeTxRetryCountWithParams:@
readAttributeTxRetryCountWithParamsSelector :: Selector
readAttributeTxRetryCountWithParamsSelector = mkSelector "readAttributeTxRetryCountWithParams:"

-- | @Selector@ for @readAttributeTxDirectMaxRetryExpiryCountWithParams:@
readAttributeTxDirectMaxRetryExpiryCountWithParamsSelector :: Selector
readAttributeTxDirectMaxRetryExpiryCountWithParamsSelector = mkSelector "readAttributeTxDirectMaxRetryExpiryCountWithParams:"

-- | @Selector@ for @readAttributeTxIndirectMaxRetryExpiryCountWithParams:@
readAttributeTxIndirectMaxRetryExpiryCountWithParamsSelector :: Selector
readAttributeTxIndirectMaxRetryExpiryCountWithParamsSelector = mkSelector "readAttributeTxIndirectMaxRetryExpiryCountWithParams:"

-- | @Selector@ for @readAttributeTxErrCcaCountWithParams:@
readAttributeTxErrCcaCountWithParamsSelector :: Selector
readAttributeTxErrCcaCountWithParamsSelector = mkSelector "readAttributeTxErrCcaCountWithParams:"

-- | @Selector@ for @readAttributeTxErrAbortCountWithParams:@
readAttributeTxErrAbortCountWithParamsSelector :: Selector
readAttributeTxErrAbortCountWithParamsSelector = mkSelector "readAttributeTxErrAbortCountWithParams:"

-- | @Selector@ for @readAttributeTxErrBusyChannelCountWithParams:@
readAttributeTxErrBusyChannelCountWithParamsSelector :: Selector
readAttributeTxErrBusyChannelCountWithParamsSelector = mkSelector "readAttributeTxErrBusyChannelCountWithParams:"

-- | @Selector@ for @readAttributeRxTotalCountWithParams:@
readAttributeRxTotalCountWithParamsSelector :: Selector
readAttributeRxTotalCountWithParamsSelector = mkSelector "readAttributeRxTotalCountWithParams:"

-- | @Selector@ for @readAttributeRxUnicastCountWithParams:@
readAttributeRxUnicastCountWithParamsSelector :: Selector
readAttributeRxUnicastCountWithParamsSelector = mkSelector "readAttributeRxUnicastCountWithParams:"

-- | @Selector@ for @readAttributeRxBroadcastCountWithParams:@
readAttributeRxBroadcastCountWithParamsSelector :: Selector
readAttributeRxBroadcastCountWithParamsSelector = mkSelector "readAttributeRxBroadcastCountWithParams:"

-- | @Selector@ for @readAttributeRxDataCountWithParams:@
readAttributeRxDataCountWithParamsSelector :: Selector
readAttributeRxDataCountWithParamsSelector = mkSelector "readAttributeRxDataCountWithParams:"

-- | @Selector@ for @readAttributeRxDataPollCountWithParams:@
readAttributeRxDataPollCountWithParamsSelector :: Selector
readAttributeRxDataPollCountWithParamsSelector = mkSelector "readAttributeRxDataPollCountWithParams:"

-- | @Selector@ for @readAttributeRxBeaconCountWithParams:@
readAttributeRxBeaconCountWithParamsSelector :: Selector
readAttributeRxBeaconCountWithParamsSelector = mkSelector "readAttributeRxBeaconCountWithParams:"

-- | @Selector@ for @readAttributeRxBeaconRequestCountWithParams:@
readAttributeRxBeaconRequestCountWithParamsSelector :: Selector
readAttributeRxBeaconRequestCountWithParamsSelector = mkSelector "readAttributeRxBeaconRequestCountWithParams:"

-- | @Selector@ for @readAttributeRxOtherCountWithParams:@
readAttributeRxOtherCountWithParamsSelector :: Selector
readAttributeRxOtherCountWithParamsSelector = mkSelector "readAttributeRxOtherCountWithParams:"

-- | @Selector@ for @readAttributeRxAddressFilteredCountWithParams:@
readAttributeRxAddressFilteredCountWithParamsSelector :: Selector
readAttributeRxAddressFilteredCountWithParamsSelector = mkSelector "readAttributeRxAddressFilteredCountWithParams:"

-- | @Selector@ for @readAttributeRxDestAddrFilteredCountWithParams:@
readAttributeRxDestAddrFilteredCountWithParamsSelector :: Selector
readAttributeRxDestAddrFilteredCountWithParamsSelector = mkSelector "readAttributeRxDestAddrFilteredCountWithParams:"

-- | @Selector@ for @readAttributeRxDuplicatedCountWithParams:@
readAttributeRxDuplicatedCountWithParamsSelector :: Selector
readAttributeRxDuplicatedCountWithParamsSelector = mkSelector "readAttributeRxDuplicatedCountWithParams:"

-- | @Selector@ for @readAttributeRxErrNoFrameCountWithParams:@
readAttributeRxErrNoFrameCountWithParamsSelector :: Selector
readAttributeRxErrNoFrameCountWithParamsSelector = mkSelector "readAttributeRxErrNoFrameCountWithParams:"

-- | @Selector@ for @readAttributeRxErrUnknownNeighborCountWithParams:@
readAttributeRxErrUnknownNeighborCountWithParamsSelector :: Selector
readAttributeRxErrUnknownNeighborCountWithParamsSelector = mkSelector "readAttributeRxErrUnknownNeighborCountWithParams:"

-- | @Selector@ for @readAttributeRxErrInvalidSrcAddrCountWithParams:@
readAttributeRxErrInvalidSrcAddrCountWithParamsSelector :: Selector
readAttributeRxErrInvalidSrcAddrCountWithParamsSelector = mkSelector "readAttributeRxErrInvalidSrcAddrCountWithParams:"

-- | @Selector@ for @readAttributeRxErrSecCountWithParams:@
readAttributeRxErrSecCountWithParamsSelector :: Selector
readAttributeRxErrSecCountWithParamsSelector = mkSelector "readAttributeRxErrSecCountWithParams:"

-- | @Selector@ for @readAttributeRxErrFcsCountWithParams:@
readAttributeRxErrFcsCountWithParamsSelector :: Selector
readAttributeRxErrFcsCountWithParamsSelector = mkSelector "readAttributeRxErrFcsCountWithParams:"

-- | @Selector@ for @readAttributeRxErrOtherCountWithParams:@
readAttributeRxErrOtherCountWithParamsSelector :: Selector
readAttributeRxErrOtherCountWithParamsSelector = mkSelector "readAttributeRxErrOtherCountWithParams:"

-- | @Selector@ for @readAttributeActiveTimestampWithParams:@
readAttributeActiveTimestampWithParamsSelector :: Selector
readAttributeActiveTimestampWithParamsSelector = mkSelector "readAttributeActiveTimestampWithParams:"

-- | @Selector@ for @readAttributePendingTimestampWithParams:@
readAttributePendingTimestampWithParamsSelector :: Selector
readAttributePendingTimestampWithParamsSelector = mkSelector "readAttributePendingTimestampWithParams:"

-- | @Selector@ for @readAttributeDelayWithParams:@
readAttributeDelayWithParamsSelector :: Selector
readAttributeDelayWithParamsSelector = mkSelector "readAttributeDelayWithParams:"

-- | @Selector@ for @readAttributeSecurityPolicyWithParams:@
readAttributeSecurityPolicyWithParamsSelector :: Selector
readAttributeSecurityPolicyWithParamsSelector = mkSelector "readAttributeSecurityPolicyWithParams:"

-- | @Selector@ for @readAttributeChannelPage0MaskWithParams:@
readAttributeChannelPage0MaskWithParamsSelector :: Selector
readAttributeChannelPage0MaskWithParamsSelector = mkSelector "readAttributeChannelPage0MaskWithParams:"

-- | @Selector@ for @readAttributeOperationalDatasetComponentsWithParams:@
readAttributeOperationalDatasetComponentsWithParamsSelector :: Selector
readAttributeOperationalDatasetComponentsWithParamsSelector = mkSelector "readAttributeOperationalDatasetComponentsWithParams:"

-- | @Selector@ for @readAttributeActiveNetworkFaultsListWithParams:@
readAttributeActiveNetworkFaultsListWithParamsSelector :: Selector
readAttributeActiveNetworkFaultsListWithParamsSelector = mkSelector "readAttributeActiveNetworkFaultsListWithParams:"

-- | @Selector@ for @readAttributeExtAddressWithParams:@
readAttributeExtAddressWithParamsSelector :: Selector
readAttributeExtAddressWithParamsSelector = mkSelector "readAttributeExtAddressWithParams:"

-- | @Selector@ for @readAttributeRloc16WithParams:@
readAttributeRloc16WithParamsSelector :: Selector
readAttributeRloc16WithParamsSelector = mkSelector "readAttributeRloc16WithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector
resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @readAttributeNeighborTableListWithParams:@
readAttributeNeighborTableListWithParamsSelector :: Selector
readAttributeNeighborTableListWithParamsSelector = mkSelector "readAttributeNeighborTableListWithParams:"

-- | @Selector@ for @readAttributeRouteTableListWithParams:@
readAttributeRouteTableListWithParamsSelector :: Selector
readAttributeRouteTableListWithParamsSelector = mkSelector "readAttributeRouteTableListWithParams:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

