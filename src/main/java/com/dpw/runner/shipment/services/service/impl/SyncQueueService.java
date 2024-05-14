package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.AwbRequest;
import com.dpw.runner.shipment.services.dto.request.TriggerSyncRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.response.TriggerSyncResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationReverseSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentReverseSync;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletRequest;
import java.util.*;
import java.util.stream.Collectors;


@Slf4j
@Service
public class SyncQueueService implements ISyncQueueService {

    @Autowired
    private ISyncQueueDao syncQueueDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IShipmentReverseSync shipmentReverseSync;
    @Autowired
    private IConsolidationReverseSync consolidationReverseSync;
    @Autowired
    private IHblService hblService;
    @Autowired
    private IAwbService awbService;
    @Autowired
    private IContainerService containerService;
    @Autowired
    private IPackingService packingService;
    @Autowired
    private IELDetailsService elDetailsService;
    @Autowired
    private IEventService eventService;

    @Autowired
    private HttpServletRequest request;

    @Transactional
    public ResponseEntity<IRunnerResponse> saveSyncRequest(String moduleType, String moduleId, Object data) {
        String responseMsg;
        try {
            log.info("RequestId: {} || Request received for saveSyncRequest() for moduleType: {}, moduleId: {}, Data: {}", LoggerHelper.getRequestIdFromMDC(), moduleType, moduleId, jsonHelper.convertToJson(data));
            Integer syncTenantId;
            String syncTenantFromHeader = request.getHeader("X-TENANT-ID");
            if (!Objects.isNull(syncTenantFromHeader) && syncTenantFromHeader.matches("[0-9]+") )
                syncTenantId = Integer.parseInt(syncTenantFromHeader);
            else
                syncTenantId = UserContext.getUser().getTenantId();

            SyncQueue syncQueue = SyncQueue.builder()
                    .moduleType(moduleType).moduleId(moduleId).data(jsonHelper.convertToJson(data)).syncTenantId(syncTenantId)
                    .build();
            syncQueue = syncQueueDao.save(syncQueue);
            return ResponseHelper.buildSuccessResponse(syncQueue);
        }
        catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            log.error("Request: {} || Error occurred during event: {} exception: {}", LoggerHelper.getRequestIdFromMDC(), "PUSH_SYNC_REQUEST_TO_QUEUE", e.getLocalizedMessage());
            // TODO: Mayank to send email if needed
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    public ResponseEntity<IRunnerResponse> triggerSyncRequest(CommonRequestModel commonRequestModel) {
        TriggerSyncRequest request = (TriggerSyncRequest) commonRequestModel.getData();
        double _start = System.currentTimeMillis();
        log.error("Request: {} || TRIGGER_SYNC request received with data: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        if (Objects.isNull(request) || Objects.isNull(request.getModuleType()) || Objects.isNull(request.getTenantIds()) || request.getTenantIds().isEmpty()) {
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        List<SyncQueue> syncQueueList;
        if (Objects.equals(request.getModuleType(), SyncingConstants.ALL))
            syncQueueList = syncQueueDao.fetchDataByTenantIds(request.getTenantIds());
        else
            syncQueueList = syncQueueDao.fetchDataByModuleTypeAndTenantIds(request.getModuleType(), request.getTenantIds());

        if (Objects.isNull(syncQueueList) || syncQueueList.isEmpty()) {
            return ResponseHelper.buildSuccessResponse("No data is available for sync for this request");
        }

        List<TriggerSyncResponse.SyncResponse> succeeded = new ArrayList<>();
        List<TriggerSyncResponse.SyncResponse> failed = new ArrayList<>();

        for (SyncQueue element : syncQueueList) {

            TriggerSyncResponse.SyncResponse status = TriggerSyncResponse.SyncResponse.builder()
                                        .id(element.getId()).moduleId(element.getModuleId()).moduleType(element.getModuleType())
                                        .build();
            UserContext.getUser().setSyncTenantId(element.getSyncTenantId());
            log.info("Request: {} || TRIGGER_SYNC started for Module: {} with data: {}", LoggerHelper.getRequestIdFromMDC(), element.getModuleType(), jsonHelper.convertToJson(element));
            double _current = System.currentTimeMillis();

            try {
                switch (element.getModuleType()) {
                    case SyncingConstants.SHIPMENT:
                        shipmentReverseSync.reverseSync(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), CustomShipmentSyncRequest.class)).build(), false, true);
                        break;

                    case SyncingConstants.CONSOLIDATION:
                        consolidationReverseSync.reverseSync(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), CustomConsolidationRequest.class)).build(), false, true);
                        break;

                    case SyncingConstants.HBL:
                        hblService.saveV1Hbl(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), HblRequestV2.class)).build(), false);
                        break;

                    case SyncingConstants.AWB:
                        awbService.createV1Awb(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), AwbRequest.class)).build(), false);
                        break;

                    case SyncingConstants.CONTAINERS:
                        containerService.V1ContainerCreateAndUpdate(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), ContainerRequestV2.class)).build(), false);
                        break;

                    case SyncingConstants.PACKAGES:
                        packingService.V1PackingCreateAndUpdate(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), PackingRequestV2.class)).build(), false);
                        break;

                    case SyncingConstants.EL_DETAILS:
                        elDetailsService.V1ELDetailsCreateAndUpdate(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), ElDetailsRequestV2.class)).build(), false);
                        break;

                    case SyncingConstants.EVENTS:
                        eventService.V1EventsCreateAndUpdate(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), EventsRequestV2.class)).build(), false);
                        break;

//                    case SyncingConstants.PICKUP_DELIVERY:
//                        pickupDeliveryDetailsService.V1PickupDeliveryCreateAndUpdate(CommonRequestModel.builder().data(jsonHelper.readFromJson(element.getData(), PickupDeliveryDetailsRequestV2.class)).build(), false);
//                        break;
                    default:

                }

                succeeded.add(status);
            }
            catch (Exception ex) {
                log.error("Request: {} || TRIGGER_SYNC failed for queue element id: {} due to: {}", LoggerHelper.getRequestIdFromMDC(), element.getId(), ex.getLocalizedMessage());
                status.setReason(ex.getLocalizedMessage());
                failed.add(status);
                // TODO: Mayank to send email if needed
            }
            UserContext.getUser().setSyncTenantId(null);
            log.info("Request: {} || TRIGGER_SYNC ended for Module: {} with time taken: {} ms", LoggerHelper.getRequestIdFromMDC(), element.getModuleType(), (System.currentTimeMillis() - _current));
        }
        if (!succeeded.isEmpty())
            syncQueueDao.updateDataInActive(succeeded.stream().map(TriggerSyncResponse.SyncResponse::getId).collect(Collectors.toList()));
        log.info("Request: {} || TRIGGER_SYNC ended for all Module with total time taken: {} ms", LoggerHelper.getRequestIdFromMDC(), (System.currentTimeMillis() - _start));

        return ResponseHelper.buildSuccessResponse(TriggerSyncResponse.builder().succeeded(succeeded).failed(failed).build());
    }
}
