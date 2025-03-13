package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Slf4j
@Service
public class ContainersSync implements IContainersSync {

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    RestTemplate restTemplate;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private IShipmentDao shipmentDao;

    @Value("${v1service.url.base}${v1service.url.containersSync}")
    private String CONTAINER_V1_SYNC_URL;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private EmailServiceUtility emailServiceUtility;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;
    @Autowired
    private ISyncService syncService;
    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Override
    public ResponseEntity<IRunnerResponse> sync(List<Long> containerIds, Page<ShipmentsContainersMapping> shipmentsContainersMappingPageable) {
        return sync(containerIds, shipmentsContainersMappingPageable, UUID.randomUUID().toString());
    }

    @Override
    public ResponseEntity<IRunnerResponse> sync(List<Long> containerIds, Page<ShipmentsContainersMapping> shipmentsContainersMappingPageable, String transactionId) {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return ResponseHelper.buildSuccessResponse();

        List<Containers> containers = getContainersFromIds(containerIds);
        if(containers == null || containers.size() == 0) {
            log.error("Error in syncing containers: Not able to get containers for ids: " + containerIds.toString());
        }
        List<ContainerRequestV2> containerRequestV2 = convertEntityToSyncDto(containers, shipmentsContainersMappingPageable);
        String json = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(containerRequestV2).module(SyncingConstants.CONTAINERS).build());
        syncService.pushToKafka(json, containers.stream().map(BaseEntity::getId).toList().toString(), containers.stream().map(BaseEntity::getGuid).toList().toString(), "Containers", transactionId);
        return ResponseHelper.buildSuccessResponse(containerRequestV2);
    }


    public List<Containers> getContainersFromIds(List<Long> containerIds) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", containerIds, "IN");
        Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
        Page<Containers> oldContainers = containerDao.findAll(pair.getLeft(), pair.getRight());
        return oldContainers.getContent();
    }

    public List<ContainerRequestV2> convertEntityToSyncDto(List<Containers> containers, Page<ShipmentsContainersMapping> shipmentsContainersMappingPageable) {
        List<ContainerRequestV2> response = new ArrayList<>();
        if(containers != null && containers.size() > 0) {
            for (Containers item: containers) {
                ContainerRequestV2 p = syncEntityConversionService.containerV2ToV1(item);
                if(item.getConsolidationId() != null) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(item.getConsolidationId());
                    consolidationDetails.ifPresent(details -> p.setConsolidationGuid(details.getGuid()));
                }
                response.add(p);
            }
            mapShipmentGuids(containers, response, shipmentsContainersMappingPageable);
        }
        return response;
    }

    private void mapShipmentGuids(List<Containers> containersList, List<ContainerRequestV2> containerRequestV2List, Page<ShipmentsContainersMapping> shipmentsContainersMappingPageable) {
        Map<Long, List<Long>> contShipIdsMap = new HashMap<>();
        List<Long> shipmentIds = new ArrayList<>();
        if(shipmentsContainersMappingPageable != null && !shipmentsContainersMappingPageable.isEmpty()) {
            contShipIdsMap = shipmentsContainersMappingPageable.getContent().stream()
                    .collect(Collectors.groupingBy(
                            ShipmentsContainersMapping::getContainerId,
                            Collectors.mapping(ShipmentsContainersMapping::getShipmentId, Collectors.toList())
                    ));
            shipmentIds = shipmentsContainersMappingPageable.getContent().stream().map(ShipmentsContainersMapping::getShipmentId).toList();
        }
        Map<Long, UUID> shipmentIdGuidMap = processShipmentIds(shipmentIds);
        Map<UUID, Long> contGuidIdMap = containersList.stream().collect(Collectors.toMap(Containers::getGuid, Containers::getId));
        if(containerRequestV2List != null && containerRequestV2List.size() > 0) {
            for (ContainerRequestV2 containerRequestV2 : containerRequestV2List) {
                Long contId = contGuidIdMap.get(containerRequestV2.getGuid());
                if(contShipIdsMap.containsKey(contId))
                    shipmentIds = contShipIdsMap.get(contId);
                else
                    shipmentIds = new ArrayList<>();
                if(shipmentIds == null || shipmentIds.size() == 0)
                    containerRequestV2.setShipmentGuids(new ArrayList<>());
                else {
                    List<UUID> shipmentGuids = shipmentIds.stream()
                            .map(shipmentIdGuidMap::get).toList();
                    containerRequestV2.setShipmentGuids(shipmentGuids);
                }
            }
        }
    }

    private Map<Long, UUID> processShipmentIds(List<Long> shipmentIds) {
        Map<Long, UUID> shipmentIdGuidMap = new HashMap<>();
        if(shipmentIds.size() > 0) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("id", shipmentIds, "IN");
            Pair<Specification<ShipmentDetails>, Pageable> pair2 = fetchData(listCommonRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipmentDetailsPage = shipmentDao.findAll(pair2.getLeft(), pair2.getRight());
            if(shipmentDetailsPage != null && !shipmentDetailsPage.isEmpty()) {
                shipmentIdGuidMap = shipmentDetailsPage.stream().collect(Collectors.toMap(ShipmentDetails::getId, ShipmentDetails::getGuid));
            }
        }
        return shipmentIdGuidMap;
    }
}
