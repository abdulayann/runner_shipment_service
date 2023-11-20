package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

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

    @Value("${v1service.url.base}${v1service.url.containersSync}")
    private String CONTAINER_V1_SYNC_URL;
    @Autowired
    private IV1Service v1Service;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Override
    @Async
    public ResponseEntity<?> sync(List<Long> containerIds) {
        List<Containers> containers = getContainersFromIds(containerIds);
        List<ContainerRequestV2> containerRequestV2 = convertEntityToSyncDto(containers);
        String json = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(containerRequestV2).module(SyncingConstants.CONTAINERS).build());
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if(ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}",ctx.getLastThrowable().getMessage());
            }
            V1DataSyncResponse response_ = v1Service.v1DataSync(json);
            return ResponseHelper.buildSuccessResponse(response_);
        });

        return ResponseHelper.buildSuccessResponse(containerRequestV2);
    }

    public List<Containers> getContainersFromIds(List<Long> containerIds) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", containerIds, "IN");
        Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
        Page<Containers> oldContainers = containerDao.findAll(pair.getLeft(), pair.getRight());
        return oldContainers.getContent();
    }

    public List<ContainerRequestV2> convertEntityToSyncDto(List<Containers> containers) {
        List<ContainerRequestV2> response = new ArrayList<>();
        if(containers != null && containers.size() > 0) {
            for (Containers item: containers) {
                ContainerRequestV2 p = modelMapper.map(item, ContainerRequestV2.class);
                List<UUID> shipmentGuids = new ArrayList<>();
                if(item.getShipmentsList() != null && item.getShipmentsList().size() > 0) {
                    for (var x : item.getShipmentsList()) {
                        shipmentGuids.add(x.getGuid());
                    }
                }
                p.setShipmentGuids(shipmentGuids);
                if(item.getConsolidationId() != null) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(item.getConsolidationId());
                    consolidationDetails.ifPresent(details -> p.setConsolidationGuid(details.getGuid()));
                }
                response.add(p);
            }
        }
        return response;
    }
}
