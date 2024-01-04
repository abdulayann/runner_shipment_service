package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.DbAccessHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@Slf4j
public class PackingsSync implements IPackingsSync {

    @Autowired
    SyncEntityConversionService syncEntityConversionService;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private EmailServiceUtility emailServiceUtility;

    @Autowired
    private IV1Service v1Service;

    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(3)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    @Override
    public ResponseEntity<?> sync(List<Packing> packingList) {
        List<PackingRequestV2> packingRequestV2List = new ArrayList<>();
        List<Containers> containers = new ArrayList<>();
        if(packingList != null && packingList.size() > 0) {
            List<Long> containerIds = packingList.stream().map(Packing::getContainerId).filter(Objects::nonNull).toList();
            if(containerIds.size() > 0) {
                ListCommonRequest listCommonRequest = CommonUtils.constructListCommonRequest("id", containerIds, "IN");
                Pair<Specification<Containers>, Pageable> pair = DbAccessHelper.fetchData(listCommonRequest, Containers.class);
                Page<Containers> containersPage = containerDao.findAll(pair.getLeft(), pair.getRight());
                if(containersPage != null && !containersPage.isEmpty()) {
                    containers = containersPage.getContent();
                }
            }

            packingRequestV2List = syncEntityConversionService.packingsV2ToV1(packingList, containers, null, null);

            String json = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(packingRequestV2List).module(SyncingConstants.PACKINGS).build());

            String idsString = packingList.stream()
                    .map(Packing::getId)
                    .map(String::valueOf)
                    .collect(Collectors.joining(","));

            String guidsString = packingList.stream()
                    .map(Packing::getGuid)
                    .map(UUID::toString)
                    .collect(Collectors.joining(","));

            callSync(json, idsString, guidsString);
            return ResponseHelper.buildSuccessResponse(packingRequestV2List);
        }
        else {
            return null;
        }
    }

    @Async
    private void callSync(String json, String ids, String guids) {
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if (ctx.getLastThrowable() != null) {
                log.error("V1 error -> {}", ctx.getLastThrowable().getMessage());
            }

            V1DataSyncResponse response_ = v1Service.v1DataSync(json);
            if (!response_.getIsSuccess()) {
                try {
                    emailServiceUtility.sendEmailForSyncEntity(ids, guids,
                            "Packings", response_.getError().toString());
                } catch (Exception ex) {
                    log.error("Not able to send email for sync failure for Containers: " + ex.getMessage());
                }
            }
            return ResponseHelper.buildSuccessResponse(response_);
        });
    }

}
