package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.helpers.DbAccessHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class PackingsSync implements IPackingsSync {

    @Autowired
    SyncEntityConversionService syncEntityConversionService;

    @Autowired
    @Lazy
    private IContainerDao containerDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ISyncService syncService;

    @Override
    public ResponseEntity<?> sync(List<Packing> packingList, String transactionId) {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return ResponseHelper.buildSuccessResponse();

        List<PackingRequestV2> packingRequestV2List = new ArrayList<>();
        Set<Containers> containers = new HashSet<>();
        if (packingList != null && packingList.size() > 0) {
            List<Long> containerIds = packingList.stream().map(Packing::getContainerId).filter(Objects::nonNull).toList();
            if (containerIds.size() > 0) {
                ListCommonRequest listCommonRequest = CommonUtils.constructListCommonRequest("id", containerIds, "IN");
                Pair<Specification<Containers>, Pageable> pair = DbAccessHelper.fetchData(listCommonRequest, Containers.class);
                Page<Containers> containersPage = containerDao.findAll(pair.getLeft(), pair.getRight());
                if (containersPage != null && !containersPage.isEmpty()) {
                    containers = new HashSet<>(containersPage.getContent());
                }
            }

            packingRequestV2List = syncEntityConversionService.packingsV2ToV1(packingList, new ArrayList<>(containers), null, null);

            String json = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(packingRequestV2List).module(SyncingConstants.PACKINGS).build());

            String idsString = packingList.stream()
                    .map(Packing::getId)
                    .map(String::valueOf)
                    .collect(Collectors.joining(","));

            String guidsString = packingList.stream()
                    .map(Packing::getGuid)
                    .map(UUID::toString)
                    .collect(Collectors.joining(","));

            syncService.pushToKafka(json, idsString, guidsString, "Packings", transactionId);
            return ResponseHelper.buildSuccessResponse(packingRequestV2List);
        } else {
            return null;
        }
    }


}
