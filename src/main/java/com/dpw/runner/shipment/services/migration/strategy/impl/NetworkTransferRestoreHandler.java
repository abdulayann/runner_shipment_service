package com.dpw.runner.shipment.services.migration.strategy.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.migration.HelperExecutor;
import com.dpw.runner.shipment.services.migration.entity.NetworkTransferBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.INetworkTransferBackupRepository;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreServiceHandler;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.repository.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Generated;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
@Generated
@SuppressWarnings({"java:S4144", "java:S1192"})
public class NetworkTransferRestoreHandler implements RestoreServiceHandler {

    private final INetworkTransferBackupRepository backupRepository;
    private final INetworkTransferRepository networkTransferDao;
    private final ObjectMapper objectMapper;
    private final V1ServiceImpl v1Service;
    @Autowired
    private MigrationUtil migrationUtil;

    @Autowired
    private HelperExecutor trxExecutor;


    @Override
    public Map<String, Object> restore(Integer tenantId) {
        log.info("Started network Transfer restore for tenant: {}", tenantId);
        List<NetworkTransferBackupEntity> networkTransferBackupEntities = backupRepository.findNetworkTransferIdsByTenantId(tenantId);
        Set<Long> allBackupNetworkTransferIds = networkTransferBackupEntities.stream().map(NetworkTransferBackupEntity::getNetworkTransferId)
                .collect(Collectors.toSet());
        log.info("Count of networkTransfer ids : {}", allBackupNetworkTransferIds.size());
        Map<String, Object> map = new HashMap<>();
        map.put("Total NetworkTransfer :", allBackupNetworkTransferIds.size());
        if (allBackupNetworkTransferIds.isEmpty()) {
            return map;
        }
        try {
            trxExecutor.runInTrx(() -> {
                v1Service.setAuthContext();
                TenantContext.setCurrentTenant(tenantId);
                UserContext.getUser().setPermissions(new HashMap<>());
                networkTransferDao.deleteAllByTenantId(tenantId);
                List<NetworkTransfer> networkTransfers = networkTransferBackupEntities.stream()
                        .filter(ids -> !ids.getIsDeleted())
                        .map(backupEntity -> {
                            try {
                                NetworkTransfer networkTransfer = objectMapper.readValue(backupEntity.getNetworkTransferDetails(), NetworkTransfer.class);
                                networkTransfer.setId(null);// Reset ID for new entity
                                backupRepository.makeIsDeleteTrueToMarkRestoreSuccessful(backupEntity.getId());
                                return networkTransfer;
                            } catch (JsonProcessingException e) {
                                log.error("Error processing JSON for Network Transfer: {}", e.getMessage(), e);
                                return null;
                            }
                        })
                        .filter(Objects::nonNull)
                        .toList();
                networkTransferDao.saveAll(networkTransfers);
                return null;
            });
        } catch (Exception e) {
            log.error("Network Transfer migration failed for tenant Id [id={}]: {}", tenantId, e.getMessage(), e);
            migrationUtil.saveErrorResponse(Long.valueOf(tenantId), Constants.NETWORK_TRANSFER,
                    IntegrationType.RESTORE_DATA_SYNC, Status.FAILED, Arrays.toString(e.getStackTrace()));
            map.put("NetworkTransfer Failed Reason", e.getMessage());
            throw new IllegalArgumentException(e);
        } finally {
            v1Service.clearAuthContext();
        }
        map.put("Total NetworkTransfer restored :", allBackupNetworkTransferIds.size());
        log.info("Completed network Transfer restore for tenant: {}", tenantId);
        return map;
    }
}



