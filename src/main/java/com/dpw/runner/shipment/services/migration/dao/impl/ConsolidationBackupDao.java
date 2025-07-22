package com.dpw.runner.shipment.services.migration.dao.impl;

import com.dpw.runner.shipment.services.migration.dao.interfaces.IConsolidationBackupDao;
import com.dpw.runner.shipment.services.migration.entity.ConsolidationBackupEntity;
import com.dpw.runner.shipment.services.migration.repository.IConsolidationBackupRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.*;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("java:S3776")
@Repository
@Slf4j
public class ConsolidationBackupDao implements IConsolidationBackupDao {

    @Autowired
    private IConsolidationBackupRepository consolidationBackupRepository;

    @Override
    public List<Long> findConsolidationIdsByTenantId(Integer tenantId) {
        return consolidationBackupRepository.findConsolidationIdsByTenantId(tenantId);
    }

    @Override
    public void makeIsDeleteTrueToMarkRestoreSuccessful(Long id) {
        consolidationBackupRepository.makeIsDeleteTrueToMarkRestoreSuccessful(id);
    }

    public ConsolidationBackupEntity findConsolidationsById(Long consolidationId) {
        return consolidationBackupRepository.findConsolidationsById(consolidationId);
    }
}
