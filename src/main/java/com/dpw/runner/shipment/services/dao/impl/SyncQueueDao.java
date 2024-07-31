package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ISyncQueueDao;
import com.dpw.runner.shipment.services.commons.entity.SyncQueue;
import com.dpw.runner.shipment.services.repository.interfaces.ISyncQueueRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public class SyncQueueDao implements ISyncQueueDao {

    @Autowired
    private ISyncQueueRepository syncQueueRepository;

    @Override
    public List<SyncQueue> fetchDataByModuleTypeAndTenantIds(String moduleType, List<Integer> tenantIds) {
        return syncQueueRepository.fetchByTenantIdsAndModuleType(tenantIds, moduleType);
    }

    @Override
    public List<SyncQueue> fetchDataByTenantIds(List<Integer> tenantIds) {
        return syncQueueRepository.fetchByTenantIds(tenantIds);
    }

    @Override
    public SyncQueue save(SyncQueue syncQueue) {
        syncQueueRepository.updateExistingDataInActive(syncQueue.getModuleType(), syncQueue.getModuleId(), syncQueue.getSyncTenantId());
        return syncQueueRepository.save(syncQueue);
    }

    @Override
    public void updateDataInActive(List<Long> ids) {
        syncQueueRepository.updateDataInActiveByIds(ids);
    }
}