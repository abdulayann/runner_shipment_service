package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.SyncQueue;

import java.util.List;

public interface ISyncQueueDao {
    List<SyncQueue> fetchDataByModuleTypeAndTenantIds(String moduleType, List<Integer> tenantIds);

    List<SyncQueue> fetchDataByTenantIds(List<Integer> tenantIds);

    SyncQueue save(SyncQueue syncQueue);

    void updateDataInActive(List<Long> ids);

}
