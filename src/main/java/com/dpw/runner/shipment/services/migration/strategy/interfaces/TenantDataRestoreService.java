package com.dpw.runner.shipment.services.migration.strategy.interfaces;

import lombok.Generated;
import org.springframework.http.ResponseEntity;

@Generated
public interface TenantDataRestoreService {
    void restoreTenantData(Integer tenantId, Integer count);
    ResponseEntity<String> restoreTenantDataAsync(Integer tenantId, Integer count);
}
