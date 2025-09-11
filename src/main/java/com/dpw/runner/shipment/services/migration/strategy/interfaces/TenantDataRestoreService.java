package com.dpw.runner.shipment.services.migration.strategy.interfaces;

import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import lombok.Generated;
import org.springframework.http.ResponseEntity;

import java.util.Map;

@Generated
public interface TenantDataRestoreService {
    Map<String, Object> restoreTenantData(Integer tenantId, Integer count);
    ResponseEntity<String> restoreTenantDataAsync(Integer tenantId, Integer count) throws RunnerException;
}
