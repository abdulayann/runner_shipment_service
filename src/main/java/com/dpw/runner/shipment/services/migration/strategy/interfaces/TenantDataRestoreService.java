package com.dpw.runner.shipment.services.migration.strategy.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Generated;
import org.springframework.http.ResponseEntity;

@Generated
public interface TenantDataRestoreService {
    void restoreTenantData(Integer tenantId);
    ResponseEntity<String> restoreTenantDataAsync(Integer tenantId);
}
