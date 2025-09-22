package com.dpw.runner.shipment.services.migration.strategy.interfaces;

import lombok.Generated;

import java.util.Map;

@Generated
public interface RestoreServiceHandler {
    Map<String, Object> restore(Integer tenantId);
}
