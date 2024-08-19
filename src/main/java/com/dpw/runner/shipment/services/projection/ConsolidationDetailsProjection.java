package com.dpw.runner.shipment.services.projection;

public interface ConsolidationDetailsProjection {
    Integer getTenantId();
    String getConsolidationNumber();
    String getMawb();
    String getBol();
}