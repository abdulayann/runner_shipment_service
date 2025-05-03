package com.dpw.runner.shipment.services.projection;

public interface PackingAssignmentProjection {
    Long getAssignedCount();

    Long getUnassignedCount();
}
