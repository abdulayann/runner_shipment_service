package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.IntegrationResponse;
import com.dpw.runner.shipment.services.utils.Generated;

import java.util.List;
import java.util.Optional;

@Generated
public interface IIntegrationRespsonseRepository extends MultiTenancyRepository<IntegrationResponse> {

    Optional<List<IntegrationResponse>> findByEntityIdAndEntityType(Long entityId, String entityType);
}
