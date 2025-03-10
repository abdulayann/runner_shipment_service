package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.DocDetails;
import com.dpw.runner.shipment.services.entity.enums.DocDetailsTypes;

import java.util.List;

public interface IDocDetailsRepository extends MultiTenancyRepository<DocDetails> {

    List<DocDetails> findByEntityIdAndType(Long entityId, DocDetailsTypes type);
}
