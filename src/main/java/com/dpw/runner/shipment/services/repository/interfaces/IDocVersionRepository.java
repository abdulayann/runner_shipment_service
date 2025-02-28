package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.DocVersion;
import com.dpw.runner.shipment.services.entity.enums.DocVersionTypes;

import java.util.List;

public interface IDocVersionRepository extends MultiTenancyRepository<DocVersion> {

    List<DocVersion> findByEntityIdAndType(Long entityId, DocVersionTypes type);
}
