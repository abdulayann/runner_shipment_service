package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.HblReleaseTypeMapping;
import com.dpw.runner.shipment.services.utils.Generated;

import java.util.List;
import java.util.Optional;

@Generated
public interface IHblReleaseTypeMappingRepository extends MultiTenancyRepository<HblReleaseTypeMapping> {

    Optional<List<HblReleaseTypeMapping>> findByHblIdAndReleaseType(Long hblId, String releaseType);
}
