package com.dpw.runner.shipment.services.repository.interfaces;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.SequenceIncrementor;
import com.dpw.runner.shipment.services.utils.Generated;

@Generated
public interface ISequenceIncrementorRepository extends MultiTenancyRepository<SequenceIncrementor> {
    SequenceIncrementor save(SequenceIncrementor request);

    void delete(SequenceIncrementor request);
}
