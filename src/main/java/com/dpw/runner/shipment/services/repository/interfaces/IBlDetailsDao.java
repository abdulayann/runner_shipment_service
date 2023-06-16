package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.BlDetails;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface IBlDetailsDao extends MultiTenancyRepository<BlDetails> {



}
