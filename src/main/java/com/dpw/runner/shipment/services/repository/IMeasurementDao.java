package com.dpw.runner.shipment.services.repository;

import com.dpw.runner.shipment.services.entity.MeasurementDetails;
import com.dpw.runner.shipment.services.filter.Multitenancy.MultiTenancyRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface IMeasurementDao extends MultiTenancyRepository<MeasurementDetails> {



}
