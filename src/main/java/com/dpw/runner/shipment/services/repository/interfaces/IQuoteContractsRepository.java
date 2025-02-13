package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.QuoteContracts;
import com.dpw.runner.shipment.services.utils.Generated;

import java.util.List;

@Generated
public interface IQuoteContractsRepository extends MultiTenancyRepository<QuoteContracts> {
    List<QuoteContracts> findByContractId(String contractId);
}
