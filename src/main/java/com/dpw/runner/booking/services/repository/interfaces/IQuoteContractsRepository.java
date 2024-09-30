package com.dpw.runner.booking.services.repository.interfaces;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.booking.services.entity.QuoteContracts;
import com.dpw.runner.booking.services.utils.Generated;

import java.util.List;

@Generated
public interface IQuoteContractsRepository extends MultiTenancyRepository<QuoteContracts> {
    List<QuoteContracts> findByContractId (String contractId);
}
