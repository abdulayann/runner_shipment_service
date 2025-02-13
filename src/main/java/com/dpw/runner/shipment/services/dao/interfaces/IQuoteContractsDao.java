package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.QuoteContracts;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface IQuoteContractsDao {

    QuoteContracts save(QuoteContracts quoteContracts);

    List<QuoteContracts> findByContractId(String contractId);

    Page<QuoteContracts> findAll(Specification<QuoteContracts> spec, Pageable pageable);
}
