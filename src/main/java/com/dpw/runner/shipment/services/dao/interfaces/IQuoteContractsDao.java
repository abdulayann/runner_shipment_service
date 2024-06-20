package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.QuoteContracts;

import java.util.List;

public interface IQuoteContractsDao {

    QuoteContracts save(QuoteContracts quoteContracts);
    List<QuoteContracts> findByContractId(String contractId);
}
