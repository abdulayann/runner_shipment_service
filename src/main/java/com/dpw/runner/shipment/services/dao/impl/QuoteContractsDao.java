package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IQuoteContractsDao;
import com.dpw.runner.shipment.services.entity.QuoteContracts;
import com.dpw.runner.shipment.services.repository.interfaces.IQuoteContractsRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@Slf4j
public class QuoteContractsDao implements IQuoteContractsDao {

    @Autowired
    private IQuoteContractsRepository quoteContractsRepository;

    @Override
    public QuoteContracts save(QuoteContracts quoteContracts) {
        return quoteContractsRepository.save(quoteContracts);
    }

    @Override
    public List<QuoteContracts> findByContractId(String contractId) {
        return quoteContractsRepository.findByContractId(contractId);
    }
}
