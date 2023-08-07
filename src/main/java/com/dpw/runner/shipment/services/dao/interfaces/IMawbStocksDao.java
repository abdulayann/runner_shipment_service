package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.MawbStocks;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IMawbStocksDao {
    MawbStocks save(MawbStocks mawbStocks);
    Page<MawbStocks> findAll(Specification<MawbStocks> spec, Pageable pageable);
    Optional<MawbStocks> findById(Long id);
    void delete(MawbStocks carrierDetails);
}
