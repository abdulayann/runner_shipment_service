package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

public interface IMawbStocksLinkDao {
    MawbStocksLink save(MawbStocksLink mawbStocksLink);
    Page<MawbStocksLink> findAll(Specification<MawbStocksLink> spec, Pageable pageable);

}
