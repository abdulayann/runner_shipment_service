package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbStocksLinkRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

@Repository
@Slf4j
public class MawbStocksLinkDao implements IMawbStocksLinkDao {
    @Autowired
    private IMawbStocksLinkRepository mawbStocksLinkRepository;

    @Override
    public MawbStocksLink save(MawbStocksLink mawbStocksLink) {
        return mawbStocksLinkRepository.save(mawbStocksLink);
    }

    @Override
    public Page<MawbStocksLink> findAll(Specification<MawbStocksLink> spec, Pageable pageable) {
        return mawbStocksLinkRepository.findAll(spec, pageable);
    }

}
