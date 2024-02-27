package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbStocksRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
@Slf4j
public class MawbStocksDao implements IMawbStocksDao {
    @Autowired
    private IMawbStocksRepository mawbStocksRepository;

    @Override
    public MawbStocks save(MawbStocks mawbStocks) {
        return mawbStocksRepository.save(mawbStocks);
    }

    @Override
    public Page<MawbStocks> findAll(Specification<MawbStocks> spec, Pageable pageable) {
        return mawbStocksRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<MawbStocks> findById(Long id) {
        return mawbStocksRepository.findById(id);
    }

    @Override
    public void delete(MawbStocks mawbStocks) {
        mawbStocksRepository.delete(mawbStocks);
    }

    @Override
    public Optional<MawbStocks> findByGuid(UUID guid) {
        return mawbStocksRepository.findByGuid(guid);
    }
}
