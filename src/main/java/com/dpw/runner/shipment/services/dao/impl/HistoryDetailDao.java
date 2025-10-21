package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IHistoryDetailDao;
import com.dpw.runner.shipment.services.entity.HistoryDetail;
import com.dpw.runner.shipment.services.repository.interfaces.IHistoryDetailRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@Slf4j
public class HistoryDetailDao implements IHistoryDetailDao {
    @Autowired
    private IHistoryDetailRepository historyDetailRepository;

    @Override
    public Optional<HistoryDetail> findByHistoryId(Long id) {
        return historyDetailRepository.findById(id);
    }

    @Override
    public HistoryDetail save(HistoryDetail historyDetail) {
        return historyDetailRepository.save(historyDetail);
    }

}
