package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IHistoryDetailMetaDao;
import com.dpw.runner.shipment.services.entity.HistoryDetailMeta;
import com.dpw.runner.shipment.services.repository.interfaces.IHistoryDetailMetaRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@Slf4j
public class HistoryDetailMetaDao implements IHistoryDetailMetaDao {
    @Autowired
    private IHistoryDetailMetaRepository historyDetailMetaRepository;

    @Override
    public List<HistoryDetailMeta> saveAll(List<HistoryDetailMeta> historyDetailMetaList) {
        return historyDetailMetaRepository.saveAll(historyDetailMetaList);
    }
}
