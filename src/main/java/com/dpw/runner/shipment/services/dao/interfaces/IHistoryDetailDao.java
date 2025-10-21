package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.HistoryDetail;

import java.util.Optional;

public interface IHistoryDetailDao {
    Optional<HistoryDetail> findByHistoryId(Long id);
    HistoryDetail save(HistoryDetail historyDetail);
}
