package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.HistoryDetailMeta;

import java.util.List;

public interface IHistoryDetailMetaDao {
    List<HistoryDetailMeta> saveAll(List<HistoryDetailMeta> historyDetailMetaList);
}
