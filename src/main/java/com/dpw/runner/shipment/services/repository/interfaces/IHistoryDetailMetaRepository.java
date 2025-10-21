package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository;
import com.dpw.runner.shipment.services.entity.HistoryDetailMeta;

public interface IHistoryDetailMetaRepository extends MultiTenancyRepository<HistoryDetailMeta> {
}
