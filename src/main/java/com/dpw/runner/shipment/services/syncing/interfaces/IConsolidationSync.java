package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;

public interface IConsolidationSync {
    CustomConsolidationRequest sync(ConsolidationDetails consolidationDetails);
}
