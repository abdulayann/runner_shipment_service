package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.syncing.Entity.CustomConsolidationRequest;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;

public class ConsolidationSync implements IConsolidationSync {

    @Override
    public CustomConsolidationRequest sync(ConsolidationDetails consolidationDetails) {
        CustomConsolidationRequest cr = new CustomConsolidationRequest();

        // assigning root level properties

        // assigning child entities

        return null;
    }
}
