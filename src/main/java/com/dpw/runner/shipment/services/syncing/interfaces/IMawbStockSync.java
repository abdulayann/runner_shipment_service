package com.dpw.runner.shipment.services.syncing.interfaces;

import com.dpw.runner.shipment.services.commons.entity.MawbStocks;

public interface IMawbStockSync {
    void sync(MawbStocks mawbStocks);
}
