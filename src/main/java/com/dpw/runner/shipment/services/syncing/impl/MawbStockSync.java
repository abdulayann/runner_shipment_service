package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.MawbStocksV2;
import com.dpw.runner.shipment.services.syncing.Entity.V1DataSyncRequest;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IMawbStockSync;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class MawbStockSync implements IMawbStockSync {

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    ISyncService syncService;

    @Autowired
    SyncEntityConversionService syncEntityConversionService;

    @Override
    public void sync(MawbStocks mawbStocks) {
        if (!Boolean.TRUE.equals(SyncingContext.getContext()))
            return;
        MawbStocksV2 mawbStocksV2 = syncEntityConversionService.mawbStocksV2ToV1(mawbStocks);

        String transactionId = String.valueOf(mawbStocks.getGuid());
        String finalJson = jsonHelper.convertToJson(V1DataSyncRequest.builder().entity(mawbStocksV2).module(SyncingConstants.MAWB_STOCKS).build());
        syncService.pushToKafka(finalJson, String.valueOf(mawbStocks.getId()), String.valueOf(mawbStocks.getGuid()), SyncingConstants.MAWB_STOCKS, transactionId);
    }
}
