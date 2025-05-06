package com.dpw.runner.shipment.services.masterdata.factory;

import com.dpw.runner.shipment.services.masterdata.helper.ICarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.helper.impl.mapper.CarrierMasterDataImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

public class CarrierMasterDataFactory {
    @Autowired
    private CarrierMasterDataImpl carrierMasterData;

    @Value("${masterdata-service.source}")
    private String source;

    public ICarrierMasterData getCarrierMasterDataService() {
        return carrierMasterData;  // LATER - throw exception?
    }
}
