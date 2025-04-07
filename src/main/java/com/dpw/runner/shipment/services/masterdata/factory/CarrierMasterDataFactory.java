package com.dpw.runner.shipment.services.masterdata.factory;

import com.dpw.runner.shipment.services.masterdata.helper.ICarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.helper.impl.mapper.CarrierMasterDataImpl;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import static com.dpw.runner.shipment.services.commons.constants.Constants.MAPPER_MASTER_DATA;

public class CarrierMasterDataFactory {
    @Autowired
    private CarrierMasterDataImpl carrierMasterData;

    @Value("${masterdata-service.source}")
    private String source;

    public ICarrierMasterData getCarrierMasterDataService() {
        return carrierMasterData;  // LATER - throw exception?
    }
}
