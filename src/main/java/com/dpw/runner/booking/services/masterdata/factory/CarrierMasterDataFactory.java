package com.dpw.runner.booking.services.masterdata.factory;

import com.dpw.runner.booking.services.masterdata.helper.ICarrierMasterData;
import com.dpw.runner.booking.services.masterdata.helper.impl.mapper.CarrierMasterDataImpl;
import com.dpw.runner.booking.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import static com.dpw.runner.booking.services.commons.constants.Constants.MAPPER_MASTER_DATA;

public class CarrierMasterDataFactory {
    @Autowired
    private CarrierMasterDataImpl carrierMasterData;

    @Value("${masterdata-service.source}")
    private String source;

    public ICarrierMasterData getCarrierMasterDataService() {
        if(StringUtility.isEmpty(source))
            return carrierMasterData;  // TODO- throw exception?
        switch (source)
        {
            case MAPPER_MASTER_DATA:
                return carrierMasterData;
            default:
                return carrierMasterData;  // TODO- throw exception?
        }
    }
}
