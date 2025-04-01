package com.dpw.runner.shipment.services.masterdata.factory;

import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import com.dpw.runner.shipment.services.masterdata.helper.impl.mapper.MapperMasterDataImpl;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

@Component
public class MasterDataFactory {

    @Autowired
    private MapperMasterDataImpl mapperMasterData;

    @Autowired
    private V1MasterDataImpl v1MasterData;

    @Value("${masterdata-service.source}")
    private String source;

    public IMasterDataService getMasterDataService() {
        if(StringUtility.isEmpty(source))
            return mapperMasterData;  // TODO- throw exception?
        switch (source)
        {
            case MAPPER_MASTER_DATA:
                return mapperMasterData;
            case V1_MASTER_DATA:
                return v1MasterData;
            default:
                return mapperMasterData;  // TODO- throw exception?
        }
    }

    public IMasterDataService getMDMServiceBean(){
        return mapperMasterData;
    }


}
