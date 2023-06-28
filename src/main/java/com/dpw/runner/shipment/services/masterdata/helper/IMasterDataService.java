package com.dpw.runner.shipment.services.masterdata.helper;

import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;

import java.util.List;

public interface IMasterDataService {

    List<MasterData> fetchByType(MasterDataType masterDataType);
}
