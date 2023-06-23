package com.dpw.runner.shipment.services.masterdata.helper.impl.mapper;

import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class MapperMasterDataImpl implements IMasterDataService {

    Map<MasterDataType , List<MasterData>> masterDataMap;

    @PostConstruct
    public void fillMasterData() {
        masterDataMap = new HashMap<>();
        List<MasterData> dataMap = new ArrayList<>();
        dataMap.add(MasterData.builder().itemValue("SEA").build());
        dataMap.add(MasterData.builder().itemValue("AIR").build());
        dataMap.add(MasterData.builder().itemValue("ROA").build());
        dataMap.add(MasterData.builder().itemValue("RAIL").build());
        masterDataMap.put(MasterDataType.TransportModes, dataMap);
    }

    @Override
    public List<MasterData> fetchByType(MasterDataType masterDataType) {
        return masterDataMap.get(masterDataType);
    }

}
