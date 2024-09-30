package com.dpw.runner.booking.services.masterdata.helper.impl.mapper;

import com.dpw.runner.booking.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.booking.services.masterdata.helper.ICarrierMasterData;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;

@Service
public class CarrierMasterDataImpl implements ICarrierMasterData {
    private List<CarrierMasterData> carrierMasterData;

    @PostConstruct
    private void setCarrierMasterData() {
        carrierMasterData = new ArrayList<>();
        carrierMasterData.add(CarrierMasterData.builder().itemValue("COSU").build());
        carrierMasterData.add(CarrierMasterData.builder().itemValue("MSCU").build());
    }

    @Override
    public List<CarrierMasterData> fetchCarrierData() {
        return carrierMasterData;
    }
}
