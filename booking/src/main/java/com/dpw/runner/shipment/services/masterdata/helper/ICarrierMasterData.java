package com.dpw.runner.shipment.services.masterdata.helper;

import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;

import java.util.List;

public interface ICarrierMasterData {
    List<CarrierMasterData> fetchCarrierData();

}
