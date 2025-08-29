package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;

@Component
@Slf4j
public class CarrierBookingMasterDataHelper {

    private final MasterDataUtils masterDataUtils;
    ExecutorService executorServiceMasterData;

    private final MasterDataHelper masterDataHelper;

    public CarrierBookingMasterDataHelper(MasterDataUtils masterDataUtils, @Qualifier("executorServiceMasterData") ExecutorService executorServiceMasterData, MasterDataHelper masterDataHelper) {
        this.masterDataUtils = masterDataUtils;
        this.masterDataHelper = masterDataHelper;
        this.executorServiceMasterData = executorServiceMasterData;
    }

    public void getMasterDataForList(List<CarrierBooking> lst, List<IRunnerResponse> responseList, boolean getMasterData, boolean includeTenantData, Set<String> includeColumns){
        if(getMasterData){
            double startTime = System.currentTimeMillis();

        }
    }

}
