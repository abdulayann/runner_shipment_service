package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
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
        if(getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorServiceMasterData);
                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorServiceMasterData);
                var carrierDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchCarriersForList(responseList)), executorServiceMasterData);
                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(includeTenantData))
                    tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorServiceMasterData);

                CompletableFuture.allOf(locationDataFuture, vesselDataFuture, tenantDataFuture, carrierDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());
            }
            catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }

}
