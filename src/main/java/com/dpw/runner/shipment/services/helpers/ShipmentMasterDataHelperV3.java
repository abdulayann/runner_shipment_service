package com.dpw.runner.shipment.services.helpers;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BILLING_DATA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTAINERS_LIST;
import static com.dpw.runner.shipment.services.commons.constants.Constants.JOB_TYPE;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.AttachListShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentPendingNotificationResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ShipmentMasterDataHelperV3 {


    private final MasterDataUtils masterDataUtils;
    ExecutorService executorServiceMasterData;

    private final MasterDataHelper masterDataHelper;

    @Autowired
    public ShipmentMasterDataHelperV3(MasterDataUtils masterDataUtils, @Qualifier("executorServiceMasterData") ExecutorService executorServiceMasterData, MasterDataHelper masterDataHelper) {
        this.masterDataUtils = masterDataUtils;
        this.executorServiceMasterData = executorServiceMasterData;
        this.masterDataHelper = masterDataHelper;
    }

    public void getMasterDataForList(List<ShipmentDetails> lst, List<IRunnerResponse> responseList, boolean getMasterData, boolean includeTenantData, Set<String> includeColumns) {
        if(getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorServiceMasterData);
                CompletableFuture<Void> containerDataFuture = CompletableFuture.completedFuture(null);
                if(includeColumns.contains(CONTAINERS_LIST))
                    containerDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setContainerTeuData(lst, responseList)), executorServiceMasterData);

                CompletableFuture<Void> shipmentTypeMasterDataFuture = CompletableFuture.completedFuture(null);
                if (includeColumns.contains(JOB_TYPE)) {
                    shipmentTypeMasterDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setShipmentTypeMasterData(responseList)),
                            executorServiceMasterData);
                }

                CompletableFuture<Void> billDataFuture = CompletableFuture.completedFuture(null);
                if(includeColumns.contains(BILLING_DATA) && responseList!=null && !(responseList.get(0) instanceof AttachListShipmentResponse)){
                    billDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchBillDataForShipments(lst, responseList)), executorServiceMasterData);
                }
                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorServiceMasterData);
                var carrierDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchCarriersForList(responseList)), executorServiceMasterData);
                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(includeTenantData))
                    tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorServiceMasterData);
                CompletableFuture.allOf(locationDataFuture, containerDataFuture, billDataFuture, vesselDataFuture, tenantDataFuture, shipmentTypeMasterDataFuture, carrierDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());
            }
            catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }

    public void getMasterDataForEntity(ShipmentPendingNotificationResponse shipmentResponse) {
        try {
            double startTime = System.currentTimeMillis();
            Map<String, Object> masterDataResponse = new HashMap<>();
            var tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataHelper.addAllTenantDataInSingleCall(shipmentResponse, masterDataResponse)), executorServiceMasterData);
            tenantDataFuture.join();
            if(masterDataResponse.get("Tenants") != null)
                shipmentResponse.setTenantMasterData((Map<String, Object>) masterDataResponse.get("Tenants"));
            log.info("Time taken to fetch Master-data for event: {} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_GET_MASTER_DATA, (System.currentTimeMillis() - startTime) , LoggerHelper.getRequestIdFromMDC());
        }
        catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
        }
    }
}
