package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.AttachListShipmentResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BILLING_DATA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTAINERS_LIST;

@Component
@Slf4j
public class ShipmentMasterDataHelperV3 {

    @Value("${include.master.data}")
    private Boolean includeMasterData;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    public void getMasterDataForList(List<ShipmentDetails> lst, List<IRunnerResponse> responseList, boolean getMasterData, boolean includeTenantData, Set<String> includeColumns) {
        if(getMasterData || Boolean.TRUE.equals(includeMasterData)) {
            try {
                double _start = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setLocationData(responseList, EntityTransferConstants.LOCATION_SERVICE_GUID)), executorServiceMasterData);
                CompletableFuture<Void> containerDataFuture = CompletableFuture.completedFuture(null);
                if(includeColumns.contains(CONTAINERS_LIST))
                    containerDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.setContainerTeuData(lst, responseList)), executorServiceMasterData);
                CompletableFuture<Void> billDataFuture = CompletableFuture.completedFuture(null);
                if(includeColumns.contains(BILLING_DATA) && responseList!=null && !(responseList.get(0) instanceof AttachListShipmentResponse)){
                    billDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchBillDataForShipments(lst, responseList)), executorServiceMasterData);
                }
                var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchVesselForList(responseList)), executorServiceMasterData);
                CompletableFuture<Void> tenantDataFuture = CompletableFuture.completedFuture(null);
                if (Boolean.TRUE.equals(includeTenantData))
                    tenantDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> masterDataUtils.fetchTenantIdForList(responseList)), executorServiceMasterData);
                CompletableFuture.allOf(locationDataFuture, containerDataFuture, billDataFuture, vesselDataFuture, tenantDataFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.SHIPMENT_LIST_MASTER_DATA, (System.currentTimeMillis() - _start) , LoggerHelper.getRequestIdFromMDC());
            }
            catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_SHIPMENT_LIST, ex.getLocalizedMessage());
            }
        }
    }
}
