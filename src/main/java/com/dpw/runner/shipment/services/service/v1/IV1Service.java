package com.dpw.runner.shipment.services.service.v1;

import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.v1.request.CreateConsolidationTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.response.SendEntityResponse;
import com.dpw.runner.shipment.services.dto.v1.response.TenantIdResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import org.springframework.http.ResponseEntity;

public interface IV1Service {
    ResponseEntity<?> createBooking(CustomerBooking customerBooking);
    ResponseEntity<?> updateOrgCreditLimitFromBooking(CheckCreditLimitResponse request);

    V1DataResponse fetchMasterData(Object request);

    V1DataResponse createMasterData(Object request);

    V1DataResponse updateMasterData(Object request);

    V1DataResponse fetchCarrierMasterData(Object request, boolean isListOnly);

    V1DataResponse createCarrierMasterData(Object request);

    V1DataResponse updateCarrierMasterData(Object request);

    V1DataResponse fetchOrganization(Object request);

    V1DataResponse fetchUnlocation(Object request);

    V1DataResponse fetchContainerTypeData(Object request);

    V1DataResponse createContainerTypeData(Object request);

    V1DataResponse updateContainerTypeData(Object request);

    V1DataResponse fetchVesselData(Object request);

    V1DataResponse createVesselData(Object request);

    V1DataResponse updateVesselData(Object request);

    V1DataResponse fetchRoutingMasterData(Object request);

    V1DataResponse createRoutingMasterData(Object request);

    V1DataResponse updateRoutingMasterData(Object request);

    V1DataResponse fetchCurrenciesData(Object request);

    V1DataResponse createCurrenciesData(Object request);

    V1DataResponse updateCurrenciesData(Object request);

    V1DataResponse fetchDangerousGoodData(Object request);

    V1DataResponse createDangerousGoodData(Object request);

    V1DataResponse updateDangerousGoodData(Object request);

    V1DataResponse fetchWarehouseData(Object request);

    V1DataResponse createWarehouseData(Object request);

    V1DataResponse updateWarehouseData(Object request);

    V1DataResponse fetchPortsData(Object request);

    V1DataResponse createPortsData(Object request);

    V1DataResponse updatePortsData(Object request);

    V1DataResponse fetchCommodityData(Object request);

    V1DataResponse createCommodityData(Object request);

    V1DataResponse updateCommodityData(Object request);

    V1DataResponse fetchSalesAgentData(Object request);

    V1DataResponse createSalesAgentData(Object request);

    V1DataResponse updateSalesAgentData(Object request);

    V1DataResponse createOrganizationData(Object request);

    V1DataResponse updateOrganizationData(Object request);

    V1DataResponse createUnlocationData(Object request);

    V1DataResponse updateUnlocationData(Object request);

    V1DataResponse fetchMultipleMasterData(Object request);

    V1DataResponse fetchUsersData(Object request);

    V1DataResponse fetchGridColorCodeData(Object request);

    V1DataResponse createGridColorCodeData(Object request);

    V1DataResponse updateGridColorCodeData(Object request);

    V1DataResponse listCousinBranches(Object request);

    V1DataResponse listCousinBranchesWithoutCurrent(Object request);

    TenantIdResponse tenantByGuid(Object request);

    V1DataResponse importFlightSchedules(Object request);

    V1DataResponse fetchFlightStatus(Object request);

    V1DataResponse importSailingSchedules(Object request);

    V1DataResponse listSailingSchedule(Object request);

    SendEntityResponse sendConsolidationTask(CreateConsolidationTaskRequest request);

    SendEntityResponse sendShipmentTask(CreateShipmentTaskRequest request);

    V1DataResponse addressList(Object request);

    V1DataResponse tenantNameByTenantId(Object request);
    V1DataResponse fetchChargeCodeData(Object request);
    V1RetrieveResponse retrieveTenantSettings();

}
