package com.dpw.runner.shipment.services.masterdata.helper;

import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dto.v1.request.CreateConsolidationTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskRequest;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;

import java.util.List;

public interface IMasterDataService {

    List<MasterData> fetchByType(MasterDataType masterDataType);

    DependentServiceResponse fetchMasterData(Object request);

    DependentServiceResponse createMasterData(Object request);

    DependentServiceResponse updateMasterData(Object request);

    DependentServiceResponse fetchCarrierMasterData(Object request);

    DependentServiceResponse createCarrierMasterData(Object request);

    DependentServiceResponse updateCarrierMasterData(Object request);

    DependentServiceResponse fetchContainerTypeData(Object request);

    DependentServiceResponse createContainerTypeData(Object request);

    DependentServiceResponse updateContainerTypeData(Object request);

    DependentServiceResponse fetchVesselData(Object request);

    DependentServiceResponse createVesselData(Object request);

    DependentServiceResponse updateVesselData(Object request);

    DependentServiceResponse fetchRoutingMasterData(Object request);

    DependentServiceResponse createRoutingMasterData(Object request);

    DependentServiceResponse updateRoutingMasterData(Object request);

    DependentServiceResponse fetchCurrenciesData(Object request);

    DependentServiceResponse createCurrenciesData(Object request);

    DependentServiceResponse updateCurrenciesData(Object request);

    DependentServiceResponse fetchDangerousGoodData(Object request);

    DependentServiceResponse createDangerousGoodData(Object request);

    DependentServiceResponse updateDangerousGoodData(Object request);

    DependentServiceResponse fetchWarehouseData(Object request);

    DependentServiceResponse createWarehouseData(Object request);

    DependentServiceResponse updateWarehouseData(Object request);

    DependentServiceResponse fetchPortsData(Object request);

    DependentServiceResponse createPortsData(Object request);

    DependentServiceResponse updatePortsData(Object request);

    DependentServiceResponse fetchCommodityData(Object request);

    DependentServiceResponse createCommodityData(Object request);

    DependentServiceResponse updateCommodityData(Object request);

    DependentServiceResponse fetchSalesAgentData(Object request);

    DependentServiceResponse createSalesAgentData(Object request);

    DependentServiceResponse updateSalesAgentData(Object request);

    DependentServiceResponse fetchOrganizationData(Object request);

    DependentServiceResponse createOrganizationData(Object request);

    DependentServiceResponse updateOrganizationData(Object request);

    DependentServiceResponse fetchUnlocationData(Object request);
    DependentServiceResponse stateBasedList(Object request);


    DependentServiceResponse createUnlocationData(Object request);

    DependentServiceResponse updateUnlocationData(Object request);

    DependentServiceResponse fetchUserData(Object request);

    DependentServiceResponse fetchGridColorCodeData(Object request);
    DependentServiceResponse createGridColorCodeData(Object request);
    DependentServiceResponse updateGridColorCodeData(Object request);

    DependentServiceResponse listCousinBranches(Object request);
    DependentServiceResponse listCousinBranchesWithoutCurrent(Object request);
    DependentServiceResponse tenantByGuid(Object request);
    DependentServiceResponse importFlightSchedules(Object request);
    DependentServiceResponse fetchFlightStatus(Object request);
    DependentServiceResponse importSailingSchedules(Object request);
    DependentServiceResponse listSailingSchedule(Object request);
    DependentServiceResponse sendConsolidationTask(CreateConsolidationTaskRequest request);
    DependentServiceResponse sendShipmentTask(CreateShipmentTaskRequest request);
    DependentServiceResponse addressList(Object request);
    DependentServiceResponse tenantNameByTenantId(Object request);
    DependentServiceResponse retrieveTenantSettings();


    DependentServiceResponse fetchUnlocationOriginAndDestinationList(Object request);
    DependentServiceResponse fetchListUnlocationTransportModeBased(Object request);
    DependentServiceResponse fetchActivityMaster(Object request);
    DependentServiceResponse retrieveTenant();
    DependentServiceResponse fetchMultipleMasterData(Object request);

    DependentServiceResponse fetchOwnType(Object request);

    DependentServiceResponse fetchCarrierFilterList(Object request);
    DependentServiceResponse fetchGetTemplateMainPage(Object request);
    DependentServiceResponse fetchBillingList(Object request);
    DependentServiceResponse fetchBillChargesList(Object request);
    DependentServiceResponse fetchArObjectList(Object request);
    DependentServiceResponse fetchChargeType(Object request);
    DependentServiceResponse getDefaultOrg(Object request);

}
