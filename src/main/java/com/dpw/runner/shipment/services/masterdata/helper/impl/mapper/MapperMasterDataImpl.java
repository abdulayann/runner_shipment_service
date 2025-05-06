package com.dpw.runner.shipment.services.masterdata.helper.impl.mapper;

import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dto.v1.request.CreateConsolidationTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskRequest;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.*;

@Service
@Generated
public class MapperMasterDataImpl implements IMasterDataService {

    Map<MasterDataType , List<MasterData>> masterDataMap;

    @PostConstruct
    public void fillMasterData() {
        masterDataMap = new EnumMap<>(MasterDataType.class);
        List<MasterData> dataMap = new ArrayList<>();
        dataMap.add(MasterData.builder().itemValue("SEA").build());
        dataMap.add(MasterData.builder().itemValue("AIR").build());
        dataMap.add(MasterData.builder().itemValue("ROA").build());
        dataMap.add(MasterData.builder().itemValue("RAIL").build());
        masterDataMap.put(MasterDataType.TRANSPORT_MODE, dataMap);
    }

    @Override
    public List<MasterData> fetchByType(MasterDataType masterDataType) {
        return masterDataMap.get(masterDataType);
    }

    @Override
    public DependentServiceResponse fetchMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchCarrierMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createCarrierMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateCarrierMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchContainerTypeData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createContainerTypeData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateContainerTypeData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchVesselData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createVesselData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateVesselData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchRoutingMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createRoutingMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateRoutingMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchCurrenciesData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createCurrenciesData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateCurrenciesData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchDangerousGoodData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createDangerousGoodData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateDangerousGoodData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchWarehouseData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createWarehouseData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateWarehouseData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchPortsData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createPortsData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updatePortsData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchCommodityData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createCommodityData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateCommodityData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchSalesAgentData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createSalesAgentData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateSalesAgentData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchOrganizationData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createOrganizationData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateOrganizationData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchUnlocationData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse stateBasedList(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createUnlocationData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateUnlocationData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchUserData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchGridColorCodeData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse createGridColorCodeData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse updateGridColorCodeData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse listCousinBranches(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse listCousinBranchesWithoutCurrent(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse tenantByGuid(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse sendConsolidationTask(CreateConsolidationTaskRequest request) {
        return null;
    }

    @Override
    public DependentServiceResponse sendShipmentTask(CreateShipmentTaskRequest request) {
        return null;
    }

    @Override
    public DependentServiceResponse addressList(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse tenantNameByTenantId(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchUnlocationOriginAndDestinationList(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchListUnlocationTransportModeBased(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchActivityMaster(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse importFlightSchedules(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchFlightStatus(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse importSailingSchedules(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse listSailingSchedule(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse retrieveTenantSettings() {
        return null;
    }

    @Override
    public DependentServiceResponse retrieveTenant() {
        return null;
    }

    @Override
    public DependentServiceResponse fetchMultipleMasterData(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchOwnType(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchCarrierFilterList(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchGetTemplateMainPage(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchBillingList(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchBillChargesList(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchArObjectList(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchChargeType(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse getDefaultOrg(Object request) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchAllUnlocationData(Object request) {
        return null;
    }
}
