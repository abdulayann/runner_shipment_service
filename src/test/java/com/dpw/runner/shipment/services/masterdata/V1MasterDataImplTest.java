package com.dpw.runner.shipment.services.masterdata;


import com.dpw.runner.shipment.services.dto.v1.request.CreateConsolidationTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.FlightScheduleRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashMap;

import static com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl.ARRIVAL_ESTIMATED_RUNWAY;
import static com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl.DEPARTURE_ESTIMATED_RUNWAY;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class V1MasterDataImplTest {

    @InjectMocks
    private V1MasterDataImpl v1MasterData;

    @Mock
    private IV1Service v1Service;

    @Mock
    private JsonHelper jsonHelper;

    @Test
    void getDefaultOrg() {
        PartyRequestV2 partyRequestV2 = new PartyRequestV2();
        when(v1Service.getDefaultOrg()).thenReturn(partyRequestV2);
        assertNotNull(v1MasterData.getDefaultOrg(new Object()));
    }

    @Test
    void fetchChargeType() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchChargeCodeData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchChargeType(new Object()));
    }

    @Test
    void fetchArObjectList() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchArObjectList(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchArObjectList(new Object()));
    }

    @Test
    void fetchBillingList() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchBillingList(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchBillingList(new Object()));
    }

    @Test
    void fetchGetTemplateMainPage() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchGetTemplateMainPage(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchGetTemplateMainPage(new Object()));
    }

    @Test
    void fetchMultipleMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchMultipleMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchMultipleMasterData(new Object()));
    }

    @Test
    void fetchBillChargesList() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchBillChargesList(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchBillChargesList(new Object()));
    }

    @Test
    void retrieveTenant() {
        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();
        when(v1Service.retrieveTenant()).thenReturn(v1RetrieveResponse);
        assertNotNull(v1MasterData.retrieveTenant());
    }

    @Test
    void retrieveTenantSettings() {
        V1RetrieveResponse v1RetrieveResponse = new V1RetrieveResponse();
        when(v1Service.retrieveTenantSettings()).thenReturn(v1RetrieveResponse);
        assertNotNull(v1MasterData.retrieveTenantSettings());
    }

    @Test
    void fetchActivityMaster() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchActivityMaster(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchActivityMaster(new Object()));
    }

    @Test
    void fetchListUnlocationTransportModeBased() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchListUnlocationTransportModeBased(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchListUnlocationTransportModeBased(new Object()));
    }

    @Test
    void fetchUnlocationOriginAndDestinationList() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchUnlocationOriginAndDestinationList(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchUnlocationOriginAndDestinationList(new Object()));
    }

    @Test
    void fetchByType() {
        MasterDataType masterDataType = MasterDataType.SHIPMENT_TYPE;
        assertEquals(null, v1MasterData.fetchByType(masterDataType));
    }

    @Test
    void tenantNameByTenantId() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.tenantNameByTenantId(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.tenantNameByTenantId(new Object()));
    }

    @Test
    void addressList() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.addressList(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.addressList(new Object()));
    }

    @Test
    void listSailingSchedule() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.listSailingSchedule(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.listSailingSchedule(new Object()));
    }

    @Test
    void importSailingSchedules() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.importSailingSchedules(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.importSailingSchedules(new Object()));
    }

    @Test
    void fetchFlightStatus() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchFlightStatus(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchFlightStatus(new Object()));
    }

    @Test
    void importFlightSchedulesCatch() {
        HashMap<String, String> map = new HashMap<>();
        map.put(ARRIVAL_ESTIMATED_RUNWAY, LocalDateTime.now().toString());
        map.put(DEPARTURE_ESTIMATED_RUNWAY, LocalDateTime.now().toString());
        FlightScheduleRequest flightScheduleRequest = FlightScheduleRequest.builder().build();
        flightScheduleRequest.setEqualityFilter(map);
        when(jsonHelper.convertValue(any(), eq(FlightScheduleRequest.class))).thenReturn(flightScheduleRequest);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.setEntities(new Object());
        when(v1Service.importFlightSchedules(any())).thenReturn(v1DataResponse);

        FlightScheduleResponse flightScheduleResponse = new FlightScheduleResponse();
        when(jsonHelper.convertValueToList(any(), eq(FlightScheduleResponse.class))).thenReturn(Arrays.asList(flightScheduleResponse));

        assertNotNull(v1MasterData.importFlightSchedules(new Object()));
    }

    @Test
    void importFlightSchedules() {
        HashMap<String, String> map = new HashMap<>();
        map.put(ARRIVAL_ESTIMATED_RUNWAY, "2024-05-20 12:12:12");
        map.put(DEPARTURE_ESTIMATED_RUNWAY, "2024-05-20 12:12:12");
        FlightScheduleRequest flightScheduleRequest = FlightScheduleRequest.builder().build();
        flightScheduleRequest.setEqualityFilter(map);
        when(jsonHelper.convertValue(any(), eq(FlightScheduleRequest.class))).thenReturn(flightScheduleRequest);

        V1DataResponse v1DataResponse = new V1DataResponse();
        v1DataResponse.setEntities(new Object());
        when(v1Service.importFlightSchedules(any())).thenReturn(v1DataResponse);

        FlightScheduleResponse flightScheduleResponse = new FlightScheduleResponse();
        when(jsonHelper.convertValueToList(any(), eq(FlightScheduleResponse.class))).thenReturn(Arrays.asList(flightScheduleResponse));

        assertNotNull(v1MasterData.importFlightSchedules(new Object()));
    }

    @Test
    void sendConsolidationTask() {
        SendEntityResponse sendEntityResponse = new SendEntityResponse();
        when(v1Service.sendConsolidationTask(any())).thenReturn(sendEntityResponse);
        assertNotNull(v1MasterData.sendConsolidationTask(CreateConsolidationTaskRequest.builder().build()));
    }

    @Test
    void tenantByGuid() {
        TenantIdResponse tenantIdResponse = new TenantIdResponse();
        when(v1Service.tenantByGuid(any())).thenReturn(tenantIdResponse);
        assertNotNull(v1MasterData.tenantByGuid(new Object()));
    }

    @Test
    void listCousinBranchesWithoutCurrent() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.listCousinBranchesWithoutCurrent(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.listCousinBranchesWithoutCurrent(new Object()));
    }

    @Test
    void listCousinBranches() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.listCousinBranches(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.listCousinBranches(new Object()));
    }

    @Test
    void sendShipmentTask() {
        SendEntityResponse sendEntityResponse = new SendEntityResponse();
        when(v1Service.sendShipmentTask(any())).thenReturn(sendEntityResponse);
        assertNotNull(v1MasterData.sendShipmentTask(CreateShipmentTaskRequest.builder().build()));
    }

    @Test
    void updateGridColorCodeData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateGridColorCodeData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateGridColorCodeData(new Object()));
    }

    @Test
    void createGridColorCodeData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createGridColorCodeData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createGridColorCodeData(new Object()));
    }

    @Test
    void fetchCarrierFilterList() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchCarrierFilterList(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchCarrierFilterList(new Object()));
    }

    @Test
    void fetchOwnType() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchOwnType(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchOwnType(new Object()));
    }

    @Test
    void createUnlocationData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createUnlocationData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createUnlocationData(new Object()));
    }

    @Test
    void fetchGridColorCodeData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchGridColorCodeData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchGridColorCodeData(new Object()));
    }

    @Test
    void fetchUserData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchUsersData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchUserData(new Object()));
    }

    @Test
    void updateUnlocationData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateUnlocationData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateUnlocationData(new Object()));
    }

    @Test
    void fetchUnlocationData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchActiveUnlocation(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchUnlocationData(new Object()));
    }
    @Test
    void stateBasedList() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.stateBasedList(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.stateBasedList(new Object()));
    }

    @Test
    void updateOrganizationData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateOrganizationData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateOrganizationData(new Object()));
    }

    @Test
    void createOrganizationData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createOrganizationData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createOrganizationData(new Object()));
    }

    @Test
    void fetchOrganizationData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchOrganization(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchOrganizationData(new Object()));
    }

    @Test
    void updateSalesAgentData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateSalesAgentData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateSalesAgentData(new Object()));
    }

    @Test
    void createSalesAgentData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createSalesAgentData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createSalesAgentData(new Object()));
    }

    @Test
    void fetchSalesAgentData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchSalesAgentData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchSalesAgentData(new Object()));
    }

    @Test
    void updateCommodityData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateCommodityData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateCommodityData(new Object()));
    }

    @Test
    void createCommodityData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createCommodityData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createCommodityData(new Object()));
    }

    @Test
    void fetchCommodityData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchCommodityData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchCommodityData(new Object()));
    }

    @Test
    void updatePortsData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updatePortsData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updatePortsData(new Object()));
    }

    @Test
    void createPortsData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createPortsData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createPortsData(new Object()));
    }

    @Test
    void fetchPortsData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchPortsData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchPortsData(new Object()));
    }

    @Test
    void updateWarehouseData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateWarehouseData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateWarehouseData(new Object()));
    }

    @Test
    void createWarehouseData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createWarehouseData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createWarehouseData(new Object()));
    }

    @Test
    void fetchWarehouseData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchWarehouseData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchWarehouseData(new Object()));
    }

    @Test
    void updateDangerousGoodData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateDangerousGoodData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateDangerousGoodData(new Object()));
    }

    @Test
    void createDangerousGoodData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createDangerousGoodData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createDangerousGoodData(new Object()));
    }

    @Test
    void fetchDangerousGoodData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchDangerousGoodData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchDangerousGoodData(new Object()));
    }

    @Test
    void updateCurrenciesData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateCurrenciesData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateCurrenciesData(new Object()));
    }

    @Test
    void createCurrenciesData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createCurrenciesData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createCurrenciesData(new Object()));
    }

    @Test
    void fetchCurrenciesData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchCurrenciesData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchCurrenciesData(new Object()));
    }

    @Test
    void updateRoutingMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateRoutingMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateRoutingMasterData(new Object()));
    }

    @Test
    void createRoutingMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createRoutingMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createRoutingMasterData(new Object()));
    }

    @Test
    void fetchRoutingMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchRoutingMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchRoutingMasterData(new Object()));
    }

    @Test
    void updateVesselData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateVesselData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateVesselData(new Object()));
    }

    @Test
    void createVesselData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createVesselData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createVesselData(new Object()));
    }

    @Test
    void fetchVesselData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchVesselData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchVesselData(new Object()));
    }

    @Test
    void updateContainerTypeData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateContainerTypeData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateContainerTypeData(new Object()));
    }

    @Test
    void createContainerTypeData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createContainerTypeData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createContainerTypeData(new Object()));
    }

    @Test
    void fetchContainerTypeData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchContainerTypeData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchContainerTypeData(new Object()));
    }

    @Test
    void updateCarrierMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateCarrierMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateCarrierMasterData(new Object()));
    }

    @Test
    void createCarrierMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createCarrierMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createCarrierMasterData(new Object()));
    }

    @Test
    void fetchCarrierMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchCarrierMasterData(any(), anyBoolean())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchCarrierMasterData(new Object()));
    }

    @Test
    void updateMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.updateMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.updateMasterData(new Object()));
    }

    @Test
    void createMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.createMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.createMasterData(new Object()));
    }

    @Test
    void fetchMasterData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchMasterData(new Object()));
    }

    @Test
    void fetchAllUnlocationData() {
        V1DataResponse v1DataResponse = new V1DataResponse();
        when(v1Service.fetchUnlocation(any())).thenReturn(v1DataResponse);
        assertNotNull(v1MasterData.fetchAllUnlocationData(new Object()));
    }

}
