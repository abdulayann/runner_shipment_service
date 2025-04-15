package com.dpw.runner.shipment.services.masterdata.helper.impl.v1;

import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.v1.request.CreateConsolidationTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.CreateShipmentTaskRequest;
import com.dpw.runner.shipment.services.dto.v1.request.FlightScheduleRequest;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.helper.IMasterDataService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.PartyRequestV2;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Service
@Slf4j
public class V1MasterDataImpl implements IMasterDataService {

    public static final String ARRIVAL_ESTIMATED_RUNWAY = "ArrivalEstimatedRunway";
    public static final String DEPARTURE_ESTIMATED_RUNWAY = "DepartureEstimatedRunway";

    private final IV1Service v1Service;

    private final JsonHelper jsonHelper;

    @Autowired
    public V1MasterDataImpl(IV1Service v1Service, JsonHelper jsonHelper){
        this.v1Service = v1Service;
        this.jsonHelper = jsonHelper;
    }

    @Override
    public DependentServiceResponse fetchMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse createMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse updateMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchCarrierMasterData(Object request) {
        boolean isList = false;
        try{
            var obj = jsonHelper.convertValue(request, CarrierListObject.class);
            isList = (obj != null && obj.getListObject() != null) && obj.getIsList() != null && obj.getIsList() ;
        }
        catch (Exception e) {
            log.error("unable to construct CarrierListObject from request");
        }
        V1DataResponse v1DataResponse = v1Service.fetchCarrierMasterData(request, isList);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse createCarrierMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createCarrierMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse updateCarrierMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateCarrierMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchContainerTypeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchContainerTypeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createContainerTypeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createContainerTypeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updateContainerTypeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateContainerTypeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchVesselData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchVesselData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createVesselData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createVesselData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updateVesselData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateVesselData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchRoutingMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchRoutingMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createRoutingMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createRoutingMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updateRoutingMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateRoutingMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchCurrenciesData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchCurrenciesData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createCurrenciesData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createCurrenciesData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updateCurrenciesData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateCurrenciesData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchDangerousGoodData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchDangerousGoodData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createDangerousGoodData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createDangerousGoodData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updateDangerousGoodData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateDangerousGoodData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchWarehouseData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchWarehouseData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createWarehouseData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createWarehouseData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updateWarehouseData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateWarehouseData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchPortsData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchPortsData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createPortsData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createPortsData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updatePortsData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updatePortsData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchCommodityData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchCommodityData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createCommodityData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createCommodityData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updateCommodityData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateCommodityData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchSalesAgentData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchSalesAgentData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse createSalesAgentData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createSalesAgentData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse updateSalesAgentData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateSalesAgentData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public DependentServiceResponse fetchOrganizationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchOrganization(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse createOrganizationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createOrganizationData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse updateOrganizationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateOrganizationData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchUnlocationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchUnlocation(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }
    @Override
    public DependentServiceResponse stateBasedList(Object request) {
        V1DataResponse v1DataResponse = v1Service.stateBasedList(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }


    @Override
    public DependentServiceResponse createUnlocationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createUnlocationData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse updateUnlocationData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateUnlocationData( request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchUserData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchUsersData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchGridColorCodeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchGridColorCodeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchOwnType(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchOwnType(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchCarrierFilterList(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchCarrierFilterList(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse createGridColorCodeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.createGridColorCodeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse updateGridColorCodeData(Object request) {
        V1DataResponse v1DataResponse = v1Service.updateGridColorCodeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse listCousinBranches(Object request) {
        V1DataResponse v1DataResponse = v1Service.listCousinBranches(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse listCousinBranchesWithoutCurrent(Object request) {
        V1DataResponse v1DataResponse = v1Service.listCousinBranchesWithoutCurrent(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse tenantByGuid(Object request) {
        TenantIdResponse response = v1Service.tenantByGuid(request);
        return DependentServiceResponse.builder().success(true)
                .data(response).build();
    }

    @Override
    public DependentServiceResponse sendConsolidationTask(CreateConsolidationTaskRequest request) {
        SendEntityResponse response = v1Service.sendConsolidationTask(request);
        return DependentServiceResponse.builder().success(true)
                .data(response).build();
    }

    @Override
    public DependentServiceResponse sendShipmentTask(CreateShipmentTaskRequest request) {
        SendEntityResponse response = v1Service.sendShipmentTask(request);
        return DependentServiceResponse.builder().success(true)
                .data(response).build();
    }

    @Override
    public DependentServiceResponse importFlightSchedules(Object request) {
        FlightScheduleRequest flightScheduleRequest = jsonHelper.convertValue(request, FlightScheduleRequest.class);
        if(flightScheduleRequest != null && flightScheduleRequest.getEqualityFilter() != null) {
            if(flightScheduleRequest.getEqualityFilter().containsKey(ARRIVAL_ESTIMATED_RUNWAY) && StringUtility.isNotEmpty(flightScheduleRequest.getEqualityFilter().get(ARRIVAL_ESTIMATED_RUNWAY))) {
                try {
                    LocalDateTime dateTime = LocalDateTime.parse(flightScheduleRequest.getEqualityFilter().get(ARRIVAL_ESTIMATED_RUNWAY), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                    flightScheduleRequest.getEqualityFilter().put(ARRIVAL_ESTIMATED_RUNWAY, dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
                } catch (Exception e) {
                }
            }
            if(flightScheduleRequest.getEqualityFilter().containsKey(DEPARTURE_ESTIMATED_RUNWAY) && StringUtility.isNotEmpty(flightScheduleRequest.getEqualityFilter().get(DEPARTURE_ESTIMATED_RUNWAY))) {
                try {
                    LocalDateTime dateTime = LocalDateTime.parse(flightScheduleRequest.getEqualityFilter().get(DEPARTURE_ESTIMATED_RUNWAY), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                    flightScheduleRequest.getEqualityFilter().put(DEPARTURE_ESTIMATED_RUNWAY, dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
                } catch (Exception e) {
                }
            }
        }
        V1DataResponse v1DataResponse = v1Service.importFlightSchedules(flightScheduleRequest);

        if(v1DataResponse.getEntities() != null) {
            List<FlightScheduleResponse> flightScheduleResponses = jsonHelper.convertValueToList(v1DataResponse.getEntities(), FlightScheduleResponse.class);
            v1DataResponse.entities = flightScheduleResponses;
        }

        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchFlightStatus(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchFlightStatus(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse importSailingSchedules(Object request) {
        V1DataResponse v1DataResponse = v1Service.importSailingSchedules(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse listSailingSchedule(Object request) {
        V1DataResponse v1DataResponse = v1Service.listSailingSchedule(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse addressList(Object request) {
        V1DataResponse v1DataResponse = v1Service.addressList(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse tenantNameByTenantId(Object request) {
        V1DataResponse v1DataResponse = v1Service.tenantNameByTenantId(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();    }

    @Override
    public List<MasterData> fetchByType(MasterDataType masterDataType) {
        return null;
    }

    @Override
    public DependentServiceResponse fetchUnlocationOriginAndDestinationList(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchUnlocationOriginAndDestinationList(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchListUnlocationTransportModeBased(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchListUnlocationTransportModeBased(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchActivityMaster(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchActivityMaster(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse retrieveTenantSettings() {
        V1RetrieveResponse v1RetrieveResponse = v1Service.retrieveTenantSettings();
        return DependentServiceResponse.builder().success(true)
                .data(v1RetrieveResponse.getEntity()).build();
    }

    @Override
    public DependentServiceResponse retrieveTenant() {
        V1RetrieveResponse v1RetrieveResponse = v1Service.retrieveTenant();
        return DependentServiceResponse.builder().success(true)
                .data(v1RetrieveResponse.getEntity()).build();
    }

    @Override
    public DependentServiceResponse fetchMultipleMasterData(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchMultipleMasterData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchGetTemplateMainPage(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchGetTemplateMainPage(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchBillingList(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchBillingList(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchBillChargesList(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchBillChargesList(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchArObjectList(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchArObjectList(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse fetchChargeType(Object request) {
        V1DataResponse v1DataResponse = v1Service.fetchChargeCodeData(request);
        return DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build();
    }

    @Override
    public DependentServiceResponse getDefaultOrg(Object request) {
        PartyRequestV2 partyRequestV2 = v1Service.getDefaultOrg();
        return DependentServiceResponse.builder().success(true)
                .data(partyRequestV2).build();
    }


}
