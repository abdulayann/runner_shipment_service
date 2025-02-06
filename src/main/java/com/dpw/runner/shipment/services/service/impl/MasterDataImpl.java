package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ListCousinBranchesForNteRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IMasterDataService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
public class MasterDataImpl implements IMasterDataService {

    private final MasterDataFactory masterDataFactory;
    private final IV1Service v1Service;
    private final MasterDataUtils masterDataUtils;
    private final CommonUtils commonUtils;

    @Autowired
    public MasterDataImpl (MasterDataFactory masterDataFactory, IV1Service v1Service, MasterDataUtils masterDataUtils, CommonUtils commonUtils) {
        this.masterDataFactory = masterDataFactory;
        this.v1Service = v1Service;
        this.masterDataUtils = masterDataUtils;
        this.commonUtils = commonUtils;
    }

    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createCarrier(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createCarrierMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateCarrier(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateCarrierMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listCarrier(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchCarrierMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createContainerType(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createContainerTypeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateContainerType(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateContainerTypeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listContainerType(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchContainerTypeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createVessel(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createVesselData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateVessel(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateVesselData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listVessel(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchVesselData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createRoutingMaster(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createRoutingMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateRoutingMaster(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateRoutingMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listRoutingMaster(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchRoutingMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createCurrencies(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createCurrenciesData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateCurrencies(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateCurrenciesData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listCurrencies(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchCurrenciesData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createDangerousGood(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createDangerousGoodData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateDangerousGood(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateDangerousGoodData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listDangerousGood(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchDangerousGoodData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createWarehouse(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createWarehouseData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateWarehouse(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateWarehouseData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listWarehouse(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchWarehouseData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createPorts(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createPortsData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updatePorts(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updatePortsData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listPorts(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchPortsData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createCommodity(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createCommodityData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateCommodity(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateCommodityData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listCommodity(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchCommodityData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createSalesAgent(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createSalesAgentData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateSalesAgent(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateSalesAgentData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listSalesAgent(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchSalesAgentData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createOrganization(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createOrganizationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateOrganization(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateOrganizationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listOrganization(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchOrganizationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createUnlocation(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createUnlocationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateUnlocation(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateUnlocationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listUnlocation(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchUnlocationData(commonRequestModel.getDependentData()));
    }
    @Override
    public ResponseEntity<IRunnerResponse> stateBasedList(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().stateBasedList(commonRequestModel.getDependentData()));
    }


    @Override
    public ResponseEntity<IRunnerResponse> listUsers(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchUserData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> createGridColorCode(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createGridColorCodeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> updateGridColorCode(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateGridColorCodeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listGridColorCode(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchGridColorCodeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listOwnType(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchOwnType(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listCarrierFilter(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchCarrierFilterList(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listCousinBranches(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().listCousinBranches(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listCousinBranchesWithoutCurrent(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().listCousinBranchesWithoutCurrent(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> importFlightSchedules(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().importFlightSchedules(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchFlightStatus(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchFlightStatus(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> importSailingSchedules(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().importSailingSchedules(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listSailingSchedule(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().listSailingSchedule(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> addressList(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().addressList(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> tenantNameByTenantId(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().tenantNameByTenantId(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchUnlocationOriginAndDestinationList(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchUnlocationOriginAndDestinationList(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchListUnlocationTransportModeBased(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchListUnlocationTransportModeBased(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchActivityMaster(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchActivityMaster(commonRequestModel.getDependentData()));
    }

    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveTenantSettings() {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().retrieveTenantSettings());
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveTenant() {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().retrieveTenant());
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchGetTemplateMainPage(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchGetTemplateMainPage(commonRequestModel.getDependentData()));
    }
    @Override
    public ResponseEntity<IRunnerResponse> listRoles(CommonRequestModel commonRequestModel) {
        V1DataResponse v1DataResponse = v1Service.fetchRolesList(commonRequestModel.getDependentData());
        return ResponseHelper.buildDependentServiceResponse(DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build());
    }

    @Override
    public ResponseEntity<IRunnerResponse> fetchChargeTypes(CommonRequestModel commonRequestModel) {
        V1DataResponse v1DataResponse = v1Service.fetchChargeCodeData(commonRequestModel.getDependentData());
        return ResponseHelper.buildDependentServiceResponse(DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build());
    }

    @Override
    public ResponseEntity<IRunnerResponse> getDefaultOrg(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().getDefaultOrg(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<IRunnerResponse> listOrgs(CommonRequestModel commonRequestModel) {
        V1DataResponse v1DataResponse = v1Service.listOrgs(commonRequestModel.getDependentData());
        return ResponseHelper.buildDependentServiceResponse(DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build());
    }


    @Override
    public ResponseEntity<IRunnerResponse> fetchMultipleMasterData(CommonRequestModel commonRequestModel) {
        MasterListRequestV2 request = (MasterListRequestV2) commonRequestModel.getData();
        request.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, "ItemDescription", "ValuenDesc", "Cascade"));
        var keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(request);
        Map<String, Object> response = new HashMap<>();
        keyMasterDataMap.forEach((key, value) -> masterDataUtils.setKeyValueForMasterLists(response, key, value));
        return ResponseHelper.buildSuccessResponse(response);
    }

    @Override
    public ResponseEntity<IRunnerResponse> listBranchesByDefaultOrgAndAddress(CommonRequestModel commonRequestModel) {
        V1DataResponse v1DataResponse = v1Service.listBranchesByDefaultOrgAndAddress(commonRequestModel.getDependentData());
        return ResponseHelper.buildDependentServiceResponse(DependentServiceResponse.builder().success(true)
                .data(v1DataResponse.entities).pageSize(v1DataResponse.take).numberOfRecords(v1DataResponse.totalCount).pageNo(v1DataResponse.skip).build());
    }

    @Override
    public ResponseEntity<IRunnerResponse> listCousinBranchForNTE(ListCousinBranchesForNteRequest request) {
        List<Object> criteria = request.getCriteria() ;
        List<Long> tenantIds = commonUtils.getTenantIdsFromEntity(request.getEntityId(), request.getEntityType(), request.getIsReassign(), request.getIsReceivingBranch(), request.getIsTriangulationBranch());
        if(tenantIds!=null) {
            List<Long> existingTenantIds = criteria!=null && !criteria.isEmpty() && criteria.size() > 2 ? (List<Long>) criteria.get(2): null;
            criteria = convertToV1NotInCriteria("TenantId", tenantIds, existingTenantIds);
        }
        CommonV1ListRequest v1ListRequest = CommonV1ListRequest.builder()
                .sort(request.getSort())
                .containsText(request.getContainsText())
                .excludeTotalCount(request.getExcludeTotalCount())
                .columnSelection(request.getColumnSelection())
                .includeColumns(request.getIncludeColumns())
                .take(request.getTake())
                .skip(request.getSkip())
                .criteriaRequests(criteria)
                .build();
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().listCousinBranches(v1ListRequest));
    }

    public List<Object> convertToV1NotInCriteria(String filterValue, List<?> values, List<Long> existingTenantIds) {
        List<String> itemType = new ArrayList<>();
        itemType.add(filterValue);
        List<Object> param = new ArrayList<>();
        if(values!=null)
            param.addAll(values);
        if (existingTenantIds != null) {
            for (Object obj : existingTenantIds) {
                if (obj instanceof List) {
                    param.addAll((List<?>) obj);
                } else {
                    param.add(obj);
                }
            }
        }
        return new ArrayList<>(Arrays.asList(itemType, "not in", Collections.singletonList(param)));
    }
}
