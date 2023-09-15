package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.service.interfaces.IMasterDataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
public class MasterDataImpl implements IMasterDataService {

    @Autowired
    private MasterDataFactory masterDataFactory;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createCarrier(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createCarrierMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateCarrier(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateCarrierMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listCarrier(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchCarrierMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createContainerType(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createContainerTypeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateContainerType(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateContainerTypeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listContainerType(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchContainerTypeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createVessel(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createVesselData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateVessel(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateVesselData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listVessel(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchVesselData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createRoutingMaster(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createRoutingMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateRoutingMaster(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateRoutingMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listRoutingMaster(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchRoutingMasterData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createCurrencies(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createCurrenciesData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateCurrencies(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateCurrenciesData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listCurrencies(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchCurrenciesData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createDangerousGood(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createDangerousGoodData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateDangerousGood(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateDangerousGoodData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listDangerousGood(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchDangerousGoodData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createWarehouse(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createWarehouseData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateWarehouse(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateWarehouseData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listWarehouse(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchWarehouseData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createPorts(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createPortsData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updatePorts(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updatePortsData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listPorts(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchPortsData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createCommodity(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createCommodityData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateCommodity(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateCommodityData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listCommodity(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchCommodityData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createSalesAgent(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createSalesAgentData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateSalesAgent(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateSalesAgentData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listSalesAgent(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchSalesAgentData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createOrganization(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createOrganizationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateOrganization(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateOrganizationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listOrganization(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchOrganizationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createUnlocation(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createUnlocationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateUnlocation(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateUnlocationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listUnlocation(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchUnlocationData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listUsers(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchUserData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> createGridColorCode(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().createGridColorCodeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> updateGridColorCode(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().updateGridColorCodeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listGridColorCode(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchGridColorCodeData(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listCousinBranches(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().listCousinBranches(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listCousinBranchesWithoutCurrent(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().listCousinBranchesWithoutCurrent(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> importFlightSchedules(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().importFlightSchedules(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> fetchFlightStatus(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().fetchFlightStatus(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> importSailingSchedules(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().importSailingSchedules(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> listSailingSchedule(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().listSailingSchedule(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> addressList(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().addressList(commonRequestModel.getDependentData()));
    }

    @Override
    public ResponseEntity<?> tenantNameByTenantId(CommonRequestModel commonRequestModel) {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().tenantNameByTenantId(commonRequestModel.getDependentData()));
    }

    @Override
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<?> retrieveTenantSettings() {
        return ResponseHelper.buildDependentServiceResponse(masterDataFactory.getMasterDataService().retrieveTenantSettings());
    }
}
