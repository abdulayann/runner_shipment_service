package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.helper.impl.v1.V1MasterDataImpl;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.concurrent.CompletableFuture;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class MasterDataImplTest {

    @Mock
    private MasterDataFactory masterDataFactory;

    @Mock
    private IV1Service v1Service;

    @Mock
    private V1MasterDataImpl v1MasterData;

    @InjectMocks
    private MasterDataImpl masterData;

    @Test
    void create() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.create(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void update() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.update(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.list(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createCarrier() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createCarrierMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createCarrier(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateCarrier() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateCarrierMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateCarrier(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCarrier() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchCarrierMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listCarrier(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createContainerType() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createContainerTypeData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createContainerType(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateContainerType() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateContainerTypeData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateContainerType(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listContainerType() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchContainerTypeData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listContainerType(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createVessel() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createVesselData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createVessel(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateVessel() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateVesselData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateVessel(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listVessel() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchVesselData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listVessel(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createRoutingMaster() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createRoutingMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createRoutingMaster(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateRoutingMaster() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateRoutingMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateRoutingMaster(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listRoutingMaster() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchRoutingMasterData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listRoutingMaster(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createCurrencies() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createCurrenciesData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createCurrencies(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateCurrencies() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateCurrenciesData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateCurrencies(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCurrencies() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchCurrenciesData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listCurrencies(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createDangerousGood() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createDangerousGoodData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createDangerousGood(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateDangerousGood() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateDangerousGoodData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateDangerousGood(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listDangerousGood() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchDangerousGoodData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listDangerousGood(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createWarehouse() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createWarehouseData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createWarehouse(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateWarehouse() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateWarehouseData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateWarehouse(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listWarehouse() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchWarehouseData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listWarehouse(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createPorts() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createPortsData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createPorts(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updatePorts() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updatePortsData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updatePorts(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listPorts() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchPortsData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listPorts(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createCommodity() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createCommodityData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createCommodity(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateCommodity() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateCommodityData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateCommodity(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCommodity() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchCommodityData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listCommodity(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createSalesAgent() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createSalesAgentData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createSalesAgent(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateSalesAgent() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateSalesAgentData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateSalesAgent(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listSalesAgent() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchSalesAgentData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listSalesAgent(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createOrganization() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createOrganizationData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createOrganization(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateOrganization() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateOrganizationData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateOrganization(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listOrganization() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchOrganizationData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listOrganization(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createUnlocation() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createUnlocationData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createUnlocation(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateUnlocation() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateUnlocationData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateUnlocation(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listUnlocation() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchUnlocationData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listUnlocation(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listUsers() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchUserData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listUsers(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createGridColorCode() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().createGridColorCodeData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.createGridColorCode(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateGridColorCode() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().updateGridColorCodeData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.updateGridColorCode(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listGridColorCode() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchGridColorCodeData(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listGridColorCode(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listOwnType() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchOwnType(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listOwnType(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCarrierFilter() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchCarrierFilterList(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listCarrierFilter(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranches() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().listCousinBranches(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listCousinBranches(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchesWithoutCurrent() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().listCousinBranchesWithoutCurrent(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listCousinBranchesWithoutCurrent(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importFlightSchedules() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().importFlightSchedules(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.importFlightSchedules(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchFlightStatus() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchFlightStatus(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.fetchFlightStatus(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importSailingSchedules() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().importSailingSchedules(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.importSailingSchedules(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listSailingSchedule() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().listSailingSchedule(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listSailingSchedule(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void addressList() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().addressList(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.addressList(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void tenantNameByTenantId() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().tenantNameByTenantId(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.tenantNameByTenantId(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchUnlocationOriginAndDestinationList() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchUnlocationOriginAndDestinationList(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.fetchUnlocationOriginAndDestinationList(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchListUnlocationTransportModeBased() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchListUnlocationTransportModeBased(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.fetchListUnlocationTransportModeBased(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchActivityMaster() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchActivityMaster(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.fetchActivityMaster(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listAsync() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = masterData.listAsync(commonRequestModel);
        Assertions.assertNull(responseEntity);
    }

    @Test
    void delete() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        ResponseEntity<IRunnerResponse> responseEntity = masterData.delete(commonRequestModel);
        Assertions.assertNull(responseEntity);
    }

    @Test
    void retrieveById() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        ResponseEntity<IRunnerResponse> responseEntity = masterData.retrieveById(commonRequestModel);
        Assertions.assertNull(responseEntity);
    }

    @Test
    void retrieveTenantSettings() {
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().retrieveTenantSettings()).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.retrieveTenantSettings();
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveTenant() {
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().retrieveTenant()).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.retrieveTenant();
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchGetTemplateMainPage() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().fetchGetTemplateMainPage(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.fetchGetTemplateMainPage(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listRoles() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        V1DataResponse v1DataResponse = V1DataResponse.builder()
                .skip(4)
                .totalCount(10)
                .take(50)
                .build();
        Mockito.when(v1Service.fetchRolesList(Mockito.any())).thenReturn(v1DataResponse);
        ResponseEntity<IRunnerResponse> responseEntity = masterData.listRoles(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchChargeTypes() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        V1DataResponse v1DataResponse = V1DataResponse.builder()
                .skip(4)
                .totalCount(10)
                .take(50)
                .build();
        Mockito.when(v1Service.fetchChargeCodeData(Mockito.any())).thenReturn(v1DataResponse);
        ResponseEntity<IRunnerResponse> responseEntity = masterData.fetchChargeTypes(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getDefaultOrg() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Mockito.when(masterDataFactory.getMasterDataService()).thenReturn(v1MasterData);
        Mockito.when(masterDataFactory.getMasterDataService().getDefaultOrg(Mockito.any())).thenReturn(new DependentServiceResponse());
        ResponseEntity<IRunnerResponse> responseEntity = masterData.getDefaultOrg(commonRequestModel);
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}