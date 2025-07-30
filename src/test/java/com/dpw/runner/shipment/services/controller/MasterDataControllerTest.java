package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.ListCousinBranchesForEtRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.service.interfaces.IMasterDataService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class MasterDataControllerTest {

    @Mock
    private IMasterDataService iMasterDataService;

    @InjectMocks
    private MasterDataController masterDataController;


    /**
     * Method under test: {@link MasterDataController#create(Object)}
     */

    @Test
    void testCreate() {
        // Mock
        when(iMasterDataService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.create(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreate2() {
        // Mock
        when(iMasterDataService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.create(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreate3() {
        // Mock
        when(iMasterDataService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.create(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#update(Object)}
     */

    @Test
    void testUpdate() throws RunnerException {
        // Mock
        when(iMasterDataService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.update(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testUpdate2() throws RunnerException {
        // Mock
        when(iMasterDataService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.update(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testUpdate3() throws RunnerException {
        // Mock
        when(iMasterDataService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.update(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#list(Object)}
     */

    @Test
    void testList() {
        // Mock
        when(iMasterDataService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.list(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testList2() {
        // Mock
        when(iMasterDataService.list(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.list(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testList3() {
        // Mock
        when(iMasterDataService.list(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.list(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    /**
     * Method under test: {@link MasterDataController#createCarrier(Object)}
     */

    @Test
    void testCreateCarrier() {
        // Mock
        when(iMasterDataService.createCarrier(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createCarrier(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateCarrier2() {
        // Mock
        when(iMasterDataService.createCarrier(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createCarrier(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateCarrier3() {
        // Mock
        when(iMasterDataService.createCarrier(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createCarrier(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateNonBillableCustomer1() throws RunnerException {
        // Mock
        when(iMasterDataService.createNonBillableCustomer(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createNonBillableCustomer(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateNonBillableCustomer2() throws RunnerException {
        // Mock
        when(iMasterDataService.createNonBillableCustomer(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createNonBillableCustomer(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateNonBillableCustomer3() throws RunnerException {
        // Mock
        when(iMasterDataService.createNonBillableCustomer(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createNonBillableCustomer(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateCarrier(Object)}
     */

    @Test
    void testUpdateCarrier() {
        // Mock
        when(iMasterDataService.updateCarrier(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateCarrier(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateCarrier2() {
        // Mock
        when(iMasterDataService.updateCarrier(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateCarrier(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateCarrier3() {
        // Mock
        when(iMasterDataService.updateCarrier(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateCarrier(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#carrierList(CarrierListObject)}
     */

    @Test
    void testCarrierList() {
        // Mock
        when(iMasterDataService.listCarrier(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.carrierList(new CarrierListObject());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCarrierList2() {
        // Mock
        when(iMasterDataService.listCarrier(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.carrierList(new CarrierListObject());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCarrierList3() {
        // Mock
        when(iMasterDataService.listCarrier(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.carrierList(new CarrierListObject());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createContainerType(Object)}
     */

    @Test
    void testCreateContainerType() {
        // Mock
        when(iMasterDataService.createContainerType(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateContainerType2() {
        // Mock
        when(iMasterDataService.createContainerType(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateContainerType3() {
        // Mock
        when(iMasterDataService.createContainerType(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateContainerType(Object)}
     */

    @Test
    void testUpdateContainerType() {
        // Mock
        when(iMasterDataService.updateContainerType(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateContainerType2() {
        // Mock
        when(iMasterDataService.updateContainerType(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateContainerType3() {
        // Mock
        when(iMasterDataService.updateContainerType(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listContainerType(Object)}
     */

    @Test
    void testListContainerType() {
        // Mock
        when(iMasterDataService.listContainerType(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListContainerType2() {
        // Mock
        when(iMasterDataService.listContainerType(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListContainerType3() {
        // Mock
        when(iMasterDataService.listContainerType(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listContainerType(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createVessel(Object)}
     */

    @Test
    void testCreateVessel() {
        // Mock
        when(iMasterDataService.createVessel(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createVessel(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateVessel2() {
        // Mock
        when(iMasterDataService.createVessel(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createVessel(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateVessel3() {
        // Mock
        when(iMasterDataService.createVessel(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createVessel(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateVessel(Object)}
     */

    @Test
    void testUpdateVessel() {
        // Mock
        when(iMasterDataService.updateVessel(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateVessel(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateVessel2() {
        // Mock
        when(iMasterDataService.updateVessel(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateVessel(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateVessel3() {
        // Mock
        when(iMasterDataService.updateVessel(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateVessel(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listVessel(Object)}
     */

    @Test
    void testListVessel() {
        // Mock
        when(iMasterDataService.listVessel(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listVessel(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListVessel2() {
        // Mock
        when(iMasterDataService.listVessel(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listVessel(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListVessel3() {
        // Mock
        when(iMasterDataService.listVessel(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listVessel(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createRoutingMaster(Object)}
     */

    @Test
    void testCreateRoutingMaster() {
        // Mock
        when(iMasterDataService.createRoutingMaster(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateRoutingMaster2() {
        // Mock
        when(iMasterDataService.createRoutingMaster(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateRoutingMaster3() {
        // Mock
        when(iMasterDataService.createRoutingMaster(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateRoutingMaster(Object)}
     */

    @Test
    void testUpdateRoutingMaster() {
        // Mock
        when(iMasterDataService.updateRoutingMaster(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateRoutingMaster2() {
        // Mock
        when(iMasterDataService.updateRoutingMaster(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateRoutingMaster3() {
        // Mock
        when(iMasterDataService.updateRoutingMaster(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listRoutingMaster(Object)}
     */

    @Test
    void testListRoutingMaster() {
        // Mock
        when(iMasterDataService.listRoutingMaster(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListRoutingMaster2() {
        // Mock
        when(iMasterDataService.listRoutingMaster(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListRoutingMaster3() {
        // Mock
        when(iMasterDataService.listRoutingMaster(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listRoutingMaster(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createCurrencies(Object)}
     */

    @Test
    void testCreateCurrencies() {
        // Mock
        when(iMasterDataService.createCurrencies(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateCurrencies2() {
        // Mock
        when(iMasterDataService.createCurrencies(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateCurrencies3() {
        // Mock
        when(iMasterDataService.createCurrencies(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateCurrencies(Object)}
     */

    @Test
    void testUpdateCurrencies() {
        // Mock
        when(iMasterDataService.updateCurrencies(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateCurrencies2() {
        // Mock
        when(iMasterDataService.updateCurrencies(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testUpdateCurrencies3() {
        // Mock
        when(iMasterDataService.updateCurrencies(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listCurrencies(Object)}
     */

    @Test
    void testListCurrencies() {
        // Mock
        when(iMasterDataService.listCurrencies(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListCurrencies2() {
        // Mock
        when(iMasterDataService.listCurrencies(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListCurrencies3() {
        // Mock
        when(iMasterDataService.listCurrencies(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listCurrencies(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createDangerousGood(Object)}
     */

    @Test
    void testCreateDangerousGood() {
        // Mock
        when(iMasterDataService.createDangerousGood(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateDangerousGood2() {
        // Mock
        when(iMasterDataService.createDangerousGood(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateDangerousGood3() {
        // Mock
        when(iMasterDataService.createDangerousGood(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateDangerousGood(Object)}
     */

    @Test
    void updateDangerousGoodTest() {
        // Mock
        when(iMasterDataService.updateDangerousGood(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateDangerousGoodTest2() {
        // Mock
        when(iMasterDataService.updateDangerousGood(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateDangerousGoodTest3() {
        // Mock
        when(iMasterDataService.updateDangerousGood(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listDangerousGood(Object)}
     */

    @Test
    void listDangerousGoodTest() {
        // Mock
        when(iMasterDataService.listDangerousGood(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listDangerousGoodTest2() {
        // Mock
        when(iMasterDataService.listDangerousGood(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listDangerousGoodTest3() {
        // Mock
        when(iMasterDataService.listDangerousGood(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listDangerousGood(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createWarehouse(Object)}
     */

    @Test
    void createWarehouseTest() {
        // Mock
        when(iMasterDataService.createWarehouse(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createWarehouseTest2() {
        // Mock
        when(iMasterDataService.createWarehouse(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createWarehouseTest3() {
        // Mock
        when(iMasterDataService.createWarehouse(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateWarehouse(Object)}
     */

    @Test
    void updateWarehouseTest() {
        // Mock
        when(iMasterDataService.updateWarehouse(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateWarehouseTest2() {
        // Mock
        when(iMasterDataService.updateWarehouse(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateWarehouseTest3() {
        // Mock
        when(iMasterDataService.updateWarehouse(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listWarehouse(Object)}
     */

    @Test
    void listWarehouseTest() {
        // Mock
        when(iMasterDataService.listWarehouse(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listWarehouseTest2() {
        // Mock
        when(iMasterDataService.listWarehouse(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listWarehouseTest3() {
        // Mock
        when(iMasterDataService.listWarehouse(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listWarehouse(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createPorts(Object)}
     */

    @Test
    void createPortsTest() {
        // Mock
        when(iMasterDataService.createPorts(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createPorts(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createPortsTest2() {
        // Mock
        when(iMasterDataService.createPorts(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createPorts(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createPortsTest3() {
        // Mock
        when(iMasterDataService.createPorts(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createPorts(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updatePorts(Object)}
     */

    @Test
    void updatePortsTest() {
        // Mock
        when(iMasterDataService.updatePorts(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updatePorts(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updatePortsTest2() {
        // Mock
        when(iMasterDataService.updatePorts(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updatePorts(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updatePortsTest3() {
        // Mock
        when(iMasterDataService.updatePorts(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updatePorts(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listPorts(Object)}
     */

    @Test
    void listPortsTest() {
        // Mock
        when(iMasterDataService.listPorts(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listPorts(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listPortsTest2() {
        // Mock
        when(iMasterDataService.listPorts(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listPorts(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listPortsTest3() {
        // Mock
        when(iMasterDataService.listPorts(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listPorts(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createCommodity(Object)}
     */

    @Test
    void createCommodityTest() {
        // Mock
        when(iMasterDataService.createCommodity(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createCommodityTest2() {
        // Mock
        when(iMasterDataService.createCommodity(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createCommodityTest3() {
        // Mock
        when(iMasterDataService.createCommodity(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateCommodity(Object)}
     */

    @Test
    void updateCommodityTest() {
        // Mock
        when(iMasterDataService.updateCommodity(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateCommodityTest2() {
        // Mock
        when(iMasterDataService.updateCommodity(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateCommodityTest3() {
        // Mock
        when(iMasterDataService.updateCommodity(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listCommodity(Object)}
     */

    @Test
    void listCommodityTest() {
        // Mock
        when(iMasterDataService.listCommodity(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCommodityTest2() {
        // Mock
        when(iMasterDataService.listCommodity(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCommodityTest3() {
        // Mock
        when(iMasterDataService.listCommodity(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listCommodity(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createSalesAgent(Object)}
     */

    @Test
    void createSalesAgentTest() {
        // Mock
        when(iMasterDataService.createSalesAgent(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createSalesAgentTest2() {
        // Mock
        when(iMasterDataService.createSalesAgent(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createSalesAgentTest3() {
        // Mock
        when(iMasterDataService.createSalesAgent(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateSalesAgent(Object)}
     */

    @Test
    void updateSalesAgentTest() {
        // Mock
        when(iMasterDataService.updateSalesAgent(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateSalesAgentTest2() {
        // Mock
        when(iMasterDataService.updateSalesAgent(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateSalesAgentTest3() {
        // Mock
        when(iMasterDataService.updateSalesAgent(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listSalesAgent(Object)}
     */

    @Test
    void listSalesAgentTest() {
        // Mock
        when(iMasterDataService.listSalesAgent(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listSalesAgentTest2() {
        // Mock
        when(iMasterDataService.listSalesAgent(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listSalesAgentTest3() {
        // Mock
        when(iMasterDataService.listSalesAgent(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listSalesAgent(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    /**
     * Method under test: {@link MasterDataController#createUnlocation(Object)}
     */

    @Test
    void createUnlocationTest() {
        // Mock
        when(iMasterDataService.createUnlocation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createUnlocationTest2() {
        // Mock
        when(iMasterDataService.createUnlocation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createUnlocationTest3() {
        // Mock
        when(iMasterDataService.createUnlocation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    /**
     * Method under test: {@link MasterDataController#updateUnlocation(Object)}
     */

    @Test
    void updateUnlocationTest() {
        // Mock
        when(iMasterDataService.updateUnlocation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateUnlocationTest2() {
        // Mock
        when(iMasterDataService.updateUnlocation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateUnlocationTest3() {
        // Mock
        when(iMasterDataService.updateUnlocation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listUnlocation(Object)}
     */

    @Test
    void listUnlocationTest() {
        // Mock
        when(iMasterDataService.listUnlocation(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listUnlocationTest2() {
        // Mock
        when(iMasterDataService.listUnlocation(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listUnlocationTest3() {
        // Mock
        when(iMasterDataService.listUnlocation(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listUnlocation(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void stateBasedListTest() {
        // Mock
        when(iMasterDataService.stateBasedList(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.stateBasedList(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void stateBasedListTest2() {
        // Mock
        when(iMasterDataService.stateBasedList(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.stateBasedList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void stateBasedListTest3() {
        // Mock
        when(iMasterDataService.stateBasedList(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.stateBasedList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createOrganization(Object)}
     */

    @Test
    void createOrganizationTest() {
        // Mock
        when(iMasterDataService.createOrganization(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createOrganization(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createOrganizationTest2() {
        // Mock
        when(iMasterDataService.createOrganization(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createOrganization(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createOrganizationTest3() {
        // Mock
        when(iMasterDataService.createOrganization(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createOrganization(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateOrganization(Object)}
     */

    @Test
    void updateOrganizationTest() {
        // Mock
        when(iMasterDataService.updateOrganization(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateOrganization(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateOrganizationTest2() {
        // Mock
        when(iMasterDataService.updateOrganization(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateOrganization(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateOrganizationTest3() {
        // Mock
        when(iMasterDataService.updateOrganization(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateOrganization(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listOrgnization(Object)}
     */

    @Test
    void listOrgnizationTest() {
        // Mock
        when(iMasterDataService.listOrganization(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listOrgnization(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listOrgnizationTest2() {
        // Mock
        when(iMasterDataService.listOrganization(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listOrgnization(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listOrgnizationTest3() {
        // Mock
        when(iMasterDataService.listOrganization(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listOrgnization(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#userList(Object)}
     */

    @Test
    void userListTest() {
        // Mock
        when(iMasterDataService.listUsers(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.userList(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void userListTest2() {
        // Mock
        when(iMasterDataService.listUsers(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.userList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void userListTest3() {
        // Mock
        when(iMasterDataService.listUsers(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.userList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#createGridColorCode(Object)}
     */

    @Test
    void createGridColorCodeTest() {
        // Mock
        when(iMasterDataService.createGridColorCode(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.createGridColorCode(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createGridColorCodeTest2() {
        // Mock
        when(iMasterDataService.createGridColorCode(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.createGridColorCode(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void createGridColorCodeTest3() {
        // Mock
        when(iMasterDataService.createGridColorCode(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.createGridColorCode(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#updateGridColorCOde(Object)}
     */

    @Test
    void updateGridColorCOdeTest() {
        // Mock
        when(iMasterDataService.updateGridColorCode(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.updateGridColorCOde(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateGridColorCOdeTest2() {
        // Mock
        when(iMasterDataService.updateGridColorCode(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.updateGridColorCOde(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateGridColorCOdeTest3() {
        // Mock
        when(iMasterDataService.updateGridColorCode(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.updateGridColorCOde(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#gridColorList(Object)}
     */

    @Test
    void gridColorListTest() {
        // Mock
        when(iMasterDataService.listGridColorCode(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.gridColorList(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void gridColorListTest2() {
        // Mock
        when(iMasterDataService.listGridColorCode(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.gridColorList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void gridColorListTest3() {
        // Mock
        when(iMasterDataService.listGridColorCode(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.gridColorList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listCousinBranch(Object)}
     */

    @Test
    void listCousinBranchTest() {
        // Mock
        when(iMasterDataService.listCousinBranches(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listCousinBranch(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchTest2() {
        // Mock
        when(iMasterDataService.listCousinBranches(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listCousinBranch(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchTest3() {
        // Mock
        when(iMasterDataService.listCousinBranches(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listCousinBranch(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listCousinBranchWithoutCurrent(Object)}
     */

    @Test
    void listCousinBranchWithoutCurrentTest() {
        // Mock
        when(iMasterDataService.listCousinBranchesWithoutCurrent(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listCousinBranchWithoutCurrent(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchWithoutCurrentTest2() {
        // Mock
        when(iMasterDataService.listCousinBranchesWithoutCurrent(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listCousinBranchWithoutCurrent(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchWithoutCurrentTest3() {
        // Mock
        when(iMasterDataService.listCousinBranchesWithoutCurrent(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listCousinBranchWithoutCurrent(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#scheduleList(Object)}
     */

    @Test
    void scheduleListTest() {
        // Mock
        when(iMasterDataService.listSailingSchedule(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.scheduleList(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void scheduleListTest2() {
        // Mock
        when(iMasterDataService.listSailingSchedule(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.scheduleList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void scheduleListTest3() {
        // Mock
        when(iMasterDataService.listSailingSchedule(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.scheduleList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#importSchedule(Object)}
     */

    @Test
    void importScheduleTest() {
        // Mock
        when(iMasterDataService.importSailingSchedules(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.importSchedule(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importScheduleTest2() {
        // Mock
        when(iMasterDataService.importSailingSchedules(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.importSchedule(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importScheduleTest3() {
        // Mock
        when(iMasterDataService.importSailingSchedules(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.importSchedule(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#importFlightSchedule(Object)}
     */

    @Test
    void importFlightScheduleTest() {
        // Mock
        when(iMasterDataService.importFlightSchedules(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.importFlightSchedule(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void importFlightScheduleTest2() {
        // Mock
        when(iMasterDataService.importFlightSchedules(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.importFlightSchedule(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void importFlightScheduleTest3() {
        // Mock
        when(iMasterDataService.importFlightSchedules(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.importFlightSchedule(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#fetchFlightStatus(Object)}
     */

    @Test
    void fetchFlightStatusTest() {
        // Mock
        when(iMasterDataService.fetchFlightStatus(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.fetchFlightStatus(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchFlightStatusTest2() {
        // Mock
        when(iMasterDataService.fetchFlightStatus(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.fetchFlightStatus(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchFlightStatusTest3() {
        // Mock
        when(iMasterDataService.fetchFlightStatus(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.fetchFlightStatus(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#tenantNameByid(Object)}
     */

    @Test
    void tenantNameByidTest() {
        // Mock
        when(iMasterDataService.tenantNameByTenantId(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.tenantNameByid(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void tenantNameByidTest2() {
        // Mock
        when(iMasterDataService.tenantNameByTenantId(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.tenantNameByid(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void tenantNameByidTest3() {
        // Mock
        when(iMasterDataService.tenantNameByTenantId(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.tenantNameByid(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#addressList(Object)}
     */

    @Test
    void addressListTest() {
        // Mock
        when(iMasterDataService.addressList(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.addressList(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void addressListTest2() {
        // Mock
        when(iMasterDataService.addressList(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.addressList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void addressListTest3() {
        // Mock
        when(iMasterDataService.addressList(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.addressList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#fetchUnlocationOriginAndDestinationList(Object)}
     */

    @Test
    void fetchUnlocationOriginAndDestinationListTest() {
        // Mock
        when(iMasterDataService.fetchUnlocationOriginAndDestinationList(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.fetchUnlocationOriginAndDestinationList(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchUnlocationOriginAndDestinationListTest2() {
        // Mock
        when(iMasterDataService.fetchUnlocationOriginAndDestinationList(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.fetchUnlocationOriginAndDestinationList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchUnlocationOriginAndDestinationListTest3() {
        // Mock
        when(iMasterDataService.fetchUnlocationOriginAndDestinationList(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.fetchUnlocationOriginAndDestinationList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#fetchListUnlocationTransportModeBased(Object)}
     */

    @Test
    void fetchListUnlocationTransportModeBasedTest() {
        // Mock
        when(iMasterDataService.fetchListUnlocationTransportModeBased(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.fetchListUnlocationTransportModeBased(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchListUnlocationTransportModeBasedTest2() {
        // Mock
        when(iMasterDataService.fetchListUnlocationTransportModeBased(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.fetchListUnlocationTransportModeBased(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchListUnlocationTransportModeBasedTest3() {
        // Mock
        when(iMasterDataService.fetchListUnlocationTransportModeBased(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.fetchListUnlocationTransportModeBased(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#fetchActivityMaster(Object)}
     */

    @Test
    void fetchActivityMasterTest() {
        // Mock
        when(iMasterDataService.fetchActivityMaster(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.fetchActivityMaster(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchActivityMasterTest2() {
        // Mock
        when(iMasterDataService.fetchActivityMaster(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.fetchActivityMaster(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchActivityMasterTest3() {
        // Mock
        when(iMasterDataService.fetchActivityMaster(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.fetchActivityMaster(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listOwnType(Object)}
     */

    @Test
    void listOwnTypeTest() {
        // Mock
        when(iMasterDataService.listOwnType(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listOwnType(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listOwnTypeTest2() {
        // Mock
        when(iMasterDataService.listOwnType(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listOwnType(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listOwnTypeTest3() {
        // Mock
        when(iMasterDataService.listOwnType(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listOwnType(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#carrierFilterList(Object)}
     */

    @Test
    void carrierFilterListTest() {
        // Mock
        when(iMasterDataService.listCarrierFilter(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.carrierFilterList(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void carrierFilterListTest2() {
        // Mock
        when(iMasterDataService.listCarrierFilter(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.carrierFilterList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void carrierFilterListTest3() {
        // Mock
        when(iMasterDataService.listCarrierFilter(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.carrierFilterList(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#getMainPageTemplate(Object)}
     */

    @Test
    void getMainPageTemplateTest() {
        // Mock
        when(iMasterDataService.fetchGetTemplateMainPage(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.getMainPageTemplate(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getMainPageTemplateTest2() {
        // Mock
        when(iMasterDataService.fetchGetTemplateMainPage(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.getMainPageTemplate(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getMainPageTemplateTest3() {
        // Mock
        when(iMasterDataService.fetchGetTemplateMainPage(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.getMainPageTemplate(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#listRole(Object)}
     */

    @Test
    void listRoleTest() {
        // Mock
        when(iMasterDataService.listRoles(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listRole(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listRoleTest2() {
        // Mock
        when(iMasterDataService.listRoles(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listRole(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listRoleTest3() {
        // Mock
        when(iMasterDataService.listRoles(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listRole(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#fetchChargeTypes(Object)}
     */

    @Test
    void fetchChargeTypesTest() {
        // Mock
        when(iMasterDataService.fetchChargeTypes(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.fetchChargeTypes(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchChargeTypesTest2() {
        // Mock
        when(iMasterDataService.fetchChargeTypes(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.fetchChargeTypes(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchChargeTypesTest3() {
        // Mock
        when(iMasterDataService.fetchChargeTypes(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.fetchChargeTypes(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#getDefaultOrg(Object)}
     */

    @Test
    void getDefaultOrgTest() {
        // Mock
        when(iMasterDataService.getDefaultOrg(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.getDefaultOrg();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getDefaultOrgTest2() {
        // Mock
        when(iMasterDataService.getDefaultOrg(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.getDefaultOrg();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getDefaultOrgTest3() {
        // Mock
        when(iMasterDataService.getDefaultOrg(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.getDefaultOrg();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    /**
     * Method under test: {@link MasterDataController#tenantSettings(Object)}
     */

    @Test
    void tenantSettingsTest() {
        // Mock

        when(iMasterDataService.retrieveTenantSettings()).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.tenantSettings();
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void tenantSettingsTest2() {
        // Mock
        when(iMasterDataService.retrieveTenantSettings()).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.tenantSettings();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void tenantSettingsTest3() {
        // Mock
        when(iMasterDataService.retrieveTenantSettings()).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.tenantSettings();
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchMultipleMasterData() {
        // Mock
        when(iMasterDataService.fetchMultipleMasterData(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.fetchMultipleMasterData(new MasterListRequestV2());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchMultipleMasterData2() {
        // Mock
        when(iMasterDataService.fetchMultipleMasterData(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.fetchMultipleMasterData(new MasterListRequestV2());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchMultipleMasterData3() {
        // Mock
        when(iMasterDataService.fetchMultipleMasterData(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.fetchMultipleMasterData(new MasterListRequestV2());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listOrgsTest() {
        // Mock
        when(iMasterDataService.listOrgs(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listOrgs(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listOrgsTest2() {
        // Mock
        when(iMasterDataService.listOrgs(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listOrgs(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listOrgsTest3() {
        // Mock
        when(iMasterDataService.listOrgs(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listOrgs(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listBranchesByDefaultOrgAndAddressTest() {
        // Mock
        when(iMasterDataService.listBranchesByDefaultOrgAndAddress(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        //Test
        var responseEntity = masterDataController.listBranchesByDefaultOrgAndAddress(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listBranchesByDefaultOrgAndAddressTest2() {
        // Mock
        when(iMasterDataService.listBranchesByDefaultOrgAndAddress(any())).thenThrow(new RuntimeException());
        //Test
        var responseEntity = masterDataController.listBranchesByDefaultOrgAndAddress(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listBranchesByDefaultOrgAndAddressTest3() {
        // Mock
        when(iMasterDataService.listBranchesByDefaultOrgAndAddress(any())).thenThrow(new RuntimeException("RuntimeException"));
        //Test
        var responseEntity = masterDataController.listBranchesByDefaultOrgAndAddress(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchForEtReassignTest() {
        // Mock
        when(iMasterDataService.listCousinBranchForEt(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listCousinBranchForNTEReassign(new ListCousinBranchesForEtRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchForEtReassignTest2() {
        // Mock
        when(iMasterDataService.listCousinBranchForEt(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listCousinBranchForNTEReassign(new ListCousinBranchesForEtRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchForEtReassignTest3() {
        // Mock
        when(iMasterDataService.listCousinBranchForEt(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listCousinBranchForNTEReassign(new ListCousinBranchesForEtRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void listCousinBranchForEtTest() {
        // Mock
        when(iMasterDataService.listCousinBranchForEt(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.listCousinBranchForEt(new ListCousinBranchesForEtRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchForEtTest2() {
        // Mock
        when(iMasterDataService.listCousinBranchForEt(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.listCousinBranchForEt(new ListCousinBranchesForEtRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCousinBranchForEtTest3() {
        // Mock
        when(iMasterDataService.listCousinBranchForEt(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.listCousinBranchForEt(new ListCousinBranchesForEtRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getDefaultOrgAddressFromTenantTest() {
        // Mock
        when(iMasterDataService.getDefaultOrgAddressByTenantId(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = masterDataController.getDefaultOrgAddressFromTenant(new Object());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getDefaultOrgAddressFromTenantTest2() {
        // Mock
        when(iMasterDataService.getDefaultOrgAddressByTenantId(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = masterDataController.getDefaultOrgAddressFromTenant(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getDefaultOrgAddressFromTenantTest3() {
        // Mock
        when(iMasterDataService.getDefaultOrgAddressByTenantId(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = masterDataController.getDefaultOrgAddressFromTenant(new Object());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}
