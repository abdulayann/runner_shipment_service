package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.HblConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.HblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.HblGenerateRequest;
import com.dpw.runner.shipment.services.dto.request.HblRequest;
import com.dpw.runner.shipment.services.dto.request.HblResetRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.response.HblResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CompanySettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.HblReset;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HblServiceTest {

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShipmentDetails testShipment;
    private static Hbl mockHbl;
    @Mock
    private HblDao hblDao;
    @InjectMocks
    private HblService hblService;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private IV1Service v1Service;
    @Mock
    private IHblSync hblSync;
    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private IShipmentService shipmentService;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        testShipment = jsonTestUtility.getTestShipment();
        testShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        testShipment.setDirection(Constants.DIRECTION_EXP);
        testShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        mockHbl = jsonTestUtility.getJson("HBL", Hbl.class);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).build());
    }

    @Test
    void create() {
        HblRequest hblRequest = new HblRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);

        Hbl mockHbl = Hbl.builder().build();
        HblDataDto hblData = objectMapper.convertValue(mockHbl, HblDataDto.class);
        mockHbl.setHblData(hblData);

        // Mock
        when(hblDao.save(any())).thenReturn(mockHbl);
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(hblData);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);

        // Test
        ResponseEntity httpResponse = hblService.create(commonRequestModel);

    }

    @Test
    void retrieveById() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);


        Hbl mockHbl = Hbl.builder().build();
        mockHbl.setHblData(new HblDataDto());
        mockHbl.setId(1L);

        // Mock
        when(hblDao.findById(1L)).thenReturn(Optional.of(mockHbl));
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);

        // Test
        ResponseEntity httpResponse = hblService.retrieveById(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);

    }

    @Test
    void checkAllContainerAssignedWhenContainerNumberMissingInInuptList() {

        // Shipment, Lis<Container>, List<Packing>
        ShipmentDetails inputShipment = testShipment;
        List<Containers> inputContainers = List.of(new Containers());
        List<Packing> inputPacking = null;


        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, inputPacking);

        // Assert
        assertNull(responseHbl);
    }

    @Test
    void checkAllContainerAssignedWhenHblIsAlreadyGeneratedSuccess() {

        // Shipment, Lis<Container>, List<Packing>
        Long shipmentId = 1L;
        ShipmentDetails inputShipment = testShipment;
        testShipment.setId(shipmentId);
        String containerNumber = "CONT12345";
        Containers container = new Containers();
        container.setId(1L);
        container.setContainerNumber(containerNumber);
        List<Containers> inputContainers = List.of(container);
        Packing packing = new Packing();
        packing.setContainerId(1L);
        List<Packing> inputPacking = List.of(packing);


        HblCargoDto hblCargoDto = new HblCargoDto();
        hblCargoDto.setBlContainerContainerNumber(containerNumber);

        // Mock
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of(mockHbl));
        when(hblDao.save(any())).thenReturn(mockHbl.setHblCargo(List.of(hblCargoDto)));

        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, inputPacking);

        // Assert
        assertEquals(mockHbl, responseHbl);
    }

    @Test
    void checkAllContainerAssignedWhenHblIsNotGenerated() {

        TenantContext.setCurrentTenant(1);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetails.builder().build();

        // Shipment, Lis<Container>, List<Packing>
        ShipmentDetails inputShipment = testShipment;
        Long shipmentId = 1L;
        testShipment.setId(shipmentId);
        String containerNumber = "CONT12345";
        Containers container = new Containers();
        container.setId(1L);
        container.setContainerNumber(containerNumber);
        List<Containers> inputContainers = List.of(container);
        Packing packing = new Packing();
        packing.setContainerId(1L);
        List<Packing> inputPacking = List.of(packing);


        HblCargoDto hblCargoDto = new HblCargoDto();
        hblCargoDto.setBlContainerContainerNumber(containerNumber);

        // Mock
        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of());
//        when(shipmentService.generateCustomHouseBL(any())).thenReturn("custom-house-bl");
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(shipmentSettingsDetails));
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of());
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(new HashMap<>());
        when(v1Service.retrieveCompanySettings()).thenReturn(new CompanySettingsResponse());
        when(hblDao.save(any())).thenReturn(mockHbl);

        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, inputPacking);

        // Assert
        assertNotNull(responseHbl);
    }

    @Test
    void checkAllContainerAssignedReturnsNullWhenRestrictHblGenIsTrue() {

        TenantContext.setCurrentTenant(1);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetails.builder().restrictHblGen(true).build();

        // Shipment, Lis<Container>, List<Packing>
        ShipmentDetails inputShipment = testShipment;
        inputShipment.setId(1L);
        String containerNumber = "CONT12345";
        Containers container = new Containers();
        container.setId(1L);
        container.setContainerNumber(containerNumber);
        List<Containers> inputContainers = List.of(container);
        Packing packing = new Packing();
        packing.setContainerId(1L);
        List<Packing> inputPacking = List.of(packing);


        HblCargoDto hblCargoDto = new HblCargoDto();
        hblCargoDto.setBlContainerContainerNumber(containerNumber);

        // Mock
        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of());
        when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(shipmentSettingsDetails));

        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, inputPacking);

        // Assert
        assertNull(responseHbl);
    }

    @Test
    void generateHblSuccess() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");


        // Mock
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of());
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(new HashMap<>());
        when(v1Service.retrieveCompanySettings()).thenReturn(new CompanySettingsResponse());
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.generateHBL(commonRequestModel);

        // Assert
        verify(hblSync, times(1)).sync(any(), anyString());
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void generateHblThrowsExceptionWhenShipmentIsNotPresent() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        String errorMessage = DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;

        // Mock
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.empty());

        // Test
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.generateHBL(commonRequestModel));

        // Verify
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void generateHblThrowsExceptionWhenHblIsAlreadyGeneratedForShipment() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        String errorMessage = String.format(HblConstants.HBL_DATA_FOUND, testShipment.getShipmentId());

        // Mock
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of(mockHbl));

        // Test
        Exception e = assertThrows(ValidationException.class, () -> hblService.generateHBL(commonRequestModel));

        // Verify
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void generateHblThrowsExceptionWhenContainerNumberIsMissing() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        String errorMessage = "Please assign container number to all the containers before generating the HBL.";

        List<Containers> containersList = List.of(new Containers());
        testShipment.setContainersList(containersList);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);

        // Mock
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));

        // Test
        Exception e = assertThrows(ValidationException.class, () -> hblService.generateHBL(commonRequestModel));

        // Verify
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void generateHblThrowsExceptionWhenPackIsNotLinkedWithContainer() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        String errorMessage = "Container Number is Mandatory for HBL Generation, please assign the container number for all the packages in the shipment.";

        List<Packing> packingList = List.of(new Packing());
        testShipment.setPackingList(packingList);
        testShipment.setShipmentType(Constants.CARGO_TYPE_FCL);

        // Mock
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));

        // Test
        Exception e = assertThrows(ValidationException.class, () -> hblService.generateHBL(commonRequestModel));

        // Verify
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void resetHblFailsWhenHblNotPresentInDb() throws RunnerException {
        Long hblId = 1L;
        HblResetRequest resetRequest = new HblResetRequest();
        resetRequest.setId(hblId);
        resetRequest.setResetType(HblReset.ALL);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        String errorMessage = DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.empty());

        // Test
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.resetHbl(commonRequestModel));

        // Assert
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void resetHblFailsWhenShipmentNotPresentInDb() throws RunnerException {
        Long hblId = 1L;
        HblResetRequest resetRequest = new HblResetRequest();
        resetRequest.setId(hblId);
        resetRequest.setResetType(HblReset.ALL);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        String errorMessage = DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.of(mockHbl));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.empty());

        // Test
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.resetHbl(commonRequestModel));

        // Assert
        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void resetHblAllSuccess() throws RunnerException {
        Long hblId = 1L;
        HblResetRequest resetRequest = new HblResetRequest();
        resetRequest.setId(hblId);
        resetRequest.setResetType(HblReset.ALL);
        testShipment.setHouseBill("houseBill");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.of(mockHbl));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(testShipment));
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.resetHbl(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }


    private IRunnerResponse convertEntityToDto(Hbl hbl) {
        HblResponse response = objectMapper.convertValue(hbl.getHblData(), HblResponse.class);
        response.setCargoes(hbl.getHblCargo());
        response.setContainers(hbl.getHblContainer());
        response.setNotifyParties(hbl.getHblNotifyParty());
        response.setId(hbl.getId());
        response.setGuid(hbl.getGuid());
        return response;
    }

}