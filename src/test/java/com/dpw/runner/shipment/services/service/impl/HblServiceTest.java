package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.HblConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.impl.HblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.HblGenerateRequest;
import com.dpw.runner.shipment.services.dto.request.HblRequest;
import com.dpw.runner.shipment.services.dto.request.HblResetRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
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
import com.dpw.runner.shipment.services.service.interfaces.IHblService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.HblRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HblServiceTest extends CommonMocks {

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShipmentDetails testShipment;
    private static Hbl mockHbl;
    private static ShipmentDetails completeShipment;
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
    @Mock
    private SyncConfig syncConfig;
    @Mock
    private PartialFetchUtils partialFetchUtils;
    @Mock
    private ConsolidationService consolidationService;


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
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).build());
        completeShipment = jsonTestUtility.getCompleteShipment();
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
    public void testHblContainersWithoutContainerNumber_ShouldThrowException() {
        Hbl hblObject = new Hbl();
        HblContainerDto hblContainerWithoutNumber = new HblContainerDto();
        hblContainerWithoutNumber.setContainerNumber(null);
        hblObject.setHblContainer(List.of(hblContainerWithoutNumber));

        assertThrows(ValidationException.class, () ->
                hblService.validateHblContainerNumberCondition(hblObject),
            "Please assign container number to all the containers in HBL before generating the HBL."
        );
    }

    @Test
    public void testHblCargosWithoutContainerNumber_ShouldThrowException() {
        Hbl hblObject = new Hbl();
        HblCargoDto hblCargoWithoutContainerNumber = new HblCargoDto();
        hblCargoWithoutContainerNumber.setBlContainerContainerNumber(null);
        hblObject.setHblCargo(List.of(hblCargoWithoutContainerNumber));

        assertThrows(ValidationException.class, () ->
                hblService.validateHblContainerNumberCondition(hblObject),
            "Container Number is Mandatory for HBL Generation, please assign the container number for all the HBLCargo in the shipment."
        );
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
    void checkAllContainerAssignedWhenContainerNumberEmptyInInuptList() {

        // Shipment, Lis<Container>, List<Packing>
        ShipmentDetails inputShipment = testShipment;
        List<Containers> inputContainers = List.of(Containers.builder().containerNumber(StringUtility.getEmptyString()).build());
        List<Packing> inputPacking = null;
        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, inputPacking);
        // Assert
        assertNull(responseHbl);
    }

    @Test
    void checkAllContainerAssignedWhenFindShipmentEmpty() {
        // Shipment, Lis<Container>, List<Packing>
        ShipmentDetails inputShipment = testShipment;
        inputShipment.setId(11L);
        List<Containers> inputContainers = List.of(Containers.builder().containerNumber(StringUtility.getRandomString(1)).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(false).build());
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.empty());
        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of());
        mockShipmentSettings();
        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, null);
        // Assert
        assertNull(responseHbl);
    }

    @Test
    void checkAllContainerAssignedWhenNoContainerIsPresentInBL() {
        ShipmentDetails inputShipment = completeShipment;
        inputShipment.setId(11L);
        Hbl inputHBL = mockHbl;
        inputHBL.setHblContainer(List.of());
        List<Containers> inputContainers = List.of(Containers.builder().containerNumber(StringUtility.getRandomString(1)).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of(inputHBL));
        when(hblDao.save(any())).thenReturn(inputHBL);
        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, completeShipment.getPackingList());
        // Assert
        assertEquals(inputHBL.getId(), responseHbl.getId());
    }

    @Test
    void checkAllContainerAssignedWhenNoContainerIsPresentInShipment() {
        ShipmentDetails inputShipment = completeShipment;
        inputShipment.setId(11L);
        inputShipment.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        inputShipment.setContainersList(List.of());
        Hbl inputHBL = mockHbl;
        inputHBL.setHblContainer(List.of());
        List<Containers> inputContainers = List.of(Containers.builder().containerNumber(StringUtility.getRandomString(1)).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of(inputHBL));
        when(hblDao.save(any())).thenReturn(inputHBL);
        when(v1Service.retrieveCompanySettings()).thenReturn(CompanySettingsResponse.builder().SeaLclContainerFlag(true).build());
        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, completeShipment.getPackingList());
        // Assert
        assertEquals(inputHBL.getId(), responseHbl.getId());
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

        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, inputPacking);

        // Assert
        assertEquals(1L, responseHbl.getShipmentId());
    }

    @Test
    void checkAllContainerAssignedWhenHblIsNotGenerated() {

        TenantContext.setCurrentTenant(1);

        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
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
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of());
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(new HashMap<>());
        when(v1Service.retrieveCompanySettings()).thenReturn(new CompanySettingsResponse());
        when(hblDao.save(any())).thenReturn(mockHbl);
        mockShipmentSettings();
        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, inputPacking);

        // Assert
        assertNotNull(responseHbl);
    }

    @Test
    void checkAllContainerAssignedReturnsNullWhenRestrictHblGenIsTrue() {

        TenantContext.setCurrentTenant(1);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictHblGen(true).build());
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
        mockShipmentSettings();
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
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(false);

        // Mock
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of());
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(new HashMap<>());
        when(v1Service.retrieveCompanySettings()).thenReturn(new CompanySettingsResponse());
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.generateHBL(commonRequestModel);

        // Assert
        verify(hblSync, times(1)).sync(any(), anyString());
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void generateHblSuccess2() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(true);


        // Mock
        addDataForAutomaticTransfer(testShipment);
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of());
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(new HashMap<>());
        when(v1Service.retrieveCompanySettings()).thenReturn(new CompanySettingsResponse());
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));
        mockShipmentSettings();
        doNothing().when(consolidationService).triggerAutomaticTransfer(any(), any(), anyBoolean());

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.generateHBL(commonRequestModel);

        // Assert
        verify(hblSync, times(1)).sync(any(), anyString());
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void generateHblSuccess3() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(true);


        // Mock
        addDataForAutomaticTransfer(testShipment);
        testShipment.setConsolidationList(null);
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of());
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(new HashMap<>());
        when(v1Service.retrieveCompanySettings()).thenReturn(new CompanySettingsResponse());
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.generateHBL(commonRequestModel);

        // Assert
        verify(hblSync, times(1)).sync(any(), anyString());
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void generateHblSuccess4() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(true);


        // Mock
        addDataForAutomaticTransfer(testShipment);
        testShipment.getConsolidationList().get(0).setConsolidationType(Constants.CONSOLIDATION_TYPE_DRT);
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of());
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(new HashMap<>());
        when(v1Service.retrieveCompanySettings()).thenReturn(new CompanySettingsResponse());
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.generateHBL(commonRequestModel);

        // Assert
        verify(hblSync, times(1)).sync(any(), anyString());
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void generateHblSuccess5() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(true);


        // Mock
        addDataForAutomaticTransfer(testShipment);
        testShipment.getConsolidationList().get(0).setTransportMode(Constants.TRANSPORT_MODE_AIR);
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(hblDao.findByShipmentId(shipmentId)).thenReturn(List.of());
        when(masterDataUtils.fetchInBulkUnlocations(any(), anyString())).thenReturn(new HashMap<>());
        when(v1Service.retrieveCompanySettings()).thenReturn(new CompanySettingsResponse());
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.generateHBL(commonRequestModel);

        // Assert
        verify(hblSync, times(1)).sync(any(), anyString());
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void generateHblSuccessWithSyncFailed() throws RunnerException {
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
        when(hblSync.sync(any(), anyString())).thenThrow(new RuntimeException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
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
        testShipment.setJobType(Constants.SHIPMENT_TYPE_DRT);

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
        testShipment.setJobType(Constants.SHIPMENT_TYPE_DRT);

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
        completeShipment.setHouseBill("houseBill");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.of(mockHbl));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(completeShipment));
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.resetHbl(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void resetHblDataSuccess() throws RunnerException {
        Long hblId = 1L;
        HblResetRequest resetRequest = new HblResetRequest();
        resetRequest.setId(hblId);
        resetRequest.setResetType(HblReset.HBL_DATA);
        completeShipment.setHouseBill("houseBill");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.of(mockHbl));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(completeShipment));
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.resetHbl(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void resetHblDataWithOutHblNumberSuccess() throws RunnerException {
        Long hblId = 1L;
        HblResetRequest resetRequest = new HblResetRequest();
        resetRequest.setId(hblId);
        resetRequest.setResetType(HblReset.HBL_DATA);
        var inputShipment = completeShipment;
        inputShipment.setHouseBill(null);
        inputShipment.getAdditionalDetails().setImportBroker(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.of(mockHbl));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(inputShipment));
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.resetHbl(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void resetHblCargoesSuccess() throws RunnerException {
        Long hblId = 1L;
        HblResetRequest resetRequest = new HblResetRequest();
        resetRequest.setId(hblId);
        resetRequest.setResetType(HblReset.HBL_CARGOES);
        completeShipment.setHouseBill("houseBill");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.of(mockHbl));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(completeShipment));
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.resetHbl(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void resetHblContainersSuccess() throws RunnerException {
        Long hblId = 1L;
        HblResetRequest resetRequest = new HblResetRequest();
        resetRequest.setId(hblId);
        resetRequest.setResetType(HblReset.HBL_CONTAINERS);
        completeShipment.setHouseBill("houseBill");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.of(mockHbl));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(completeShipment));
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData() , HblResponse.class));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.resetHbl(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(mockHbl)), httpResponse);
    }

    @Test
    void resetHblNotifyPartiesSuccess() throws RunnerException {
        Long hblId = 1L;
        HblResetRequest resetRequest = new HblResetRequest();
        resetRequest.setId(hblId);
        resetRequest.setResetType(HblReset.HBL_PARTIES);
        completeShipment.setHouseBill("houseBill");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetRequest);

        // Mock
        when(hblDao.findById(hblId)).thenReturn(Optional.of(mockHbl));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(completeShipment));
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

    @Test
    void createWithEmptyData() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        Hbl mockHbl = Hbl.builder().build();
        HblDataDto hblData = objectMapper.convertValue(mockHbl, HblDataDto.class);
        mockHbl.setHblData(hblData);
        // Mock
        when(hblDao.save(any())).thenReturn(mockHbl);
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        // Test
        ResponseEntity httpResponse = hblService.create(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void createHblThrowsException() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        // Mock
        when(hblDao.save(any())).thenThrow(new RuntimeException("22323"));
        // Test
        ResponseEntity httpResponse = hblService.create(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void testUpdateHBL() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        Hbl mockHbl = getHblModel();
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(mockHbl));
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(mockHbl.getHblData());
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);

        ResponseEntity httpResponse = hblService.update(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void updateHblWIthHblSyncFailed() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        Hbl mockHbl = getHblModel();
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(mockHbl));
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(hblSync.sync(any(), anyString())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(mockHbl.getHblData());
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);

        ResponseEntity httpResponse = hblService.update(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void updateHblWIthHblSaveFailed() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        Hbl mockHbl = getHblModel();
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(mockHbl));
        when(hblDao.save(any())).thenThrow(new RuntimeException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(mockHbl.getHblData());

        ResponseEntity httpResponse = hblService.update(commonRequestModel);
        // Test
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void updateHblWIthHblSaveFailedWithoutExceptionMessage() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        Hbl mockHbl = getHblModel();
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(mockHbl));
        when(hblDao.save(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(mockHbl.getHblData());

        ResponseEntity httpResponse = hblService.update(commonRequestModel);
        // Test
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void testUpdateHBLWithException() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.empty());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.update(commonRequestModel));
        // Assert
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }

    @Test
    void retrieveByIdFailureCase() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.empty());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.retrieveById(commonRequestModel));
        // Assert
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }

    @Test
    void retrieveByIdWithIncludeColumns() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().includeColumns(List.of("String")).id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        Hbl mockHbl = getHblModel();
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        when(partialFetchUtils.fetchPartialListData(any(), any())).thenReturn(any());
        var responseEntity = hblService.retrieveById(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByIdWithEmptyIncludeColumns() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().includeColumns(List.of()).id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        Hbl mockHbl = getHblModel();
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        var responseEntity = hblService.retrieveById(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBLWithRestrictHBLTest() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictBLEdit(true).build());
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(mockHbl));
        mockShipmentSettings();
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.partialUpdateHBL(commonRequestModel));
        // Assert
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }

    @Test
    void partialUpdateException() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.empty());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.partialUpdateHBL(commonRequestModel));
        // Assert
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }

    @Test
    void partialUpdateExceptionForHbl() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of());

        Exception e = assertThrows(ValidationException.class, () -> hblService.partialUpdateHBL(commonRequestModel));
        // Assert
        assertEquals(ValidationException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void partialUpdateHBLWithRestrictHBLFalseTest() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder()
                        .autoUpdateShipmentBL(true)
                        .hblLockSettings(jsonTestUtility.getJson("HBL_LOCK_ALL_FALSE", HblLockSettings.class))
                        .build());
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(completeShipment));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        when(hblDao.save(any())).thenReturn(mockHbl);
        mockShipmentSettings();
        var responseEntity = hblService.partialUpdateHBL(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBLWithSyncFailed() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder()
                        .hblLockSettings(jsonTestUtility.getJson("HBL_LOCK_ALL_FALSE", HblLockSettings.class))
                        .build());
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(completeShipment));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        when(hblSync.sync(any(), anyString())).thenThrow(new RuntimeException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        mockShipmentSettings();
        var responseEntity = hblService.partialUpdateHBL(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBLWithRestrictHBLFalseWithoutHblNotifyParty() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder()
                        .autoUpdateShipmentBL(true)
                        .hblLockSettings(jsonTestUtility.getJson("HBL_LOCK_ALL_FALSE", HblLockSettings.class))
                        .build());
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        var inputHbl = mockHbl;
        inputHbl.setHblNotifyParty(null);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(completeShipment));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(inputHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        when(hblDao.save(any())).thenReturn(inputHbl);
        mockShipmentSettings();
        var responseEntity = hblService.partialUpdateHBL(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBLWithRestrictHBLFalseWithoutHblContainers() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder()
                        .autoUpdateShipmentBL(true)
                        .hblLockSettings(jsonTestUtility.getJson("HBL_LOCK_ALL_FALSE", HblLockSettings.class))
                        .build());
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        var inputHbl = mockHbl;
        inputHbl.setHblContainer(null);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(completeShipment));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(inputHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        when(hblDao.save(any())).thenReturn(inputHbl);
        mockShipmentSettings();
        var responseEntity = hblService.partialUpdateHBL(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBLWithRestrictHBLFalseWithoutHblCargoes() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder()
                        .autoUpdateShipmentBL(true)
                        .hblLockSettings(jsonTestUtility.getJson("HBL_LOCK_ALL_FALSE", HblLockSettings.class))
                        .build());
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        var inputHbl = mockHbl;
        inputHbl.setHblCargo(null);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(completeShipment));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(inputHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        when(hblDao.save(any())).thenReturn(inputHbl);
        mockShipmentSettings();
        var responseEntity = hblService.partialUpdateHBL(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBLWithRestrictHBLFalseWithoutShipmentNotifyParty() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(
                ShipmentSettingsDetails.builder()
                        .autoUpdateShipmentBL(true)
                        .hblLockSettings(jsonTestUtility.getJson("HBL_LOCK_ALL_FALSE", HblLockSettings.class))
                        .build());
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        var inputShipment = completeShipment;
        inputShipment.getAdditionalDetails().setNotifyParty(null);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(inputShipment));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        when(hblDao.save(any())).thenReturn(mockHbl);
        mockShipmentSettings();
        var responseEntity = hblService.partialUpdateHBL(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void partialUpdateHBLWithSyncFailedTest() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.of(ShipmentDetails.builder().build()));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(mockHbl));
        when(hblSync.sync(any(), anyString())).thenReturn(new ResponseEntity<>(HttpStatus.OK));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        mockShipmentSettings();
        // test
        var responseEntity =  hblService.partialUpdateHBL(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    private Hbl getHblModel() {
        return Hbl.builder()
                .shipmentId(10L)
                .hblData(new HblDataDto())
                .hblCargo(List.of(new HblCargoDto()))
                .hblContainer(List.of(new HblContainerDto()))
                .build();
    }

    @Test
    void createHblWithExceptionInSyncTest() {
        HblRequest hblRequest = new HblRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);

        Hbl mockHbl = Hbl.builder().build();
        HblDataDto hblData = objectMapper.convertValue(mockHbl, HblDataDto.class);
        mockHbl.setHblData(hblData);
        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.save(any())).thenReturn(mockHbl);
        when(hblSync.sync(any(), anyString())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(hblData);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        // Test
        ResponseEntity httpResponse = hblService.create(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void createHblWithExceptionInSaveTest() {
        HblRequest hblRequest = new HblRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);

        Hbl mockHbl = Hbl.builder().build();
        HblDataDto hblData = objectMapper.convertValue(mockHbl, HblDataDto.class);
        mockHbl.setHblData(hblData);
        // Mock
        when(hblDao.save(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(hblData);
        // Test
        ResponseEntity httpResponse = hblService.create(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void listTest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        ResponseEntity httpResponse = hblService.list(commonRequestModel);
        // Test
        assertEquals(null, httpResponse);
    }

    @Test
    void listAsyncTest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        var httpResponse = hblService.listAsync(commonRequestModel);
        // Test
        assertEquals(null, httpResponse);
    }

    @Test
    void deleteTest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        var httpResponse = hblService.delete(commonRequestModel);
        // Test
        assertEquals(null, httpResponse);
    }

    @Test
    void saveV1HblTestWithEmptyRequest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(new HblRequestV2());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.saveV1Hbl(commonRequestModel, false));
        // Test
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), e.getClass().getSimpleName());
    }

//    @Test
    void saveV1HblWithSyncServiceTest() throws RunnerException {
        var hblRequestV2 = new HblRequestV2();
        hblRequestV2.setShipmentGuid(UUID.randomUUID());
        syncConfig.IS_REVERSE_SYNC_ACTIVE = false;

        // Test
        var responseEntity = hblService.saveV1Hbl(CommonRequestModel.buildRequest(hblRequestV2), true);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void saveV1HblWithEmptyShipment() {
        var hblRequestV2 = new HblRequestV2();
        hblRequestV2.setShipmentGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequestV2);

        when(shipmentDao.findAll(any(), any())).thenReturn(Page.empty());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.saveV1Hbl(commonRequestModel, false));
        // Test
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void saveV1Hbl() throws RunnerException {
        var hblRequestV2 = new HblRequestV2();
        hblRequestV2.setShipmentGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequestV2);

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<ShipmentDetails>(Arrays.asList(completeShipment)));
        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class));
        when(jsonHelper.convertValue(any(), eq(HblRequest.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData(), HblRequest.class));
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(mockHbl.getHblData());

        when(hblDao.save(any())).thenReturn(mockHbl);
        var responseEntity = hblService.saveV1Hbl(commonRequestModel, false);
        // Test
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void saveV1HblWithoutHBL() throws RunnerException {
        var hblRequestV2 = new HblRequestV2();
        hblRequestV2.setShipmentGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequestV2);

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<ShipmentDetails>(Arrays.asList(completeShipment)));
        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of());
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class));
        when(jsonHelper.convertValue(any(), eq(HblRequest.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData(), HblRequest.class));
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(mockHbl.getHblData());

        when(hblDao.save(any())).thenReturn(mockHbl);
        var responseEntity = hblService.saveV1Hbl(commonRequestModel, false);
        // Test
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void saveV1HblException() throws RunnerException {
        var hblRequestV2 = new HblRequestV2();
        hblRequestV2.setShipmentGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequestV2);

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<ShipmentDetails>(Arrays.asList(completeShipment)));
        when(hblDao.findByShipmentId(anyLong())).thenThrow(new RuntimeException());

        Exception e = assertThrows(RuntimeException.class, () -> hblService.saveV1Hbl(commonRequestModel, false));
        // Test
        assertEquals(RuntimeException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void saveV1HblExceptionWithMessage() throws RunnerException {
        var hblRequestV2 = new HblRequestV2();
        hblRequestV2.setShipmentGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequestV2);

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<ShipmentDetails>(Arrays.asList(completeShipment)));
        when(hblDao.findByShipmentId(anyLong())).thenThrow(new RuntimeException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));

        Exception e = assertThrows(RuntimeException.class, () -> hblService.saveV1Hbl(commonRequestModel, false));
        // Test
        assertEquals(RuntimeException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void retrieveByShipmentIdTestWithEmptyHbl() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictBLEdit(true).build());

        var responseEntity = hblService.retrieveByShipmentId(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByShipmentIdTestWithHblInResponse() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().restrictBLEdit(true).build());

        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class));
        var responseEntity = hblService.retrieveByShipmentId(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByShipmentIdTestWithAutoUpdateShipmentBL() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoUpdateShipmentBL(true).build());

        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class));
        var responseEntity = hblService.retrieveByShipmentId(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByShipmentIdTestWithoutAutoUpdateShipmentBL() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoUpdateShipmentBL(false).restrictBLEdit(false).build());

        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of(mockHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class));
        var responseEntity = hblService.retrieveByShipmentId(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByShipmentIdWithShipmentDetailsNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());

        var responseEntity = hblService.retrieveByShipmentId(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void generateHblSuccess1() {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        testShipment.setContainsHazardous(true);
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        assertThrows(ValidationException.class, () -> hblService.generateHBL(commonRequestModel));
    }

    private void addDataForAutomaticTransfer(ShipmentDetails shipment) {
        shipment.setMasterBill("MBL123");
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        consolidationDetails.setConsolidationType(Constants.SHIPMENT_TYPE_STD);

        shipment.setConsolidationList(List.of(consolidationDetails));
    }

}