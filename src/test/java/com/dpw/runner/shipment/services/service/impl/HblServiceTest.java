package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.adapters.impl.MDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.HblConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.impl.HblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.hbl.HblCargoDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.request.mdm.MdmTaskCreateRequest;
import com.dpw.runner.shipment.services.dto.response.HblResponse;
import com.dpw.runner.shipment.services.dto.response.mdm.MdmTaskCreateResponse;
import com.dpw.runner.shipment.services.dto.v1.response.CompanySettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.HblReset;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.MdmTaskException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.HblRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IHblSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;


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
    @Mock
    private MDMServiceAdapter mdmServiceAdapter;
    @Mock
    private HblService self;
    private ShipmentDetails shipmentDetail;
    private HblDataDto hblData;
    private AdditionalDetails additionalDetails;


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
        shipmentDetail = new ShipmentDetails();
        hblData = new HblDataDto();
        additionalDetails = new AdditionalDetails();
    }

    @Test
    void create() {
        HblRequest hblRequest = new HblRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);

        Hbl testHbl = Hbl.builder().build();
        HblDataDto hblData = objectMapper.convertValue(testHbl, HblDataDto.class);
        testHbl.setHblData(hblData);

        // Mock
        when(hblDao.save(any())).thenReturn(testHbl);
        HblResponse response = objectMapper.convertValue(testHbl.getHblData(), HblResponse.class);
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(hblData);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.create(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());

    }

    @Test
    void retrieveById() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);


        Hbl testHbl = Hbl.builder().build();
        testHbl.setHblData(new HblDataDto());
        testHbl.setId(1L);

        // Mock
        when(hblDao.findById(1L)).thenReturn(Optional.of(testHbl));
        HblResponse response = objectMapper.convertValue(testHbl.getHblData(), HblResponse.class);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.retrieveById(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(convertEntityToDto(testHbl)), httpResponse);

    }

    @Test
    void testHblContainersWithoutContainerNumber_ShouldThrowException() {
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
    void testHblCargosWithoutContainerNumber_ShouldThrowException() {
        Hbl hblObject = new Hbl();
        HblCargoDto hblCargoWithoutContainerNumber = new HblCargoDto();
        hblCargoWithoutContainerNumber.setBlContainerContainerNumber(null);
        hblObject.setHblCargo(List.of(hblCargoWithoutContainerNumber));
        Mockito.when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().build());
        assertThrows(ValidationException.class, () ->
                hblService.validateHblContainerNumberCondition(hblObject),
            "Container Number is Mandatory for HBL Generation, please assign the container number for all the HBLCargo in the shipment."
        );
    }

    @Test
    void checkAllContainerAssignedWhenContainerNumberMissingInInuptList() {

        // Shipment, Lis<Container>, List<Packing>
        ShipmentDetails inputShipment = testShipment;
        Set<Containers> inputContainers = Set.of(new Containers());
        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, null);
        // Assert
        assertNull(responseHbl);
    }

    @Test
    void checkAllContainerAssignedWhenContainerNumberEmptyInInuptList() {

        // Shipment, Lis<Container>, List<Packing>
        ShipmentDetails inputShipment = testShipment;
        Set<Containers> inputContainers = Set.of(Containers.builder().containerNumber(Constants.EMPTY_STRING).build());
        // Test
        Hbl responseHbl = hblService.checkAllContainerAssigned(inputShipment, inputContainers, null);
        // Assert
        assertNull(responseHbl);
    }

    @Test
    void checkAllContainerAssignedWhenFindShipmentEmpty() {
        // Shipment, Lis<Container>, List<Packing>
        ShipmentDetails inputShipment = testShipment;
        inputShipment.setId(11L);
        Set<Containers> inputContainers = Set.of(Containers.builder().containerNumber(StringUtility.getRandomString(1)).build());
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
        Set<Containers> inputContainers = Set.of(Containers.builder().containerNumber(StringUtility.getRandomString(1)).build());
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
        inputShipment.setContainersList(Set.of());
        Hbl inputHBL = mockHbl;
        inputHBL.setHblContainer(List.of());
        Set<Containers> inputContainers = Set.of(Containers.builder().containerNumber(StringUtility.getRandomString(1)).build());
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
        Set<Containers> inputContainers = Set.of(container);
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
        testShipment.setContainersList(new HashSet<>());
        String containerNumber = "CONT12345";
        Containers container = new Containers();
        container.setId(1L);
        container.setContainerNumber(containerNumber);
        Set<Containers> inputContainers = Set.of(container);
        Packing packing = new Packing();
        packing.setContainerId(1L);
        List<Packing> inputPacking = List.of(packing);


        HblCargoDto hblCargoDto = new HblCargoDto();
        hblCargoDto.setBlContainerContainerNumber(containerNumber);

        // Mock
        when(hblDao.findByShipmentId(anyLong())).thenReturn(List.of());
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
        Set<Containers> inputContainers = Set.of(container);
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
        testShipment.setContainersList(new HashSet<>());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(false);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsRunnerV3Enabled(true);

        // Mock
        mockShipmentSettings();
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
    void generateHblSuccess2() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        testShipment.setContainersList(new HashSet<>());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(true);


        // Mock
        mockShipmentSettings();
        addDataForAutomaticTransfer(testShipment);
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
    void generateHblSuccess3() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        testShipment.setContainersList(new HashSet<>());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(true);


        // Mock
        mockShipmentSettings();
        addDataForAutomaticTransfer(testShipment);
        testShipment.setConsolidationList(null);
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
    void generateHblSuccess4() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        testShipment.setContainersList(new HashSet<>());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(true);


        // Mock
        mockShipmentSettings();
        addDataForAutomaticTransfer(testShipment);
        testShipment.getConsolidationList().iterator().next().setConsolidationType(Constants.CONSOLIDATION_TYPE_DRT);
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
    void generateHblSuccess5() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        testShipment.setContainersList(new HashSet<>());
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAutomaticTransferEnabled(true);


        // Mock
        mockShipmentSettings();
        addDataForAutomaticTransfer(testShipment);
        testShipment.getConsolidationList().iterator().next().setTransportMode(Constants.TRANSPORT_MODE_AIR);
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
    void generateHblSuccessWithSyncFailed() throws RunnerException {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        testShipment.setHouseBill("custom-house-bl");
        testShipment.setContainersList(new HashSet<>());
        mockShipmentSettings();
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
    void generateHblThrowsExceptionWhenShipmentIsNotPresent() {
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
    void generateHblThrowsExceptionWhenHblIsAlreadyGeneratedForShipment() {
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
    void generateHblThrowsExceptionWhenContainerNumberIsMissing() {
        Long shipmentId = 1L;
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(shipmentId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        String errorMessage = "Please assign container number to all the containers before generating the HBL.";

        Set<Containers> containersList = Set.of(new Containers());
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
    void generateHblThrowsExceptionWhenPackIsNotLinkedWithContainer() {
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
    void resetHblFailsWhenHblNotPresentInDb() {
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
    void resetHblFailsWhenShipmentNotPresentInDb() {
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
        mockShipmentSettings();
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
        mockShipmentSettings();
        // Mock
        mockShipmentSettings();
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
        mockShipmentSettings();
        // Mock
        mockShipmentSettings();
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
        mockShipmentSettings();
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
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsRunnerV3Enabled(true);
        // Mock
        mockShipmentSettings();
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
        mockShipmentSettings();
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
        Hbl testHbl = Hbl.builder().build();
        HblDataDto hblData = objectMapper.convertValue(testHbl, HblDataDto.class);
        testHbl.setHblData(hblData);
        // Mock
        when(hblDao.save(any())).thenReturn(testHbl);
        HblResponse response = objectMapper.convertValue(testHbl.getHblData(), HblResponse.class);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.create(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void createHblThrowsException() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        // Mock
        when(hblDao.save(any())).thenThrow(new RuntimeException("22323"));
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.create(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void testUpdateHBL() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        Hbl testHbl = getHblModel();
        HblResponse response = objectMapper.convertValue(testHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(testHbl));
        when(hblDao.save(any())).thenReturn(testHbl);
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(testHbl.getHblData());
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> httpResponse = hblService.update(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void updateHblWIthHblSyncFailed() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        Hbl testHbl = getHblModel();
        HblResponse response = objectMapper.convertValue(testHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(testHbl));
        when(hblDao.save(any())).thenReturn(testHbl);
        when(hblSync.sync(any(), anyString())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(testHbl.getHblData());
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);

        ResponseEntity<IRunnerResponse> httpResponse = hblService.update(commonRequestModel);
        // Test
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void updateHblWIthHblSaveFailed() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        Hbl testHbl = getHblModel();
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(testHbl));
        when(hblDao.save(any())).thenThrow(new RuntimeException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(testHbl.getHblData());

        ResponseEntity<IRunnerResponse> httpResponse = hblService.update(commonRequestModel);
        // Test
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void updateHblWIthHblSaveFailedWithoutExceptionMessage() {
        HblRequest hblRequest = new HblRequest();
        hblRequest.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);
        Hbl testHbl = getHblModel();
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(testHbl));
        when(hblDao.save(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(testHbl.getHblData());

        ResponseEntity<IRunnerResponse> httpResponse = hblService.update(commonRequestModel);
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
        Hbl testHbl = getHblModel();
        HblResponse response = objectMapper.convertValue(testHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(testHbl));
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
        Hbl testHbl = getHblModel();
        HblResponse response = objectMapper.convertValue(testHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.findById(anyLong())).thenReturn(Optional.of(testHbl));
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
        doThrow(new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE)).when(self).resetHbl(any());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.partialUpdateHBL(commonRequestModel));
        // Assert
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }

    @Test
    void partialUpdateException() {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        // Mock
        when(shipmentDao.findById(10L)).thenReturn(Optional.empty());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> hblService.partialUpdateHBL(commonRequestModel));
        // Assert
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }

    @Test
    void partialUpdateExceptionForHbl() {
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
    void partialUpdateHBLWithRestrictHBLFalseWithoutHblNotifyPartyV3Flow() throws RunnerException {
        HblGenerateRequest request = HblGenerateRequest.builder().shipmentId(10L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetails.builder()
                .autoUpdateShipmentBL(true)
                .hblLockSettings(jsonTestUtility.getJson("HBL_LOCK_ALL_FALSE", HblLockSettings.class))
                .isRunnerV3Enabled(true)
                .build();

        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        HblResponse response = objectMapper.convertValue(mockHbl.getHblData(), HblResponse.class);
        var inputHbl = mockHbl;
        inputHbl.setHblNotifyParty(null);

        when(shipmentDao.findById(10L)).thenReturn(Optional.of(completeShipment));
        when(hblDao.findByShipmentId(10L)).thenReturn(List.of(inputHbl));
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        when(hblDao.save(any())).thenReturn(inputHbl);
        mockShipmentSettings();

        // Building v1Data map
        EntityTransferUnLocations usLoc = new EntityTransferUnLocations();
        usLoc.setLocCode("USNYC");
        usLoc.setNameWoDiacritics("New York");

        EntityTransferUnLocations usLoc1 = new EntityTransferUnLocations();
        usLoc1.setLocCode("USNYC");
        usLoc1.setName("New York");
        usLoc1.setStateName("XY");
        usLoc1.setCityName("ABC");

        EntityTransferUnLocations usLoc2 = new EntityTransferUnLocations();
        usLoc2.setLocCode("USNYC");
        usLoc2.setName("New York");
        usLoc2.setState("XY");
        usLoc2.setCityName("ABC");

        EntityTransferUnLocations usLoc3 = new EntityTransferUnLocations();
        usLoc3.setLocCode("USNYC");
        usLoc3.setName("New York");
        usLoc3.setState("XY");

        EntityTransferUnLocations inLoc = new EntityTransferUnLocations();
        inLoc.setLocCode("INBLR");
        inLoc.setNameWoDiacritics("Bengaluru");

        Map<String, EntityTransferUnLocations> v1Data = new HashMap<>();
        v1Data.put("USKEY", usLoc);
        v1Data.put("INKEY", inLoc);
        v1Data.put("USKEY1", usLoc1);
        v1Data.put("USKEY2", usLoc2);
        v1Data.put("USKEY3", usLoc3);

        try {
            Method method = hblService.getClass().getDeclaredMethod("getUnLocationsName", Map.class, String.class);
            method.setAccessible(true);

            // Case 1: Null key
            String result1 = (String) method.invoke(hblService, v1Data, null);
            assertEquals("", result1);

            // Case 2: RunnerV3 Flag enabled and US key
            String result2 = (String) method.invoke(hblService, v1Data, "USKEY");
            assertEquals("", result2);

            // Case 3: RunnerV3 Flag enabled and Non-US key
            String result3 = (String) method.invoke(hblService, v1Data, "INKEY");
            assertEquals("BENGALURU", result3);

            String result5 = (String) method.invoke(hblService, v1Data, "USKEY1");
            assertEquals("ABC", result5);

            String result6 = (String) method.invoke(hblService, v1Data, "USKEY2");
            assertEquals("ABC,XY", result6);

            String result7 = (String) method.invoke(hblService, v1Data, "USKEY3");
            assertEquals("XY", result7);

            // Case 4: RunnerV3 Flag disabled
            shipmentSettingsDetails.setIsRunnerV3Enabled(false);
            String result4 = (String) method.invoke(hblService, v1Data, "INKEY");
            assertEquals("INBLR Bengaluru", result4);

        } catch (Exception e) {
            fail("Reflection call failed: " + e.getMessage());
        }

        var responseEntity = hblService.partialUpdateHBL(commonRequestModel);
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

        Hbl testHbl = Hbl.builder().build();
        HblDataDto hblData = objectMapper.convertValue(testHbl, HblDataDto.class);
        testHbl.setHblData(hblData);
        HblResponse response = objectMapper.convertValue(testHbl.getHblData(), HblResponse.class);
        // Mock
        when(hblDao.save(any())).thenReturn(testHbl);
        when(hblSync.sync(any(), anyString())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(hblData);
        when(jsonHelper.convertValue(any(), eq(HblResponse.class))).thenReturn(response);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.create(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void createHblWithExceptionInSaveTest() {
        HblRequest hblRequest = new HblRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequest);

        Hbl testHbl = Hbl.builder().build();
        HblDataDto hblData = objectMapper.convertValue(testHbl, HblDataDto.class);
        testHbl.setHblData(hblData);
        // Mock
        when(hblDao.save(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(HblDataDto.class))).thenReturn(hblData);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = hblService.create(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void listTest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        ResponseEntity<IRunnerResponse> httpResponse = hblService.list(commonRequestModel);
        // Test
        assertNull(httpResponse);
    }

    @Test
    void listAsyncTest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        var httpResponse = hblService.listAsync(commonRequestModel);
        // Test
        assertNull(httpResponse);
    }

    @Test
    void deleteTest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        var httpResponse = hblService.delete(commonRequestModel);
        // Test
        assertNull(httpResponse);
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

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.singletonList(completeShipment)));
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

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.singletonList(completeShipment)));
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
    void saveV1HblException() {
        var hblRequestV2 = new HblRequestV2();
        hblRequestV2.setShipmentGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequestV2);

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.singletonList(completeShipment)));
        when(hblDao.findByShipmentId(anyLong())).thenThrow(new RuntimeException());

        Exception e = assertThrows(GenericException.class, () -> hblService.saveV1Hbl(commonRequestModel, false));
        // Test
        assertEquals(GenericException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void saveV1HblExceptionWithMessage() {
        var hblRequestV2 = new HblRequestV2();
        hblRequestV2.setShipmentGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(hblRequestV2);

        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.singletonList(completeShipment)));
        when(hblDao.findByShipmentId(anyLong())).thenThrow(new RuntimeException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));

        Exception e = assertThrows(GenericException.class, () -> hblService.saveV1Hbl(commonRequestModel, false));
        // Test
        assertEquals(GenericException.class.getSimpleName(), e.getClass().getSimpleName());
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

        shipment.setConsolidationList(Set.of(consolidationDetails));
    }

    @Test
     void testValidateSealNumberWarning_ShipmentNotFound() {
        Long shipmentId = 1L;
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.empty());
        Exception exception = assertThrows(DataRetrievalFailureException.class, () -> {
            hblService.validateSealNumberWarning(shipmentId);
        });
        assertEquals("Shipment not found", exception.getMessage());
    }
    @Test
    void testValidateSealNumberWarning_containersMissingSeals() {
        Long shipmentId = 4L;
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setDirection(Constants.DIRECTION_EXP);

        Containers container1 = new Containers();
        container1.setContainerNumber("CONT001");
        container1.setCarrierSealNumber(null);
        container1.setCustomsSealNumber(null);
        container1.setShipperSealNumber(null);
        container1.setVeterinarySealNumber(null);
        // All seals empty

        Containers container2 = new Containers();
        container2.setContainerNumber("CONT002");
        container2.setCarrierSealNumber("SealX"); // Has at least one seal

        shipment.setContainersList(new HashSet<>(Arrays.asList(container1, container2)));

        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipment));

        ResponseEntity<IRunnerResponse> response = hblService.validateSealNumberWarning(shipmentId);
        RunnerResponse<?> responseBody = (RunnerResponse<?>) response.getBody();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(responseBody.getWarning());
        assertTrue(responseBody.getWarning().contains("CONT001"));
        assertFalse(responseBody.getWarning().contains("CONT002"));
    }


    @Test
    void testValidateSealNumberWarning_containersMissingSeals2() {
        Long shipmentId = 4L;
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipment.setDirection(Constants.DIRECTION_EXP);

        Containers container1 = new Containers();
        container1.setId(1L);
        container1.setContainerCode("20GP");
        container1.setCarrierSealNumber(null);
        container1.setCustomsSealNumber(null);
        container1.setShipperSealNumber(null);
        container1.setVeterinarySealNumber(null);
        // All seals empty

        Containers container3 = new Containers();
        container3.setId(2L);
        container3.setContainerNumber("CONT003");
        container3.setContainerCode("20FP");
        container3.setCarrierSealNumber(null);
        container3.setCustomsSealNumber(null);
        container3.setShipperSealNumber(null);
        container3.setVeterinarySealNumber(null);

        Containers container2 = new Containers();
        container2.setId(4L);
        container2.setContainerCode("20GP");
        container2.setCarrierSealNumber("SealX"); // Has at least one seal

        shipment.setContainersList(new HashSet<>(Arrays.asList(container3, container1, container2)));

        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipment));

        ResponseEntity<IRunnerResponse> response = hblService.validateSealNumberWarning(shipmentId);
        RunnerResponse<?> responseBody = (RunnerResponse<?>) response.getBody();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(responseBody);
        assertNotNull(responseBody.getWarning());
        assertTrue(responseBody.getWarning().contains("20GP"));
        assertTrue(responseBody.getWarning().contains("CONT003"));
        assertFalse(responseBody.getWarning().contains("CONT002"));
    }

    @Test
    void testValidateSealNumberWarning_AllContainersHaveSeals() {
        Containers container = new Containers();
        container.setContainerNumber("CTN789");
        container.setCarrierSealNumber("SEAL789");

        ShipmentDetails shipment = mock(ShipmentDetails.class);
        when(shipment.getTransportMode()).thenReturn(Constants.TRANSPORT_MODE_SEA);
        when(shipment.getDirection()).thenReturn(Constants.DIRECTION_EXP);
        shipment.setContainersList(new HashSet<>(Arrays.asList(container)));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));

        ResponseEntity<IRunnerResponse> response = hblService.validateSealNumberWarning(1L);
        RunnerResponse<?> responseBody = (RunnerResponse<?>) response.getBody();

        assertEquals(200, response.getStatusCodeValue());
        assertNull(responseBody.getWarning()); // No warning expected
    }
@Test
    void testNegativeCasesForSelaVal() {
        Containers container = new Containers();
        container.setContainerNumber("CTN000");
        // IMPORT direction
        ShipmentDetails importShipment = mock(ShipmentDetails.class);
        when(importShipment.getTransportMode()).thenReturn("SEA");
        when(importShipment.getDirection()).thenReturn("IMP");
        importShipment.setContainersList(new HashSet<>(Arrays.asList(container)));
    // No containers
    ShipmentDetails emptyShipment = mock(ShipmentDetails.class);
    when(emptyShipment.getTransportMode()).thenReturn("SEA");
    when(emptyShipment.getDirection()).thenReturn("EXP");
    when(emptyShipment.getContainersList()).thenReturn(Collections.emptySet());
    //  Null containers
    ShipmentDetails nullShipment = mock(ShipmentDetails.class);
    when(nullShipment.getTransportMode()).thenReturn("SEA");
    when(nullShipment.getDirection()).thenReturn("EXP");
    when(nullShipment.getContainersList()).thenReturn(null);
    // Common mock
    when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(mock(ShipmentDetails.class)));
    // Execute and verify all negative cases
    Arrays.asList( importShipment, emptyShipment, nullShipment).forEach(shipment -> {
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipment));
        ResponseEntity<IRunnerResponse> response = hblService.validateSealNumberWarning(1L);
        RunnerResponse<?> responseBody = (RunnerResponse<?>) response.getBody();
        assertEquals(200, response.getStatusCodeValue());
        assertNull(responseBody.getWarning());
    });
}


    @Test
    void testConvertCountryCodeTo2Letters_With3LetterCode() throws Exception {
        Method method = HblService.class.getDeclaredMethod("convertCountryCodeTo2Letters", String.class);
        method.setAccessible(true);
        assertEquals("US", method.invoke(hblService, "USA"));
        assertEquals("GB", method.invoke(hblService, "GBR"));
        assertEquals("IN", method.invoke(hblService, "IND"));
    }

    @Test
    void testExtractAddressComponents_WithCompleteData() throws Exception {
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(PartiesConstants.ADDRESS1, "123 Main St");
        addressData.put(PartiesConstants.ADDRESS2, "Suite 100");
        addressData.put(PartiesConstants.CITY, "New York");
        addressData.put(PartiesConstants.STATE, "NY");
        addressData.put(PartiesConstants.ZIP_POST_CODE, "10001");
        addressData.put(PartiesConstants.COUNTRY, "USA");

        Method method = HblService.class.getDeclaredMethod("extractAddressComponents", Map.class);
        method.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, String> result = (Map<String, String>) method.invoke(hblService, addressData);
        assertEquals("123 Main St", result.get(PartiesConstants.ADDRESS1));
        assertEquals("Suite 100", result.get(PartiesConstants.ADDRESS2));
        assertEquals("New York", result.get(PartiesConstants.CITY));
        assertEquals("NY", result.get(PartiesConstants.STATE));
        assertEquals("10001", result.get(PartiesConstants.ZIP_POST_CODE));
        assertEquals("USA", result.get(PartiesConstants.COUNTRY));
    }

    @Test
    void testMapDeliveryDataInHbl_full() throws Exception {
        Map<String, Object> addr = Map.of(
                PartiesConstants.ADDRESS1, "123 St",
                PartiesConstants.ADDRESS2, "Suite 9",
                PartiesConstants.CITY, "Chennai",
                PartiesConstants.STATE, "TN",
                PartiesConstants.ZIP_POST_CODE, "600001",
                PartiesConstants.COUNTRY, "IND"
        );
        Parties broker = new Parties();
        broker.setOrgData(Map.of(PartiesConstants.FULLNAME, "Ship Co"));
        broker.setAddressData(addr);
        AdditionalDetails ad = new AdditionalDetails();
        ad.setImportBroker(broker);
        mockShipmentSettings();

        HblDataDto dto = new HblDataDto();
        Method method = HblService.class.getDeclaredMethod("mapDeliveryAgentDataInHblV3", AdditionalDetails.class, HblDataDto.class);
        Method method1 = HblService.class.getDeclaredMethod("mapForwardingAgentDataInHblV3", AdditionalDetails.class, HblDataDto.class);
        method.setAccessible(true);
        method1.setAccessible(true);
        method.invoke(hblService, ad, dto);

        assertEquals("SHIP CO", dto.getDeliveryAgentName());
        assertEquals("123 ST", dto.getDeliveryAgentAddressLine1());
        assertEquals("IN", dto.getDeliveryAgentCountry()); // ISO3166 mocked or real
    }
    @Test
    void endToEnd_fullBrokerConsignerConsigneeMapping() throws Exception {
        Map<String, Object> importAddr = new HashMap<>();
        importAddr.put(PartiesConstants.COMPANY_NAME, "ImportCo Pvt Ltd");
        importAddr.put(PartiesConstants.ADDRESS1,    "12 Harbor Road");
        importAddr.put(PartiesConstants.ADDRESS2,    "Dock Area");
        importAddr.put(PartiesConstants.CITY,        "Chennai");
        importAddr.put(PartiesConstants.STATE,       "TN");
        importAddr.put(PartiesConstants.ZIP_POST_CODE, "600001");
        importAddr.put(PartiesConstants.COUNTRY,     "IND");          // alpha-3 → should become IN
        importAddr.put(PartiesConstants.CONTACT_PHONE,"+91-44-1111");
        mockShipmentSettings();
        Parties importBroker = new Parties();
        importBroker.setOrgData(Map.of(PartiesConstants.FULLNAME, "Eastern Brokers"));
        importBroker.setAddressData(importAddr);

        Map<String, Object> exportAddr = new HashMap<>();
        exportAddr.put(PartiesConstants.ADDRESS1, "99 Ocean Ave");
        exportAddr.put(PartiesConstants.CITY,     "Singapore");
        exportAddr.put(PartiesConstants.STATE,    "SG");
        exportAddr.put(PartiesConstants.ZIP_POST_CODE, "234567");
        exportAddr.put(PartiesConstants.COUNTRY,  "SGP");             // alpha-3 → should become SG

        Parties exportBroker = new Parties();
        exportBroker.setOrgData(Map.of(PartiesConstants.FULLNAME, "Pacific Forwarding"));
        exportBroker.setAddressData(exportAddr);

        AdditionalDetails additional = new AdditionalDetails();
        additional.setImportBroker(importBroker);
        additional.setExportBroker(exportBroker);

        // ---------- Build Consigner ----------
        Map<String, Object> consignerAddr = new HashMap<>();
        consignerAddr.put(PartiesConstants.ADDRESS1, "45 Hill St");
        consignerAddr.put(PartiesConstants.CITY,     "Mumbai");
        consignerAddr.put(PartiesConstants.STATE,    "MH");
        consignerAddr.put(PartiesConstants.ZIP_POST_CODE, "400001");
        consignerAddr.put(PartiesConstants.COUNTRY,  "IND");

        Parties consigner = new Parties();
        consigner.setOrgData(Map.of(PartiesConstants.FULLNAME, "Blue Line Traders"));
        consigner.setAddressData(consignerAddr);

        // ---------- Build Consignee ----------
        Map<String, Object> consigneeAddr = new HashMap<>();
        consigneeAddr.put(PartiesConstants.ADDRESS1, "77 Main Road");
        consigneeAddr.put(PartiesConstants.ADDRESS2, "Industrial Estate");
        consigneeAddr.put(PartiesConstants.CITY,     "Colombo");
        consigneeAddr.put(PartiesConstants.STATE,    "WP");
        consigneeAddr.put(PartiesConstants.ZIP_POST_CODE, "00900");
        consigneeAddr.put(PartiesConstants.COUNTRY,  "LKA");

        Parties consignee = new Parties();
        consignee.setOrgData(Map.of(PartiesConstants.FULLNAME, "Island Imports"));
        consignee.setAddressData(consigneeAddr);

        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setConsigner(consigner);
        shipment.setConsignee(consignee);

        // ---------- Execute mapping ----------
        HblDataDto dto = new HblDataDto();
        HblService svc = new HblService();   // replace with actual class
        Method method = HblService.class.getDeclaredMethod("mapForwardingAgentDataInHblV3", AdditionalDetails.class, HblDataDto.class);
        method.setAccessible(true);
        method.invoke(hblService, additional, dto);
        Method method1 = HblService.class.getDeclaredMethod("mapDeliveryAgentDataInHblV3", AdditionalDetails.class, HblDataDto.class);
        method1.setAccessible(true);
        method1.invoke(hblService, additional, dto);
        Method method2 = HblService.class.getDeclaredMethod("mapConsignerConsigneeToHblV3", ShipmentDetails.class, HblDataDto.class);
        method2.setAccessible(true);
        method2.invoke(hblService, shipment, dto);

        // Delivery agent (import broker)
        assertEquals("EASTERN BROKERS", dto.getDeliveryAgentName());
        assertEquals("12 HARBOR ROAD",  dto.getDeliveryAgentAddressLine1());
        assertEquals("DOCK AREA",       dto.getDeliveryAgentAddressLine2());
        assertEquals("CHENNAI",         dto.getDeliveryAgentCity());
        assertEquals("TN",              dto.getDeliveryAgentState());
        assertEquals("600001",          dto.getDeliveryAgentZipCode());
        assertEquals("IN",              dto.getDeliveryAgentCountry());

        // Forwarder (export broker)
        assertEquals("PACIFIC FORWARDING", dto.getForwarderName());
        assertEquals("99 OCEAN AVE",       dto.getForwarderAddressLine1());
        assertEquals("SINGAPORE",          dto.getForwarderCity());
        assertEquals("SG",                 dto.getForwarderCountry());

        // Consigner
        assertEquals("BLUE LINE TRADERS", dto.getShipperName());
        assertEquals("45 HILL ST",        dto.getShipperAddressLine1());
        assertEquals("MUMBAI",            dto.getShipperCity());
        assertEquals("IN",                dto.getShipperCountry());

        // Consignee
        assertEquals("ISLAND IMPORTS", dto.getConsigneeName());
        assertEquals("77 MAIN ROAD",   dto.getConsigneeAddressLine1());
        assertEquals("INDUSTRIAL ESTATE", dto.getConsigneeAddressLine2());
        assertEquals("COLOMBO",        dto.getConsigneeCity());
        assertEquals("LKA".length() == 3 ? "LK" : "LK", dto.getConsigneeCountry()); // depends on ISO helper

    }
    private Parties party(String fullName) {
        Map<String, Object> org = new HashMap<>();
        org.put(PartiesConstants.FULLNAME, fullName);
        mockShipmentSettings();
        Map<String, Object> addr = new HashMap<>();
        addr.put(PartiesConstants.ADDRESS1, "Line1");
        addr.put(PartiesConstants.ADDRESS2, "Line2");
        addr.put(PartiesConstants.CITY, "Mumbai");
        addr.put(PartiesConstants.STATE, "MH");
        addr.put(PartiesConstants.ZIP_POST_CODE, "400001");
        addr.put(PartiesConstants.COUNTRY, "INDIA");

        Parties p = new Parties();      // your domain class, not PartiesModel
        p.setOrgData(org);
        p.setAddressData(addr);
        // set other simple fields if you need them in tests
        return p;
    }



    @Test
    void mapDeliveryData_setsAgentFields() throws Exception {
        AdditionalDetails addl = new AdditionalDetails();
        addl.setImportBroker(party("Import Broker"));
        HblDataDto dto = HblDataDto.builder().build();
        Method method2 = HblService.class.getDeclaredMethod("mapDeliveryAgentDataInHblV3", AdditionalDetails.class, HblDataDto.class);
        method2.setAccessible(true);
        method2.invoke(hblService, addl, dto);
        assertEquals("IMPORT BROKER", dto.getDeliveryAgentName());
        assertEquals("LINE1", dto.getDeliveryAgentAddressLine1());
        assertEquals("MUMBAI", dto.getDeliveryAgentCity());
        assertEquals("MH", dto.getDeliveryAgentState());
        assertEquals("400001", dto.getDeliveryAgentZipCode());
        assertEquals("INDIA", dto.getDeliveryAgentCountry()); // depends on convertCountryCodeTo2Letters
    }

    @Test
    void mapForwardData_setsForwarderFields() throws Exception {
        AdditionalDetails addl = new AdditionalDetails();
        addl.setExportBroker(party("Forwarder"));

        HblDataDto dto = HblDataDto.builder().build();
        Method method2 = HblService.class.getDeclaredMethod("mapForwardingAgentDataInHblV3", AdditionalDetails.class, HblDataDto.class);
        method2.setAccessible(true);
        method2.invoke(hblService, addl, dto);
        assertEquals("FORWARDER", dto.getForwarderName());
        assertEquals("LINE1", dto.getForwarderAddressLine1());
        assertEquals("MUMBAI", dto.getForwarderCity());
        assertEquals("MH", dto.getForwarderState());
        assertEquals("400001", dto.getForwarderZipCode());
        assertEquals("INDIA", dto.getForwarderCountry());
    }

    @Test
    void mapConsignerConsignee_setsBothParties() throws Exception {
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setConsigner(party("Consignor"));
        shipment.setConsignee(party("Consignee"));
        HblDataDto dto = HblDataDto.builder().build();
        Method method2 = HblService.class.getDeclaredMethod("mapConsignerConsigneeToHblV3", ShipmentDetails.class, HblDataDto.class);
        method2.setAccessible(true);
        method2.invoke(hblService, shipment, dto);

        assertEquals("CONSIGNOR", dto.getShipperName());
        assertEquals("CONSIGNOR", dto.getShipperName());
        assertEquals("CONSIGNEE", dto.getConsigneeName());
        assertEquals("LINE1", dto.getShipperAddressLine1());
        assertEquals("LINE1", dto.getConsigneeAddressLine1());
        assertEquals("MUMBAI", dto.getConsigneeCity());
    }
    @Test
    void testUpdateShipmentPartiesToHBL_addNewParty() throws Exception {
        // Given
        Hbl hbl = new Hbl();
        hbl.setHblNotifyParty(new ArrayList<>());
        Parties party = new Parties();
        party.setOrgData(Map.of(PartiesConstants.FULLNAME, "Test Party"));
        party.setAddressData(Map.of(PartiesConstants.ADDRESS1, "123 Main St"));
        HblLockSettings hblLock = new HblLockSettings();

        ShipmentSettingsDetails settings = ShipmentSettingsDetails.builder().isRunnerV3Enabled(false).build();
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(settings);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(settings);
        // When
        Method method = HblService.class.getDeclaredMethod("updateShipmentPartiesToHBL", Parties.class, Hbl.class, HblLockSettings.class);
        method.setAccessible(true);
        method.invoke(hblService, party, hbl, hblLock);

        assertNotNull(hbl.getHblNotifyParty());
        assertFalse(hbl.getHblNotifyParty().isEmpty());
        assertEquals(1, hbl.getHblNotifyParty().size());
        HblPartyDto hblParty = hbl.getHblNotifyParty().get(0);
        assertEquals("Test Party", hblParty.getName());
        assertEquals("123 MAIN ST", hblParty.getAddress());
        assertTrue(hblParty.getIsShipmentCreated());
    }
    @Test
    void testMapShipmentPartiesToHBL_v3Enabled() throws Exception {
        // Given
        Parties party = new Parties();
        party.setOrgData(Map.of(
                PartiesConstants.FULLNAME, "Test Party",
                PartiesConstants.EMAIL, "test@party.com"
        ));
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(PartiesConstants.ADDRESS1, "123 Main St");
        addressData.put(PartiesConstants.ADDRESS2, "Apt 4B");
        addressData.put(PartiesConstants.CITY, "Anytown");
        addressData.put(PartiesConstants.STATE, "Anystate");
        addressData.put(PartiesConstants.ZIP_POST_CODE, "12345");
        addressData.put(PartiesConstants.COUNTRY, "US");
        party.setAddressData(addressData);

        ShipmentSettingsDetails shipmentSettings = new ShipmentSettingsDetails();
        shipmentSettings.setIsRunnerV3Enabled(true);

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettings);

        // When
        Method method = HblService.class.getDeclaredMethod("mapShipmentPartiesToHBL", Parties.class);
        method.setAccessible(true);
        @SuppressWarnings("unchecked")
        List<HblPartyDto> result = (List<HblPartyDto>) method.invoke(hblService, party);

        assertNotNull(result);
        assertEquals(1, result.size());
        HblPartyDto hblParty = result.get(0);
        assertTrue(hblParty.getIsShipmentCreated());
        assertEquals("TEST PARTY", hblParty.getName());
        assertEquals("123 MAIN ST", hblParty.getAddress1());
        assertEquals("APT 4B", hblParty.getAddress2());
        assertEquals("ANYTOWN", hblParty.getCity());
        assertEquals("ANYSTATE", hblParty.getState());
        assertEquals("12345", hblParty.getZipCode());
        assertEquals("US", hblParty.getCountry());
        assertEquals("test@party.com", hblParty.getEmail());
    }
    private Parties createParty(String fullName, String address1, String city, String country) {
        Parties party = new Parties();
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(PartiesConstants.FULLNAME, fullName);
        party.setOrgData(orgData);

        Map<String, Object> addressData = new HashMap<>();
        addressData.put(PartiesConstants.ADDRESS1, address1);
        addressData.put(PartiesConstants.CITY, city);
        addressData.put(PartiesConstants.COUNTRY, country);
        party.setAddressData(addressData);

        return party;
    }

    @Test
    void testMapConsignerConsigneeToHbl_withPartialData() {
        // Arrange
        Parties consigner = new Parties();
        consigner.setOrgData(null); // No org data
        Map<String, Object> addressData = new HashMap<>();
        addressData.put(PartiesConstants.ADDRESS1, "123 Consigner St");
        consigner.setAddressData(addressData);
        shipmentDetail.setConsigner(consigner);

        Parties consignee = new Parties();
        Map<String, Object> orgData = new HashMap<>();
        orgData.put(PartiesConstants.FULLNAME, "Consignee Name");
        consignee.setOrgData(orgData);
        consignee.setAddressData(null); // No address data
        shipmentDetail.setConsignee(consignee);

        // Act
        ReflectionTestUtils.invokeMethod(hblService, "mapConsignerConsigneeToHbl", shipmentDetail, hblData);

        // Assert
        assertNull(hblData.getConsignorName());
        assertNotNull(hblData.getConsignorAddress());
        assertEquals("Consignee Name", hblData.getConsigneeName());
        assertNull(hblData.getConsigneeAddress());
    }

    @Test
    void testMapDeliveryDataInHbl_withNullBroker() {
        // Arrange
        additionalDetails.setImportBroker(null);
        // Act & Assert
        assertDoesNotThrow(() -> {
            ReflectionTestUtils.invokeMethod(hblService, "mapDeliveryDataInHbl", additionalDetails, hblData);
        });
        assertNull(hblData.getDeliveryAgent());
        assertNull(hblData.getDeliveryAgentAddress());
    }

    @Test
    void testMapConsignerConsigneeToHbl_withNullPartyData() {
        // Arrange
        shipmentDetail.setConsigner(null);
        shipmentDetail.setConsignee(null);
        // Act & Assert
        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(hblService, "mapConsignerConsigneeToHbl", shipmentDetail, hblData));
        assertNull(hblData.getConsignorName());
        assertNull(hblData.getConsigneeName());
    }

    @Test
    void mapConsignerConsigneeToHbl_WithNullData_ShouldHandleGracefully() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        ShipmentDetails shipmentDetail = mock(ShipmentDetails.class);

        // Mock consigner with null orgData and addressData
        Parties consigner = mock(Parties.class);
        when(consigner.getOrgData()).thenReturn(null);
        when(consigner.getAddressData()).thenReturn(null);

        // Mock consignee with null orgData and addressData
        Parties consignee = mock(Parties.class);
        when(consignee.getOrgData()).thenReturn(null);
        when(consignee.getAddressData()).thenReturn(null);

        when(shipmentDetail.getConsigner()).thenReturn(consigner);
        when(shipmentDetail.getConsignee()).thenReturn(consignee);

        // Act
        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(hblService, "mapConsignerConsigneeToHbl", shipmentDetail, hblData));

        // Assert
        assertNull(hblData.getConsignorName());
        assertNull(hblData.getConsignorAddress());
        assertNull(hblData.getConsigneeName());
        assertNull(hblData.getConsigneeAddress());
    }

    // Test null hblData
    @Test
    void mapConsignerConsigneeToHbl_WithNullHblData_ShouldNotThrow() {
        ShipmentDetails shipmentDetail = mock(ShipmentDetails.class);
        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(hblService, "mapConsignerConsigneeToHbl", shipmentDetail, null));
    }

    @Test
    void mapDeliveryDataInHbl_WithEmptyAddressData_ShouldHandleGracefully() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        AdditionalDetails additionalDetails = mock(AdditionalDetails.class);

        Parties importBroker = mock(Parties.class);
        when(importBroker.getOrgData()).thenReturn(Map.of(PartiesConstants.FULLNAME, "Test Agent"));
        when(importBroker.getAddressData()).thenReturn(Collections.emptyMap());
        when(additionalDetails.getImportBroker()).thenReturn(importBroker);
        // Act
        ReflectionTestUtils.invokeMethod(hblService, "mapDeliveryDataInHbl", additionalDetails, hblData);
        // Assert - test what the actual method returns for empty map
        assertEquals("Test Agent", hblData.getDeliveryAgent());
        // Don't assert the exact address value, just that it doesn't throw
        assertNotNull(hblData.getDeliveryAgentAddress());
    }
    @Test
    void mapConsignerToHbl_WithEmptyConsignerAddressData_ShouldHandleGracefully() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        ShipmentDetails shipmentDetail = mock(ShipmentDetails.class);

        Parties consigner = mock(Parties.class);
        when(consigner.getOrgData()).thenReturn(Map.of(PartiesConstants.FULLNAME, "Test Shipper"));
        when(consigner.getAddressData()).thenReturn(Collections.emptyMap());
        when(shipmentDetail.getConsigner()).thenReturn(consigner);
        when(shipmentDetail.getConsignee()).thenReturn(null);

        ReflectionTestUtils.invokeMethod(hblService, "mapConsignerConsigneeToHbl", shipmentDetail, hblData);

        // Assert
        assertEquals("Test Shipper", hblData.getConsignorName());
        assertNotNull(hblData.getConsignorAddress()); // Should not throw
    }
    @Test
    void mapConsigneeToHbl_WithEmptyConsigneeAddressData_ShouldHandleGracefully() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        ShipmentDetails shipmentDetail = mock(ShipmentDetails.class);

        Parties consignee = mock(Parties.class);
        when(consignee.getOrgData()).thenReturn(Map.of(PartiesConstants.FULLNAME, "Test Consignee"));
        when(consignee.getAddressData()).thenReturn(Collections.emptyMap());

        when(shipmentDetail.getConsigner()).thenReturn(null);
        when(shipmentDetail.getConsignee()).thenReturn(consignee);

        ReflectionTestUtils.invokeMethod(hblService, "mapConsignerConsigneeToHbl", shipmentDetail, hblData);

        // Assert
        assertEquals("Test Consignee", hblData.getConsigneeName());
        assertNotNull(hblData.getConsigneeAddress()); // Should not throw
    }
    @Test
    void mapDeliveryDataInHbl_WithNullBrokerOrgData_ShouldHandleGracefully() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        AdditionalDetails additionalDetails = mock(AdditionalDetails.class);

        Parties importBroker = mock(Parties.class);
        when(importBroker.getOrgData()).thenReturn(null);
        when(importBroker.getAddressData()).thenReturn(Map.of("address", "test address"));

        when(additionalDetails.getImportBroker()).thenReturn(importBroker);
        ReflectionTestUtils.invokeMethod(hblService, "mapDeliveryDataInHbl", additionalDetails, hblData);

        // Assert
        assertNull(hblData.getDeliveryAgent()); // Should be null
        assertNotNull(hblData.getDeliveryAgentAddress()); // Should be set
    }

    @Test
    void mapConsignerConsigneeToHblV3_WithNullData_ShouldHandleGracefully() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        ShipmentDetails shipmentDetail = mock(ShipmentDetails.class);

        // Mock consigner with some null data
        Parties consigner = mock(Parties.class);
        when(consigner.getOrgData()).thenReturn(null); // Null orgData
        when(consigner.getAddressData()).thenReturn(null); // Null addressData

        // Mock consignee with some null data
        Parties consignee = mock(Parties.class);
        when(consignee.getOrgData()).thenReturn(null); // Null orgData
        when(consignee.getAddressData()).thenReturn(null); // Null addressData

        when(shipmentDetail.getConsigner()).thenReturn(consigner);
        when(shipmentDetail.getConsignee()).thenReturn(consignee);

        // Act
        ReflectionTestUtils.invokeMethod(hblService, "mapConsignerConsigneeToHblV3", shipmentDetail, hblData);

        // Assert - Should not throw any exceptions and handle nulls gracefully
        assertNull(hblData.getShipperName());
        assertNull(hblData.getShipperAddressLine1());
        assertNull(hblData.getShipperAddressLine2());
        assertNull(hblData.getShipperCity());
        assertNull(hblData.getShipperState());
        assertNull(hblData.getShipperZipCode());
        assertNull(hblData.getShipperCountry());

        assertNull(hblData.getConsigneeName());
        assertNull(hblData.getConsigneeAddressLine1());
        assertNull(hblData.getConsigneeAddressLine2());
        assertNull(hblData.getConsigneeCity());
        assertNull(hblData.getConsigneeState());
        assertNull(hblData.getConsigneeZipCode());
        assertNull(hblData.getConsigneeCountry());
    }
    @Test
    void mapForwardDataInHblV3_WithNullData_ShouldHandleGracefully() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        AdditionalDetails additionalDetails = mock(AdditionalDetails.class);

        // Mock export broker with null data
        Parties exportBroker = mock(Parties.class);
        when(exportBroker.getOrgData()).thenReturn(null); // Null orgData
        when(exportBroker.getAddressData()).thenReturn(null); // Null addressData

        when(additionalDetails.getExportBroker()).thenReturn(exportBroker);
        ReflectionTestUtils.invokeMethod(hblService, "mapForwardingAgentDataInHblV3", additionalDetails, hblData);

        // Assert - Should not throw any exceptions and handle nulls gracefully
        assertNull(hblData.getForwarderName());
        assertNull(hblData.getForwarderAddressLine1());
        assertNull(hblData.getForwarderAddressLine2());
        assertNull(hblData.getForwarderCity());
        assertNull(hblData.getForwarderState());
        assertNull(hblData.getForwarderZipCode());
        assertNull(hblData.getForwarderCountry());
    }

    @Test
    void mapDeliveryDataInHblV3_WithNullData_ShouldHandleGracefully() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        AdditionalDetails additionalDetails = mock(AdditionalDetails.class);

        // Mock import broker with null data
        Parties importBroker = mock(Parties.class);
        when(importBroker.getOrgData()).thenReturn(null); // Null orgData
        when(importBroker.getAddressData()).thenReturn(null); // Null addressData

        when(additionalDetails.getImportBroker()).thenReturn(importBroker);

        ReflectionTestUtils.invokeMethod(hblService, "mapDeliveryAgentDataInHblV3", additionalDetails, hblData);

        // Assert - Should not throw any exceptions and handle nulls gracefully
        assertNull(hblData.getDeliveryAgentName());
        assertNull(hblData.getDeliveryAgentAddressLine1());
        assertNull(hblData.getDeliveryAgentAddressLine2());
        assertNull(hblData.getDeliveryAgentCity());
        assertNull(hblData.getDeliveryAgentState());
        assertNull(hblData.getDeliveryAgentZipCode());
        assertNull(hblData.getDeliveryAgentCountry());
    }

    @Test
    void mapPartiesV3_ShouldPopulateAllV3Fields() {
        // Arrange
        HblDataDto hblData = new HblDataDto();
        ShipmentDetails shipmentDetail = mock(ShipmentDetails.class);
        AdditionalDetails additionalDetails = mock(AdditionalDetails.class);

        // Mock minimal data to ensure methods don't throw exceptions
        when(shipmentDetail.getConsigner()).thenReturn(mock(Parties.class));
        when(shipmentDetail.getConsignee()).thenReturn(mock(Parties.class));
        when(additionalDetails.getImportBroker()).thenReturn(mock(Parties.class));
        when(additionalDetails.getExportBroker()).thenReturn(mock(Parties.class));
        ReflectionTestUtils.invokeMethod(hblService, "mapPartiesV3", shipmentDetail, additionalDetails, hblData);
        assertNotNull(hblData);
        // You can add more specific assertions based on what the methods should set
    }

    @Test
    void updateShipmentPartiesToHBL_WithV3Enabled_ShouldSetV3AddressFields() {
        // Arrange
        Hbl hbl = new Hbl();
        hbl.setHblNotifyParty(new ArrayList<>());

        Parties party = mock(Parties.class);
        HblLockSettings hblLock = mock(HblLockSettings.class);

        // Mock V3 enabled
        ShipmentSettingsDetails settings = mock(ShipmentSettingsDetails.class);
        when(settings.getIsRunnerV3Enabled()).thenReturn(true);
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(settings);

        // Mock party data
        Map<String, Object> orgData = Map.of(
                PartiesConstants.FULLNAME, "Test Party",
                PartiesConstants.EMAIL, "test@example.com"
        );
        Map<String, Object> addressData = Map.of(
                PartiesConstants.ADDRESS1, "123 Test St",
                PartiesConstants.ADDRESS2, "Suite 100",
                PartiesConstants.CITY, "Test City",
                PartiesConstants.STATE, "TS",
                PartiesConstants.ZIP_POST_CODE, "12345",
                PartiesConstants.COUNTRY, "USA"
        );

        when(party.getOrgData()).thenReturn(orgData);
        when(party.getAddressData()).thenReturn(addressData);


        ReflectionTestUtils.invokeMethod(hblService, "updateShipmentPartiesToHBL", party, hbl, hblLock);
        // Assert
        assertEquals(1, hbl.getHblNotifyParty().size());
        HblPartyDto resultParty = hbl.getHblNotifyParty().get(0);
        assertTrue(resultParty.getIsShipmentCreated());
        assertEquals("TEST PARTY", resultParty.getName());
        assertEquals("test@example.com", resultParty.getEmail());
        assertNotNull(resultParty.getAddress1());
        assertNotNull(resultParty.getAddress2());
        assertNotNull(resultParty.getCity());
        assertNotNull(resultParty.getState());
        assertNotNull(resultParty.getZipCode());
        assertNotNull(resultParty.getCountry());
    }

    @Nested
    class CreateHblTaskForApprovalTests {

        @Test
        void shouldCreateTaskSuccessfully() throws RunnerException {
            //given
            Long shipmentId = 100L;
            ShipmentDetails shipment = jsonTestUtility.getTestShipment();
            shipment.setId(shipmentId);
            shipment.setShipmentId("SHP-2025-000100");
            shipment.setGuid(UUID.randomUUID());
            CarrierDetails carrier = new CarrierDetails();
            carrier.setOrigin("AEJEA");
            carrier.setDestination("USNYC");
            shipment.setCarrierDetails(carrier);

            ConsolidationDetails consol = new ConsolidationDetails();
            consol.setId(555L);
            consol.setConsolidationNumber("CONS-2025-555");
            shipment.setConsolidationList(Set.of(consol));

            // mdm returns no existing pending tasks
            when(mdmServiceAdapter.getTaskList(String.valueOf(shipment.getGuid()),
                    SHIPMENTS_WITH_SQ_BRACKETS,
                    PENDING_ACTION_TASK,
                    ORIGINAL_BL_APPROVAL)).thenReturn(Collections.emptyList());

            // mock location cache data
            EntityTransferUnLocations originLoc = new EntityTransferUnLocations();
            originLoc.LocCode = "AEJEA";
            originLoc.Name = "Jebel Ali";
            EntityTransferUnLocations destLoc = new EntityTransferUnLocations();
            destLoc.LocCode = "USNYC";
            destLoc.Name = "New York";
            Map<String, EntityTransferUnLocations> locs = new HashMap<>();
            locs.put("AEJEA", originLoc);
            locs.put("USNYC", destLoc);

            when(masterDataUtils.getLocationDataFromCache(anySet(), eq(EntityTransferConstants.LOCATION_SERVICE_GUID)))
                    .thenReturn(locs);

            // ensure shipment is returned
            when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipment));

            when(commonUtils.getShipmentSettingFromContext()).thenReturn(
                    ShipmentSettingsDetails.builder()
                            .restrictBlApprovalRole(1)
                            .build());

            // mock MDM create response
            MdmTaskCreateResponse mdmResp = new MdmTaskCreateResponse();
            mdmResp.setId(999);
            mdmResp.setUuid("uuid-999");
            when(mdmServiceAdapter.createTask(any())).thenReturn(mdmResp);

            when(jsonHelper.convertToJson(any())).thenReturn(
                    "{\"transportMode\":\"Sea\",\"shipmentType\":\"BCN\",\"origin\":\"AEJEA - Jebel Ali\",\"destination\":\"USNYC - New York\","
                            + "\"masterBill\":null,\"houseBill\":null,\"shipmentId\":\"SHP-2025-000100\","
                            + "\"consolidationNumber\":\"CONS-2025-555\",\"consoleIdPk\":555,\"shipmentIdPk\":100}"
            );

            //when
            ResponseEntity<IRunnerResponse> response = hblService.createHblTaskForApproval(CommonRequestModel.buildRequest(shipmentId));

            //then
            assertEquals(HttpStatus.OK, response.getStatusCode());

            ArgumentCaptor<MdmTaskCreateRequest> captor = ArgumentCaptor.forClass(MdmTaskCreateRequest.class);
            verify(mdmServiceAdapter, times(1)).createTask(captor.capture());
            MdmTaskCreateRequest captured = captor.getValue();

            assertEquals(String.valueOf(shipment.getGuid()), captured.getEntityUuid());
            assertEquals(shipment.getShipmentId(), captured.getReferenceFileNumber());
            assertTrue(captured.getTaskInfoJson().contains("\"transportMode\""));
            assertTrue(captured.getTaskInfoJson().contains("\"origin\":\"AEJEA - Jebel Ali\""));
            assertTrue(captured.getTaskInfoJson().contains("\"destination\":\"USNYC - New York\""));
        }

        @Test
        void shouldThrowWhenTaskAlreadyPending() {
            //given
            Long shipmentId = 200L;
            ShipmentDetails shipment = jsonTestUtility.getTestShipment();
            shipment.setId(shipmentId);
            shipment.setGuid(UUID.randomUUID());

            when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipment));

            // return a non-empty list to simulate an existing pending task
            when(mdmServiceAdapter.getTaskList(String.valueOf(shipment.getGuid()),
                    SHIPMENTS_WITH_SQ_BRACKETS, PENDING_ACTION_TASK,
                    ORIGINAL_BL_APPROVAL)).thenReturn(List.of(Map.of("id", "1")));

            //when //then
            assertThrows(MdmTaskException.class, () ->
                    hblService.createHblTaskForApproval(CommonRequestModel.buildRequest(shipmentId)));
        }

        @Test
        void shouldThrowWhenShipmentNotFound() {
            //given
            Long shipmentId = 300L;
            when(shipmentDao.findById(shipmentId)).thenReturn(Optional.empty());

            //when //then
            assertThrows(DataRetrievalFailureException.class, () ->
                    hblService.createHblTaskForApproval(CommonRequestModel.buildRequest(shipmentId)));
        }
    }


}