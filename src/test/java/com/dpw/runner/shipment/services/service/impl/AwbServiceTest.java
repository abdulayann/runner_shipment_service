package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AirMessagingLogsConstants;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.request.reportService.CompanyDto;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.syncing.interfaces.IAwbSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.logging.log4j.util.Strings;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.stream.Stream;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AwbServiceTest extends CommonMocks {

    @Mock
    private IConsolidationService consolidationService;
    @Mock
    private PackingService packingService;
    @Mock
    private IAwbDao awbDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IMawbHawbLinkDao mawbHawbLinkDao;
    @Mock
    private IAwbSync awbSync;
    @Mock
    private IShipmentSync shipmentSync;
    @Mock
    private IShipmentService shipmentService;
    @Mock
    private IV1Service v1Service;
    @Mock
    private IAuditLogService auditLogService;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IAirMessagingLogsService airMessagingLogsService;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;
    @Mock
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private BridgeServiceAdapter bridgeServiceAdapter;
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @InjectMocks
    private AwbService awbService;

    private static JsonTestUtility jsonTestUtility;
    private static ShipmentDetails testShipment;
    private static ConsolidationDetails testConsol;
    private static ObjectMapper objectMapper;
    private static Awb testHawb;
    private static Awb testDmawb;
    private static Awb testMawb;
    private static List<EntityTransferOrganizations> mockEntityTransferOrganizationList;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setCompanyId(1);
        mockUser.setTenantDisplayName("test");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).legalEntityCode("test").build());


        testShipment = jsonTestUtility.getTestShipment();
        testConsol = jsonTestUtility.getJson("MAWB_CONSOLIDATION", ConsolidationDetails.class);

        testHawb = jsonTestUtility.getTestHawb();
        testDmawb = jsonTestUtility.getTestDmawb();
        testMawb = jsonTestUtility.getTestMawb();
        awbService.executorService = Executors.newFixedThreadPool(2);
    }

    @AfterEach
    void tearDown() {
        awbService.executorService.shutdown();
    }

    @Test
    void createAwb_failure_empty_request() {
        // retrieve request works with id
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        //Make service call
        Exception exception = assertThrows(ValidationException.class, () -> awbService.createAwb(commonRequestModel));
        assertEquals("Request can't be empty for creating AWB", exception.getMessage());
    }

    @Test
    void createAwb_throws_exception_for_empty_shipmentId() {
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);
        Exception exception = assertThrows(ValidationException.class, () -> awbService.createAwb(commonRequestModel));
        assertEquals("Shipment Id can't be null or empty in Create AWB Request", exception.getMessage());
    }


    @Test
    void createAwb_fails_on_missing_shipment_validation() {
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ShipmentId(1L).AwbType("DMAWB").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);
        var mockShipment = testShipment;
        mockShipment.setConsigner(null);
        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(mockShipment));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.createAwb(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals("Consigner details are required in shipment to generate the document.", runnerResponse.getError().getMessage());
    }

    @Test
    void createAwb_success() throws RunnerException {
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ShipmentId(1L).AwbType("DMAWB").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);

        testShipment.setHouseBill("custom-house-bill");
        testShipment.setSecurityStatus(Constants.SHR);
        addShipmentDataForAwbGeneration(testShipment);

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
//        Mockito.when(shipmentService.generateCustomHouseBL(any())).thenReturn("test_hbl_123");
//        Mockito.when(shipmentDao.save(any(), anyBoolean())).thenReturn(testShipment);

//        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
            ));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);

        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
//        when(jsonHelper.convertValue(any(), eq(EntityTransferMasterLists.class))).thenReturn(null);

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(
                objectMapper.convertValue(testDmawb, AwbResponse.class)
        );

        OrgAddressResponse mockOrgAddressResponse = new OrgAddressResponse();
        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.createAwb(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void createAwbSuccessWithUpdatedNatureAndQuantityOfGoods() throws RunnerException {
        AwbPackingInfo testAwbPackingInfo = new AwbPackingInfo();
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ShipmentId(1L).AwbType("DMAWB").build();
        List<AwbPackingInfo> awbPackingInfoList = new ArrayList<>();
        testAwbPackingInfo.setVolume(new BigDecimal("1230.450"));
        awbPackingInfoList.add(testAwbPackingInfo);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);

        testShipment.setHouseBill("custom-house-bill");
        testShipment.setSecurityStatus(Constants.SHR);
        testShipment.setGoodsDescription("ShipmentDescription");
        Packing packing = new Packing();
        packing.setPacks("1");
        packing.setVolume(new BigDecimal("1230.450"));
        List<Packing> packingList = new ArrayList<>();
        packingList.add(packing);
        testShipment.setPackingList(packingList);
        testShipment.setVolume(new BigDecimal("1230.450"));
        addShipmentDataForAwbGeneration(testShipment);
        testDmawb.setAwbPackingInfo(awbPackingInfoList);

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(awbDao.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);

        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class)))
                .thenAnswer(invocation -> {
                    Object arg = invocation.getArgument(0);
                    return objectMapper.convertValue(arg, AwbResponse.class);
                });

        OrgAddressResponse mockOrgAddressResponse = new OrgAddressResponse();
        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> response = awbService.createAwb(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(response.getBody(), RunnerResponse.class);
        assertEquals("SHIPMENTDESCRIPTION\r\nVOL 1230.450 M3", objectMapper.convertValue(runnerResponse.getData(), AwbResponse.class).getAwbCargoInfo().getNtrQtyGoods());
    }

    @Test
    void createAwb_throwsRaKcException() throws RunnerException {
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ShipmentId(1L).AwbType("DMAWB").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);

        testShipment.setHouseBill("custom-house-bill");
        addShipmentDataForAwbGeneration(testShipment);

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));

        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;

        mockTenantSettings();
        doThrow(new RunnerException("Error while validating RaKC details")).when(shipmentService).validateRaKcDetails(any());
        var response = awbService.createAwb(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void createAwbSuccessGeneratesRoutingInfoFromCarrierDetails() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(false);
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ShipmentId(1L).AwbType("DMAWB").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);

        testShipment.setHouseBill("custom-house-bill");
        testShipment.setRoutingsList(null);
        addShipmentDataForAwbGeneration(testShipment);

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
//        Mockito.when(shipmentService.generateCustomHouseBL(any())).thenReturn("test_hbl_123");
//        Mockito.when(shipmentDao.save(any(), anyBoolean())).thenReturn(testShipment);

//        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));

        // TenantModel Response mocking
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity("").build());
        when(jsonHelper.convertValue(eq(""), eq(TenantModel.class))).thenReturn(new TenantModel());
        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(jsonHelper.convertValueToList(any(),eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
//        when(jsonHelper.convertValue(any(), eq(EntityTransferMasterLists.class))).thenReturn(null);

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(
                objectMapper.convertValue(testDmawb, AwbResponse.class)
        );

        OrgAddressResponse mockOrgAddressResponse = new OrgAddressResponse();
        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.createAwb(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }


    @Test
    void updateAwbEmptyRequestIdFails() throws RunnerException {
        AwbRequest request = new AwbRequest();
        request.setAwbNumber("updatedAWBNumber");
        request.setAwbShipmentInfo(testHawb.getAwbShipmentInfo());
        request.setShcIdList(List.of("shcId1", "shcId2"));

        Awb mockAwb = testHawb;
        mockAwb.getAwbShipmentInfo().setAwbNumber("updatedAWBNumber");

        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(jsonHelper.convertValue(any(), eq(Awb.class))).thenReturn(mockAwb);

        // Test
        ResponseEntity<IRunnerResponse> responseEntity = awbService.updateAwb(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void updateAwbEmptyFailsWhenAwbNotPresent() throws RunnerException {
        AwbRequest request = new AwbRequest(); // Provide necessary data for request
        request.setAwbNumber("updatedAWBNumber");
        request.setId(1);
        Awb mockAwb = testHawb;
        mockAwb.getAwbShipmentInfo().setAwbNumber("updatedAWBNumber");
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(jsonHelper.convertValue(any(), eq(Awb.class))).thenReturn(mockAwb);
        when(awbDao.findById(1L)).thenReturn(Optional.empty());

        // Test
        ResponseEntity<IRunnerResponse> responseEntity = awbService.updateAwb(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateAwb_shipment() throws RunnerException {
        AwbRequest request = new AwbRequest(); // Provide necessary data for request
        request.setAwbNumber("updatedAWBNumber");
        request.setId(1);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Awb mockAwb = testHawb;
        mockAwb.getAwbShipmentInfo().setAwbNumber("updatedAWBNumber");
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        // Mocking
        when(awbDao.findById(1L)).thenReturn(Optional.of(mockAwb));
        when(jsonHelper.convertValue(any(), eq(Awb.class))).thenReturn(mockAwb);
        when(awbDao.save(any(Awb.class))).thenReturn(mockAwb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        // Test
        ResponseEntity<IRunnerResponse> responseEntity = awbService.updateAwb(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), responseEntity);
    }

    @Test
    void updateAwb_shipment_SetSciT1() throws RunnerException {
        AwbRequest request = new AwbRequest(); // Provide necessary data for request
        request.setAwbNumber("updatedAWBNumber");
        request.setId(1);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Awb mockAwb = testHawb;
        Awb mockMawb = testMawb;
        mockMawb.setId(2L);
        mockAwb.getAwbShipmentInfo().setAwbNumber("updatedAWBNumber");
        mockAwb.getAwbCargoInfo().setSci("T1");
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        // Mocking
        when(awbDao.findById(1L)).thenReturn(Optional.of(mockAwb));
        when(jsonHelper.convertValue(any(), eq(Awb.class))).thenReturn(mockAwb);
        when(awbDao.save(any(Awb.class))).thenReturn(mockAwb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        // Test
        ResponseEntity<IRunnerResponse> responseEntity = awbService.updateAwb(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), responseEntity);
    }

    @Test
    void updateAwb_shipment_RemoveSciT1() throws RunnerException {
        AwbRequest request = new AwbRequest(); // Provide necessary data for request
        request.setAwbNumber("updatedAWBNumber");
        request.setId(1);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Awb mockAwb = testHawb;
        Awb oldEntity = jsonTestUtility.getTestHawb();
        oldEntity.getAwbCargoInfo().setSci("T1");
        Awb mockMawb = testMawb;
        mockMawb.setId(2L);
        mockMawb.getAwbCargoInfo().setSci("T1");
        mockAwb.getAwbShipmentInfo().setAwbNumber("updatedAWBNumber");
        mockAwb.getAwbCargoInfo().setSci(null);
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        // Mocking
        when(awbDao.findById(1L)).thenReturn(Optional.of(oldEntity));
        when(jsonHelper.convertValue(any(), eq(Awb.class))).thenReturn(mockAwb);
        when(awbDao.save(any(Awb.class))).thenReturn(mockAwb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        // Test
        ResponseEntity<IRunnerResponse> responseEntity = awbService.updateAwb(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), responseEntity);
    }

    @Test
    void updateAwb_shipment_RountingException() throws RunnerException {
        AwbRequest request = new AwbRequest(); // Provide necessary data for request
        request.setAwbNumber("updatedAWBNumber");
        request.setId(1);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Awb mockAwb = testHawb;
        mockAwb.getAwbShipmentInfo().setAwbNumber("updatedAWBNumber");
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        mockAwb.getAwbRoutingInfo().get(0).setLeg(1L);
        mockAwb.getAwbRoutingInfo().get(1).setLeg(1L);
        // Mocking
        when(awbDao.findById(1L)).thenReturn(Optional.of(mockAwb));
        when(jsonHelper.convertValue(any(), eq(Awb.class))).thenReturn(mockAwb);
        // Test
        ResponseEntity<IRunnerResponse> responseEntity = awbService.updateAwb(commonRequestModel);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void updateAwb_consolidation() throws RunnerException {
        AwbRequest request = new AwbRequest(); // Provide necessary data for request
        request.setAwbNumber("updatedAWBNumber");
        request.setId(1);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Awb mockAwb = testMawb;
        mockAwb.getAwbShipmentInfo().setAwbNumber("updatedAWBNumber");
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);


        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();
//        when(awbDao.(id)).thenReturn(List.of(testMawb));
        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().interBranchConsole(true).build()));
        // Mocking
        when(awbDao.findById(1L)).thenReturn(Optional.of(mockAwb));
        when(jsonHelper.convertValue(any(), eq(Awb.class))).thenReturn(mockAwb);
        when(awbDao.save(any(Awb.class))).thenReturn(mockAwb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        // Test
        ResponseEntity<IRunnerResponse> responseEntity = awbService.updateAwb(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), responseEntity);
    }

    @Test
    void updateAwb_consolidation2() throws RunnerException {
        AwbRequest request = new AwbRequest(); // Provide necessary data for request
        request.setAwbNumber("updatedAWBNumber");
        request.setId(1);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        Awb mockAwb = testMawb;
        mockAwb.getAwbShipmentInfo().setAwbNumber("updatedAWBNumber");
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);


        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();
        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(ConsolidationDetails.builder().intraBranch(false).build()));
        // Mocking
        when(awbDao.findById(1L)).thenReturn(Optional.of(mockAwb));
        when(jsonHelper.convertValue(any(), eq(Awb.class))).thenReturn(mockAwb);
        when(awbDao.save(any(Awb.class))).thenReturn(mockAwb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        // Test
        ResponseEntity<IRunnerResponse> responseEntity = awbService.updateAwb(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), responseEntity);
    }

    @Test
    void listShipmentAwb() {
        // adding special handiling codes in awb
        List<AwbSpecialHandlingCodesMappingInfo> sph = new ArrayList<>();
        sph.add(AwbSpecialHandlingCodesMappingInfo.builder().shcId("testShcId").build());
        testHawb.setAwbSpecialHandlingCodesMappings(sph);

        AwbResponse mockAwbResponse = objectMapper.convertValue(testHawb, AwbResponse.class);

        FetchAwbListRequest listCommonRequest = contructFetchAwbListRequest("id" , 1L, "=");
        Page<Awb> resultPage = new PageImpl<Awb>(List.of(testHawb));
        Mockito.when(awbDao.findAll(any(), any())).thenReturn(resultPage);
        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));

        // convertEntityListToDtoList
        List<MasterData> chargeMasterData = List.of(new MasterData());
        V1DataResponse mockChargeCodeMasterData = V1DataResponse.builder().entities(chargeMasterData).build();
        when(jsonHelper.convertValueToList(any(), eq(MasterData.class))).thenReturn(chargeMasterData);
        when(v1Service.fetchMasterData(any())).thenReturn(mockChargeCodeMasterData);
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        when(masterDataUtils.getCountriesMasterListData(any())).thenReturn(Map.of("IN", "INDIA"));
        V1DataResponse v1Response = V1DataResponse.builder().entities("").build();
        when(v1Service.getCompaniesDetails(any())).thenReturn(v1Response);
        CompanyDto companyDto = CompanyDto.builder().country("IND").city("test").zipPostCode("test").address1("test").address2("test").state("test").build();
        List<CompanyDto> companyDtos = new ArrayList<>(List.of(companyDto));
        when(jsonHelper.convertValueToList(any(), eq(CompanyDto.class))).thenReturn(companyDtos);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> listResponse = awbService.list(CommonRequestModel.buildRequest(listCommonRequest));
        assertEquals(HttpStatus.OK, listResponse.getStatusCode());
        assertNotNull(listResponse.getBody());
    }

    @Test
    void listConsolidationAwb() {
        Long consolidationId = 1L;
        Awb mockAwb = testMawb;
        mockAwb.setConsolidationId(consolidationId);
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);

        FetchAwbListRequest listCommonRequest = contructFetchAwbListRequest("id" , 1L, "=");
        Page<Awb> resultPage = new PageImpl<Awb>(List.of(mockAwb));
        Mockito.when(awbDao.findAll(any(), any())).thenReturn(resultPage);
        Mockito.when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(new TenantModel());
        when(masterDataUtils.consolidationAddressCountryMasterData(any())).thenReturn(Map.of("IN", "India", "PE", "Peru", "CA", "Canada"));
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(Map.of("Air Transat", EntityTransferCarrier.builder().HeadQuartersDetails("test").build()));
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> listResponse = awbService.list(CommonRequestModel.buildRequest(listCommonRequest));
        assertEquals(HttpStatus.OK, listResponse.getStatusCode());
        assertNotNull(listResponse.getBody());
    }


    @ParameterizedTest
    @CsvSource({
            "null, SECURITY_STATUS",       // Test with null ScreeningStatus list
            "'', SECURITY_STATUS",         // Test with empty ScreeningStatus list
            "VALID_STATUS, null",          // Test with valid ScreeningStatus and null SecurityStatus
            "VALID_STATUS, ''",            // Test with valid ScreeningStatus and empty SecurityStatus
            "VCK, SECURITY_STATUS"         // Test with ScreeningStatus containing only "VCK"
    })
    void listConsolidationAwb_withValidationFailureResponse(String screeningStatus, String securityStatus) {
        Long consolidationId = 1L;
        Awb mockAwb = testMawb;
        mockAwb.setConsolidationId(consolidationId);

        FetchAwbListRequest listCommonRequest = contructFetchAwbListRequest("id", 1L, "=");
        listCommonRequest.setFromGenerateAwbButton(true);

        Page<Awb> resultPage = new PageImpl<>(List.of(mockAwb));
        Mockito.when(awbDao.findAll(any(), any())).thenReturn(resultPage);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();

        if ("null".equals(screeningStatus)) {
            consolidationDetails.setScreeningStatus(null);
        } else if ("".equals(screeningStatus)) {
            consolidationDetails.setScreeningStatus(new ArrayList<>());
        } else {
            consolidationDetails.setScreeningStatus(List.of(screeningStatus));
        }

        consolidationDetails.setSecurityStatus("null".equals(securityStatus) ? null : securityStatus);

        Mockito.when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(consolidationDetails));

        ResponseEntity<IRunnerResponse> response = awbService.list(CommonRequestModel.buildRequest(listCommonRequest));

        assertNotNull(response);
        assertFalse(response.getStatusCode().is2xxSuccessful());
    }

    @ParameterizedTest
    @CsvSource({
            "null, SECURITY_STATUS",       // Test with null ScreeningStatus list
            "'', SECURITY_STATUS",         // Test with empty ScreeningStatus list
            "VALID_STATUS, null",          // Test with valid ScreeningStatus and null SecurityStatus
            "VALID_STATUS, ''",            // Test with valid ScreeningStatus and empty SecurityStatus
            "VCK, SECURITY_STATUS"         // Test with ScreeningStatus containing only "VCK"
    })
    void listShipmentAwb_withValidationFailureResponse(String screeningStatus, String securityStatus) {
        Long shipmentId = 1L;
        Awb mockAwb = testHawb;
        mockAwb.setShipmentId(shipmentId);

        FetchAwbListRequest listCommonRequest = contructFetchAwbListRequest("id", 1L, "=");
        listCommonRequest.setFromGenerateAwbButton(true);

        Page<Awb> resultPage = new PageImpl<>(List.of(mockAwb));
        Mockito.when(awbDao.findAll(any(), any())).thenReturn(resultPage);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        shipmentDetails.setAdditionalDetails(additionalDetails);

        if ("null".equals(screeningStatus)) {
            shipmentDetails.getAdditionalDetails().setScreeningStatus(null);
        } else if ("".equals(screeningStatus)) {
            shipmentDetails.getAdditionalDetails().setScreeningStatus(new ArrayList<>());
        } else {
            shipmentDetails.getAdditionalDetails().setScreeningStatus(List.of(screeningStatus));
        }

        shipmentDetails.setSecurityStatus("null".equals(securityStatus) ? null : securityStatus);

        Mockito.when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(shipmentDetails));

        ResponseEntity<IRunnerResponse> response = awbService.list(CommonRequestModel.buildRequest(listCommonRequest));

        assertNotNull(response);
        assertFalse(response.getStatusCode().is2xxSuccessful());
    }

    @Test
    void testGetMawnLinkPacks() {
        Long id = 1L;

        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();

        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));
        when(awbDao.findByIds(anyList())).thenReturn(List.of(Awb.builder().awbPackingInfo(Arrays.asList()).build()));
        var awbResponse = awbService.getMawnLinkPacks(testMawb);

        assertEquals(testMawb, awbResponse);

    }

    @Test
    void retrieveById_success_shipment_awb() {
        // retrieve request works with id
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        // Mock
        Mockito.when(awbDao.findById(1L)).thenReturn(Optional.of(testHawb));
        AwbResponse awbResponse = objectMapper.convertValue(testHawb, AwbResponse.class);
        Mockito.when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(awbResponse);
        //Make service call
        ResponseEntity<IRunnerResponse> response = awbService.retrieveById(commonRequestModel);
        // Assert
        assertNotNull(response.getBody());
        assertEquals(response.getStatusCode(), HttpStatus.OK);
        RunnerResponse runnerResponse = objectMapper.convertValue(response.getBody(), RunnerResponse.class);
        assertEquals(1, objectMapper.convertValue(runnerResponse.getData(), AwbResponse.class).getId());
    }

    @Test
    void retrieveById_success_consolidation_awb() {
        Long id = 3L;
        // retrieve request works with id
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);

        List<ShipmentSettingsDetails> mockTenantSettingsList = List.of(new ShipmentSettingsDetails());
        AwbResponse awbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);

        // get Linked Hawb
        List<MawbHawbLink> mockMawbHawbLink = List.of(MawbHawbLink.builder().hawbId(1L).mawbId(3L).build());

        // Mock
        Mockito.when(awbDao.findById(id)).thenReturn(Optional.of(testMawb));
        Mockito.when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(awbResponse);
        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(mockTenantSettingsList);

        when(mawbHawbLinkDao.findByMawbId(id)).thenReturn(mockMawbHawbLink);


        //Make service call
        ResponseEntity<IRunnerResponse> response = awbService.retrieveById(commonRequestModel);
        // Assert
        assertNotNull(response.getBody());
        assertEquals(response.getStatusCode(), HttpStatus.OK);
        RunnerResponse runnerResponse = objectMapper.convertValue(response.getBody(), RunnerResponse.class);
        assertEquals(3, objectMapper.convertValue(runnerResponse.getData(), AwbResponse.class).getId());
    }

    @Test
    void retrieveById_failure_empty_request() {
        // retrieve request works with id
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        //Make service call
        ResponseEntity<IRunnerResponse> response = awbService.retrieveById(commonRequestModel);
        // Assert
        assertNotNull(response.getBody());
        assertEquals(response.getStatusCode(), HttpStatus.BAD_REQUEST);
    }

    @Test
    void retrieveById_empty_id() {
        // retrieve request works with id
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        //Make service call
        ResponseEntity<IRunnerResponse> response = awbService.retrieveById(commonRequestModel);
        // Assert
        assertNotNull(response.getBody());
        assertEquals(response.getStatusCode(), HttpStatus.BAD_REQUEST);

    }

    @Test
    void retrieveById_failure_no_record() {
        // retrieve request works with id
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(0L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        // Mock
        Mockito.when(awbDao.findById(0L)).thenReturn(Optional.empty());
        AwbResponse awbResponse = objectMapper.convertValue(testDmawb, AwbResponse.class);
        //Make service call
        ResponseEntity<IRunnerResponse> response = awbService.retrieveById(commonRequestModel);
        // Assert
        assertNotNull(response.getBody());
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(response.getBody(), RunnerResponse.class);
        assertEquals("Failed to fetch data for given constraint.", runnerResponse.getError().getMessage());
    }

    @Test
    void createMawbThrowsExceptionWhenEmptyConsolidationId() {
        CreateAwbRequest request = new CreateAwbRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        assertThrows(ValidationException.class, () -> awbService.createMawb(commonRequestModel));
    }

    @Test
    void createMawbThrowsFailsWhenInternalFailureOccurs() {
        CreateAwbRequest request = new CreateAwbRequest();
        request.setConsolidationId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consolidationDetailsDao.findById(any())).thenThrow(new RuntimeException());

        var res = awbService.createMawb(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, res.getStatusCode());
    }

    @Test
    void createMawb_success() throws RunnerException {
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ConsolidationId(1L).AwbType(Constants.MAWB).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);
        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));
        testConsol.setSecurityStatus(Constants.SCO);
        testConsol.setInterBranchConsole(true);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);

        AwbResponse mockMawbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);

        Mockito.when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.findByShipmentIdList(Arrays.asList(shipmentId))).thenReturn(List.of(testHawb));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);

        when(awbDao.save(any())).thenReturn(testMawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));


        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockMawbResponse);

        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.createMawb(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void createMawbSuccessWithUpdatedNatureAndQuantityOfGoods() throws RunnerException {
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ConsolidationId(1L).AwbType(Constants.MAWB).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);
        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));
        testConsol.setSecurityStatus(Constants.SCO);
        testConsol.setInterBranchConsole(true);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setPacksVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);

        AwbResponse mockMawbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);

        Mockito.when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.findByShipmentIdList(Arrays.asList(shipmentId))).thenReturn(List.of(testHawb));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);


        when(awbDao.save(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));


        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class)))
                .thenAnswer(invocation -> {
                    Object arg = invocation.getArgument(0);
                    return objectMapper.convertValue(arg, AwbResponse.class);
                });

        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> response = awbService.createMawb(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(response.getBody(), RunnerResponse.class);
        assertEquals("CONSOLIDATION AS PER ATTACHED LIST\r\nVOL 1000.567 M3", objectMapper.convertValue(runnerResponse.getData(), AwbResponse.class).getAwbCargoInfo().getNtrQtyGoods());
    }

    @Test
    void createMawbGeneratesRoutingInfoFromCarrierDetails() throws RunnerException {
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ConsolidationId(1L).AwbType(Constants.MAWB).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);
        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));
        testConsol.setRoutingsList(null);
        testHawb.getAwbCargoInfo().setSci(AwbConstants.T1);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);

        AwbResponse mockMawbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);

        Mockito.when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.findByShipmentIdList(anyList())).thenReturn(List.of(testHawb));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);


        when(awbDao.save(any())).thenReturn(testMawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));


        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockMawbResponse);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.createMawb(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testUpdateGoodsAndPacksForMawbUpdateGoodsDescInMawb() throws RunnerException {
        Long id = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(id);
        createAwbRequest.setConsolidationId(id);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        AwbResponse mockAwbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);
        ShipmentSettingsDetails mockShipmentSettingDetails = ShipmentSettingsDetails.builder().consolidationLite(false).build();

        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();
        when(awbDao.findByConsolidationId(id)).thenReturn(List.of(testMawb));
        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(mockShipmentSettingDetails);
        when(awbDao.save(testMawb)).thenReturn(testMawb);

        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        var httpResponse = awbService.updateGoodsAndPacksForMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

    @Test
    void testUpdateGoodsAndPacksForMawb() throws RunnerException {
        Long id = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(id);
        createAwbRequest.setConsolidationId(id);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        AwbResponse mockAwbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);
        ShipmentSettingsDetails mockShipmentSettingDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();
        Page<Awb> resultPage = new PageImpl<Awb>(List.of(testHawb));
        when(awbDao.findByConsolidationId(id)).thenReturn(List.of(testMawb));
        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));
//        when(awbDao.findAll(any(), any())).thenReturn(resultPage);

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(mockShipmentSettingDetails);
        when(awbDao.save(testMawb)).thenReturn(testMawb);

        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        var httpResponse = awbService.updateGoodsAndPacksForMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

    @Test
    void testUpdateGoodsAndPacksForMawb3() throws RunnerException {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CreateAwbRequest.builder().ShipmentId(id).ConsolidationId(id).build());

        AwbResponse mockAwbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);
        ShipmentSettingsDetails mockShipmentSettingDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        mockShipmentSettingDetails.setConsolidationLite(true);
        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();
        when(awbDao.findByConsolidationId(id)).thenReturn(List.of(testMawb));
        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(mockShipmentSettingDetails);

        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        var httpResponse = awbService.updateGoodsAndPacksForMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

    @Test
    void testUpdateGoodsAndPacksForMawb4() throws RunnerException {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CreateAwbRequest.builder().ShipmentId(id).ConsolidationId(id).build());
        AwbResponse mockAwbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);
        ShipmentSettingsDetails mockShipmentSettingDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        mockShipmentSettingDetails.setConsolidationLite(true);
        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();
        when(awbDao.findByConsolidationId(id)).thenReturn(List.of(testMawb));
        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));
        when(awbDao.findByIds(anyList())).thenReturn(
                Arrays.asList(
                        Awb.builder()
                                .awbPackingInfo(Arrays.asList(AwbPackingInfo.builder().build()))
                                .build()
                ));
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(mockShipmentSettingDetails);
        when(awbDao.findAwbByAwbNumbers(anyList())).thenReturn(Arrays.asList(Awb.builder().awbNumber("SHP0001").build()));
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        var httpResponse = awbService.updateGoodsAndPacksForMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

    @Test
    void testUpdateGoodsAndPacksForMawbForEmptyRequest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest();
        String errorMessage =  "Request is empty for Update Goods And Packs For Mawb";

        var httpResponse = awbService.updateGoodsAndPacksForMawb(commonRequestModel);

        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void testUpdateGoodsAndPacksForMawbForEmptyShipmentId() {
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        String errorMessage = "Shipment Id can't be null or empty in Update Goods And Packs For Mawb";

        var httpResponse = awbService.updateGoodsAndPacksForMawb(commonRequestModel);

        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void customAwbRetrieve_return_empty_when_params_are_null() {
        CustomAwbRetrieveRequest request = new CustomAwbRetrieveRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        List<Awb> mockAwb = new ArrayList<>();
        List<AwbResponse> mockAwbRes = jsonTestUtility.convertValueToList(mockAwb, AwbResponse.class);

        // Mock
        when(jsonHelper.convertValueToList(any(), eq(AwbResponse.class))).thenReturn(mockAwbRes);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = awbService.customAwbRetrieve(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbRes), httpResponse);
    }

    @Test
    void customAwbRetrieve_success() {
        CustomAwbRetrieveRequest request = new CustomAwbRetrieveRequest();
        request.setAwbNumber(List.of("235-88998641"));
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        List<AwbResponse> mockAwbRes = jsonTestUtility.convertValueToList(List.of(testHawb, testDmawb), AwbResponse.class);
        // Mock
        when(awbDao.findByAwbNumber(List.of("235-88998641"))).thenReturn(List.of(testHawb, testDmawb));
        when(jsonHelper.convertValueToList(any(), eq(AwbResponse.class))).thenReturn(mockAwbRes);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = awbService.customAwbRetrieve(commonRequestModel);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbRes), httpResponse);
    }



    @ParameterizedTest
    @ValueSource(strings = {
        Constants.PREPAID_DESC,
        Constants.COLLECT_DESC,
        Constants.COLLECT_PREPAID_DESC_CODE,
        Constants.PREPAID_COLLECT_DESC_CODE
    })
    void resetAllHawb(String chargeCodeArg) throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(2L).shipmentId(1L).awbType("DMAWB").resetType(AwbReset.ALL).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        testShipment.setHouseBill("custom-house-bill");
        Awb mockAwb = testDmawb;
        testShipment.setPaymentTerms(chargeCodeArg);

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(mockAwb));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(mockAwb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));

        // TenantModel Response mocking
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity("").build());
        when(jsonHelper.convertValue(eq(""), eq(TenantModel.class))).thenReturn(new TenantModel());
        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        EntityTransferOrganizations entityTransferOrganization1 = new EntityTransferOrganizations();
        entityTransferOrganization1.setOrganizationCode("RTY4552");
        entityTransferOrganization1.setTaxRegistrationNumber("TRP12343");
        EntityTransferOrganizations entityTransferOrganization2 = new EntityTransferOrganizations();
        entityTransferOrganization1.setOrganizationCode("RTY4558");
        entityTransferOrganization1.setTaxRegistrationNumber("TRP130839");
        when(jsonHelper.convertValueToList(any(),eq(EntityTransferOrganizations.class))).thenReturn(List.of(entityTransferOrganization1, entityTransferOrganization2));
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);

        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
//        when(jsonHelper.convertValue(any(), eq(EntityTransferMasterLists.class))).thenReturn(null);

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(
                objectMapper.convertValue(mockAwb, AwbResponse.class)
        );

        when(masterDataUtils.shipmentAddressCountryMasterData(any())).thenReturn(Map.of("PE", "Peru", "CA", "Canada"));

        OrgAddressResponse mockOrgAddressResponse = new OrgAddressResponse();
        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);
        when(masterDataUtils.getCountriesMasterListData(any())).thenReturn(Map.of("IN", "INDIA"));
        V1DataResponse v1Response = V1DataResponse.builder().entities("").build();
        when(v1Service.getCompaniesDetails(any())).thenReturn(v1Response);

        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetHawbRoutingInfo() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(2L).shipmentId(1L).awbType("DMAWB")
                .resetType(AwbReset.AWB_ROUTING).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        testShipment.setHouseBill("custom-house-bill");

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testDmawb));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testDmawb, AwbResponse.class));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetHawbNotifyPartyInfo() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(2L).shipmentId(1L).awbType("DMAWB")
                .resetType(AwbReset.AWB_NOTIFY_PARTY_INFO).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        testShipment.setHouseBill("custom-house-bill");

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testDmawb));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testDmawb, AwbResponse.class));
        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(jsonHelper.convertValueToList(any(),eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetHawbPacksAndGoods() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(2L).shipmentId(1L).awbType("DMAWB")
                .resetType(AwbReset.AWB_PACKS_AND_GOODS).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        testShipment.setHouseBill("custom-house-bill");
        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testDmawb));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testDmawb, AwbResponse.class));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetHawbOtherChargesInfo() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(2L).shipmentId(1L).awbType("DMAWB")
                .resetType(AwbReset.AWB_OTHER_CHARGES_INFO).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        testShipment.setHouseBill("custom-house-bill");

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testDmawb));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testDmawb, AwbResponse.class));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetHawbOciInfo() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(2L).shipmentId(1L).awbType("DMAWB")
                .resetType(AwbReset.AWB_OCI_INFO).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        testShipment.setHouseBill("custom-house-bill");

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testDmawb));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testDmawb, AwbResponse.class));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetAllMawb() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(3L).consolidationId(1L).awbType("MAWB").resetType(AwbReset.ALL).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);
        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);
        AwbResponse mockMawbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testMawb));
        Mockito.when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.findByShipmentIdList(anyList())).thenReturn(List.of(testHawb));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);


        when(awbDao.save(any())).thenReturn(testMawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));


        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockMawbResponse);
        when(masterDataUtils.consolidationAddressCountryMasterData(any())).thenReturn(Map.of("IN", "India", "EG", "Egypt"));
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(Map.of("Air Transat", EntityTransferCarrier.builder().HeadQuartersDetails("test").build()));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetAllMawbThrowsRaKcException() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(3L).consolidationId(1L).awbType("MAWB").resetType(AwbReset.ALL).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);
        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));

        AwbResponse mockMawbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testMawb));
        Mockito.when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.findByShipmentIdList(Arrays.asList(shipmentId))).thenReturn(List.of(testHawb));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(commonUtils.getCurrentTenantSettings()).thenReturn(new V1TenantSettingsResponse());

        doThrow(new RunnerException()).when(consolidationService).validateRaKcForConsol(any());
        assertThrows(RunnerException.class, () -> awbService.reset(commonRequestModel));
    }

    @Test
    void resetMawbRoutingInfo() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(3L).consolidationId(1L).awbType("MAWB")
                .resetType(AwbReset.AWB_ROUTING).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testMawb));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.save(any())).thenReturn(testMawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testMawb, AwbResponse.class));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetMawbNotifyPartyInfo() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(3L).consolidationId(1L).awbType("MAWB")
                .resetType(AwbReset.AWB_NOTIFY_PARTY_INFO).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testMawb));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.save(any())).thenReturn(testMawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testMawb, AwbResponse.class));
        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetMawbPacksAndGoods() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(3L).consolidationId(1L).awbType("MAWB")
                .resetType(AwbReset.AWB_PACKS_AND_GOODS).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));

        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();
        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));
        Page<Awb> resultPage = new PageImpl<Awb>(List.of(testHawb));
//        when(awbDao.findAll(any(), any())).thenReturn(resultPage);

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testMawb));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.save(any())).thenReturn(testMawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testMawb, AwbResponse.class));
        when(awbDao.findByShipmentId(anyLong())).thenReturn(List.of(testHawb));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetMawbOtherChargesInfo() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(3L).consolidationId(1L).awbType("MAWB")
                .resetType(AwbReset.AWB_OTHER_CHARGES_INFO).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testMawb));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.save(any())).thenReturn(testMawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testMawb, AwbResponse.class));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void resetMawbOciInfo() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(3L).consolidationId(1L).awbType("MAWB")
                .resetType(AwbReset.AWB_OCI_INFO).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testMawb));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.save(any())).thenReturn(testMawb);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(objectMapper.convertValue(testMawb, AwbResponse.class));

        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }



    @Test
    void partialAutoUpdateAwbThrowsExceptionEmptyRequest() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        var e = assertThrows(ValidationException.class, () ->
                awbService.partialAutoUpdateAwb(commonRequestModel));

        assertNotNull(e);
    }

    @Test
    void partialAutoUpdateAwbThrowsExceptionEmptyConsolId() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(null);
        createAwbRequest.setAwbType(Constants.HAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        var e = assertThrows(ValidationException.class, () ->
                awbService.partialAutoUpdateAwb(commonRequestModel));
        assertNotNull(e);

    }

    @Test
    void partialAutoUpdateAwbThrowsExceptionWhenAwbNotPresent() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(shipmentId);
        createAwbRequest.setAwbType(Constants.HAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        // Mocking
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(Collections.emptyList());

        var e = assertThrows(ValidationException.class, () ->
                awbService.partialAutoUpdateAwb(commonRequestModel));

        assertNotNull(e);
    }

    @Test
    void partialAutoUpdateAwbWithRestrictAwbEditFlagTrue() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(shipmentId);
        createAwbRequest.setAwbType(Constants.HAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        HawbLockSettings hawbLockSettings = jsonTestUtility.getJson("HAWB_LOCK_SETTINGS_ALL_TRUE", HawbLockSettings.class);

        Awb mockAwb = testHawb;
        mockAwb.getAwbPackingInfo().get(0).setGuid(testShipment.getPackingList().get(0).getGuid());
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addShipmentDataForAwbGeneration(testShipment);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setHawbLockSettings(hawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        tenantSettings.setRestrictAWBEdit(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(mockAwb));
//        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
//        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
//        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
//        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        // Reset Mocking

        testShipment.setHouseBill("custom-house-bill");

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(mockAwb));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(mockAwb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);

        // TenantModel Response mocking
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity("").build());
        when(jsonHelper.convertValue(eq(""), eq(TenantModel.class))).thenReturn(new TenantModel());

        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
//        when(jsonHelper.convertValue(any(), eq(EntityTransferMasterLists.class))).thenReturn(null);

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(
                mockAwbResponse
        );

        OrgAddressResponse mockOrgAddressResponse = new OrgAddressResponse();
        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(jsonHelper.convertValueToList(any(),eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        mockShipmentSettings();
        mockTenantSettings();

        var httpResponse = awbService.partialAutoUpdateAwb(commonRequestModel);


        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);

    }

    @Test
    void partialAutoUpdateAwbWithAllTrueHawbLockSettingsHawb() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(shipmentId);
        createAwbRequest.setAwbType(Constants.HAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        HawbLockSettings hawbLockSettings = jsonTestUtility.getJson("HAWB_LOCK_SETTINGS_ALL_TRUE", HawbLockSettings.class);

        Awb mockAwb = testHawb;
        mockAwb.getAwbPackingInfo().get(0).setGuid(testShipment.getPackingList().get(0).getGuid());
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addShipmentDataForAwbGeneration(testShipment);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setHawbLockSettings(hawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(mockAwb));
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
//        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        mockShipmentSettings();

        var httpResponse = awbService.partialAutoUpdateAwb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);

      }

    @Test
    void partialAutoUpdateAwbWithAllFalseHawbLockSettingsHawb() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(shipmentId);
        createAwbRequest.setAwbType(Constants.HAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        HawbLockSettings hawbLockSettings = jsonTestUtility.getJson("HAWB_LOCK_SETTINGS_ALL_FALSE", HawbLockSettings.class);

        Awb mockAwb = testHawb;
        AwbNotifyPartyInfo awbNotifyPartyInfo = AwbNotifyPartyInfo.builder().isShipmentCreated(true).build();
        ArrayList<AwbNotifyPartyInfo> awbNotifyPartyInfoList = new ArrayList<>();
        awbNotifyPartyInfoList.add(awbNotifyPartyInfo);
        mockAwb.setAwbNotifyPartyInfo(awbNotifyPartyInfoList);
        mockAwb.getAwbPackingInfo().get(0).setGuid(testShipment.getPackingList().get(0).getGuid());
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addShipmentDataForAwbGeneration(testShipment);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setHawbLockSettings(hawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(mockAwb));
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        mockShipmentSettings();
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));
        var httpResponse = awbService.partialAutoUpdateAwb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);

    }

    @Test
    void partialAutoUpdateAwbWithAllFalseHawbLockSettingsHawbGeneratesDefaultInfo() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(shipmentId);
        createAwbRequest.setAwbType(Constants.HAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        HawbLockSettings hawbLockSettings = jsonTestUtility.getJson("HAWB_LOCK_SETTINGS_ALL_FALSE", HawbLockSettings.class);

        Awb mockAwb = testHawb;
        mockAwb.setAwbRoutingInfo(null);
        mockAwb.setAwbGoodsDescriptionInfo(null);
        mockAwb.getAwbPackingInfo().get(0).setGuid(testShipment.getPackingList().get(0).getGuid());
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addShipmentDataForAwbGeneration(testShipment);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setHawbLockSettings(hawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(mockAwb));
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        mockShipmentSettings();
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));
        var httpResponse = awbService.partialAutoUpdateAwb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);

    }

    @Test
    void partialAutoUpdateAwbWithAllTrueMawbLockSettingsDmawb() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(shipmentId);
        createAwbRequest.setAwbType(Constants.DMAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        MawbLockSettings mawbLockSettings = jsonTestUtility.getJson("MAWB_LOCK_SETTINGS_ALL_TRUE", MawbLockSettings.class);

        Awb mockAwb = testDmawb;
        mockAwb.getAwbPackingInfo().get(0).setGuid(testShipment.getPackingList().get(0).getGuid());
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addShipmentDataForAwbGeneration(testShipment);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setMawbLockSettings(mawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(mockAwb));
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
//        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        mockShipmentSettings();
        var httpResponse = awbService.partialAutoUpdateAwb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);

    }

    @Test
    void partialAutoUpdateAwbWithAllFalseMawbLockSettingsDmawb() throws RunnerException {
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setShipmentId(shipmentId);
        createAwbRequest.setAwbType(Constants.DMAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        MawbLockSettings mawbLockSettings = jsonTestUtility.getJson("MAWB_LOCK_SETTINGS_ALL_FALSE", MawbLockSettings.class);

        Awb mockAwb = testDmawb;
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addShipmentDataForAwbGeneration(testShipment);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setMawbLockSettings(mawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(mockAwb));
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        mockShipmentSettings();

        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));

        var httpResponse = awbService.partialAutoUpdateAwb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);

    }

    @Test
    void partialAutoUpdateMawbThrowsExceptionEmptyRequest() throws RunnerException {
        Long consolidationId = 1L;
        CreateAwbRequest createAwbRequest = null;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        var e = assertThrows(ValidationException.class, () ->
                awbService.partialAutoUpdateMawb(commonRequestModel));

        assertNotNull(e);
    }

    @Test
    void partialAutoUpdateMawbThrowsExceptionEmptyConsolId() throws RunnerException {
        Long consolidationId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setConsolidationId(null);
        createAwbRequest.setAwbType(Constants.MAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        var e = assertThrows(ValidationException.class, () ->
                awbService.partialAutoUpdateMawb(commonRequestModel));
        assertNotNull(e);

    }

    @Test
    void partialAutoUpdateMawbThrowsExceptionWhenAwbNotPresent() throws RunnerException {
        Long consolidationId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setConsolidationId(consolidationId);
        createAwbRequest.setAwbType(Constants.MAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        // Mocking
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(Collections.emptyList());

        var e = assertThrows(ValidationException.class, () ->
                awbService.partialAutoUpdateMawb(commonRequestModel));

        assertNotNull(e);
    }


    @Test
    void partialAutoUpdateMawbWithRestrictAwbEditFlagTrue() throws RunnerException {
        Long consolidationId = 1L;
        Long shipmentId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setConsolidationId(consolidationId);
        createAwbRequest.setAwbType(Constants.MAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);

        MawbLockSettings mawbLockSettings = jsonTestUtility.getJson("MAWB_LOCK_SETTINGS_ALL_TRUE", MawbLockSettings.class);

        Awb mockAwb = testMawb;
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addConsolDataForMawbGeneration(testConsol);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setMawbLockSettings(mawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        tenantSettings.setRestrictAWBEdit(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(mockAwb));
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
//        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());


        // Reset Mocking
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));


        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testMawb));
        Mockito.when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.findByShipmentIdList(anyList())).thenReturn(List.of(testHawb));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);

        when(awbDao.save(any())).thenReturn(testMawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));


        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        mockShipmentSettings();
        mockTenantSettings();

        var httpResponse = awbService.partialAutoUpdateMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

    @Test
    void partialAutoUpdateMawbWithAllTrueMawbLockSettings() throws RunnerException {
        Long consolidationId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setConsolidationId(consolidationId);
        createAwbRequest.setAwbType(Constants.MAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);

        MawbLockSettings mawbLockSettings = jsonTestUtility.getJson("MAWB_LOCK_SETTINGS_ALL_TRUE", MawbLockSettings.class);

        Awb mockAwb = testMawb;
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(1L);
        testConsol.setShipmentsList(List.of(testShipment));

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setMawbLockSettings(mawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByShipmentId(1L)).thenReturn(List.of(testHawb));
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(mockAwb));
        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(testConsol));
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));
//        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        mockShipmentSettings();

        var httpResponse = awbService.partialAutoUpdateMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

    @Test
    void partialAutoUpdateMawbWithAllFalseMawbLockSettings() throws RunnerException {
        Long consolidationId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setConsolidationId(consolidationId);
        createAwbRequest.setAwbType(Constants.MAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);
        MawbLockSettings mawbLockSettings = jsonTestUtility.getJson("MAWB_LOCK_SETTINGS_ALL_FALSE", MawbLockSettings.class);

        Awb mockAwb = testMawb;
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addConsolDataForMawbGeneration(testConsol);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setMawbLockSettings(mawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(mockAwb));
        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(testConsol));
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));
        mockShipmentSettings();
        var httpResponse = awbService.partialAutoUpdateMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

    @Test
    void partialAutoUpdateMawbDeletesAwbNotifyPartyElementsOnMatchingGuidFromConsolidationAddressList() throws RunnerException {
        Long consolidationId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setConsolidationId(consolidationId);
        createAwbRequest.setAwbType(Constants.MAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);
        MawbLockSettings mawbLockSettings = jsonTestUtility.getJson("MAWB_LOCK_SETTINGS_ALL_FALSE", MawbLockSettings.class);

        Awb mockAwb = testMawb;
        addConsolDataForMawbGeneration(testConsol);
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        AwbNotifyPartyInfo awbNotifyPartyInfo = AwbNotifyPartyInfo.builder().isShipmentCreated(true).guid(
                testConsol.getConsolidationAddresses().get(0).getGuid()
        ).build();
        List<AwbNotifyPartyInfo> awbNotifyPartyInfoList = new ArrayList<>();
        awbNotifyPartyInfoList.add(awbNotifyPartyInfo);
        mockAwb.setAwbNotifyPartyInfo(awbNotifyPartyInfoList);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setMawbLockSettings(mawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(mockAwb));
        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(testConsol));
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        mockShipmentSettings();
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));
        var httpResponse = awbService.partialAutoUpdateMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

    @Test
    void partialAutoUpdateMawbGeneratesRoutingInfoFromCarrierDetails() throws RunnerException {
        Long consolidationId = 1L;
        CreateAwbRequest createAwbRequest = new CreateAwbRequest();
        createAwbRequest.setConsolidationId(consolidationId);
        createAwbRequest.setAwbType(Constants.MAWB);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(createAwbRequest);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);
        MawbLockSettings mawbLockSettings = jsonTestUtility.getJson("MAWB_LOCK_SETTINGS_ALL_FALSE", MawbLockSettings.class);

        Awb mockAwb = testMawb;
        mockAwb.setAwbRoutingInfo(null);
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);
        addConsolDataForMawbGeneration(testConsol);

        var tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setAutoUpdateShipmentAWB(true);
        tenantSettings.setMawbLockSettings(mawbLockSettings);
        tenantSettings.setWeightChargeableUnit("KG");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        // Mocking
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(mockAwb));
        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(testConsol));
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
        when(awbDao.save(mockAwb)).thenReturn(mockAwb);
        when(jsonHelper.convertValue(anyString(), eq(LocalDateTime.class))).thenReturn(LocalDateTime.now());
        when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(mockAwbResponse);
        mockShipmentSettings();

        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));

        var httpResponse = awbService.partialAutoUpdateMawb(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(mockAwbResponse), httpResponse);
    }

  @Test
  void generateAwbPaymentInfoWithAllIdentifierFalse() throws JsonProcessingException, RunnerException {
        GenerateAwbPaymentInfoRequest request = jsonTestUtility.getJson("AWB_GENERATE_PAYMENT_INFO_PAYLOAD", GenerateAwbPaymentInfoRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        Mockito.when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build()));
        AwbCalculationResponse generatePaymentResponse = jsonTestUtility.getJson("AWB_CALCULATION_RESPONSE", AwbCalculationResponse.class);
        generatePaymentResponse.getAwbPaymentInfo().setTotalPrepaid(BigDecimal.valueOf(0.0));
        ResponseEntity<IRunnerResponse> httpResponse = awbService.generateAwbPaymentInfo(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(generatePaymentResponse, objectMapper.convertValue(runnerResponse.getData(), AwbCalculationResponse.class));
    }

    @Test
    void generateAwbPaymentInfoWithAllIdentifierTrue() throws JsonProcessingException, RunnerException {
        GenerateAwbPaymentInfoRequest request = jsonTestUtility.getJson("AWB_GENERATE_PAYMENT_INFO_PAYLOAD", GenerateAwbPaymentInfoRequest.class);
        request.getChargeDetails().setIdentifier1("true");
        request.getChargeDetails().setIdentifier2("true");
        request.getChargeDetails().setIdentifier3("true");
        request.getChargeDetails().setIdentifier4("true");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        Mockito.when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build()));
        AwbCalculationResponse generatePaymentResponse = jsonTestUtility.getJson("AWB_CALCULATION_RESPONSE", AwbCalculationResponse.class);
        generatePaymentResponse.getAwbPaymentInfo().setTotalCollect(
            generatePaymentResponse.getAwbPaymentInfo().getTotalPrepaid()
        );
        ResponseEntity<IRunnerResponse> httpResponse = awbService.generateAwbPaymentInfo(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(generatePaymentResponse, objectMapper.convertValue(runnerResponse.getData(), AwbCalculationResponse.class));
    }

    @ParameterizedTest
    @ValueSource(ints = {1,2,3})
    void generateAwbPaymentInfoWithChargeBasis(int chargeBasis) throws JsonProcessingException, RunnerException {
        GenerateAwbPaymentInfoRequest request = jsonTestUtility.getJson("AWB_GENERATE_PAYMENT_INFO_PAYLOAD", GenerateAwbPaymentInfoRequest.class);
        request.getChargeDetails().setIdentifier1("true");
        request.getChargeDetails().setIdentifier2("true");
        request.getChargeDetails().setIdentifier3("true");
        request.getChargeDetails().setIdentifier4("true");

        request.getAwbOtherChargesInfo().get(0).setChargeBasis(chargeBasis);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        Mockito.when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build()));
        AwbCalculationResponse generatePaymentResponse = jsonTestUtility.getJson("AWB_CALCULATION_RESPONSE", AwbCalculationResponse.class);
        generatePaymentResponse.getAwbOtherChargesInfo().get(0).setChargeBasis(chargeBasis);
        BigDecimal zero = new BigDecimal(0);
        var chargeableWt = request.getAwbGoodsDescriptionInfo().get(0).getChargeableWt();
        var grossWt = request.getAwbGoodsDescriptionInfo().get(0).getGrossWt();
        var rate = generatePaymentResponse.getAwbOtherChargesInfo().get(0).getRate();
        if(chargeBasis == 2) {
            BigDecimal val = rate.multiply(chargeableWt);
            generatePaymentResponse.getAwbOtherChargesInfo().get(0).setAmount(val);
            generatePaymentResponse.getAwbPaymentInfo().setTotalPrepaid(val.setScale(1, RoundingMode.HALF_DOWN));
            generatePaymentResponse.getAwbPaymentInfo().setDueCarrierCharges(val);
        }
        if(chargeBasis == 3){
            BigDecimal val = rate.multiply(grossWt);
            generatePaymentResponse.getAwbOtherChargesInfo().get(0).setAmount(val);
            generatePaymentResponse.getAwbPaymentInfo().setTotalPrepaid(val.setScale(1, RoundingMode.HALF_DOWN));
            generatePaymentResponse.getAwbPaymentInfo().setDueCarrierCharges(val);
        }
        generatePaymentResponse.getAwbPaymentInfo().setTotalCollect(
            generatePaymentResponse.getAwbPaymentInfo().getTotalPrepaid()
        );
        ResponseEntity<IRunnerResponse> httpResponse = awbService.generateAwbPaymentInfo(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(generatePaymentResponse, objectMapper.convertValue(runnerResponse.getData(), AwbCalculationResponse.class));
    }

    @Test
    void retrieveByAwbByMawb() {
        // Build request
        Long mawbId = 3L;
        CommonGetRequest commonGetRequest  = CommonGetRequest.builder().id(mawbId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        //Mocking
        MawbHawbLink link = MawbHawbLink.builder().hawbId(2L).mawbId(3L).build();
        when(mawbHawbLinkDao.findByMawbId(any())).thenReturn(List.of(link));

        List<AwbResponse> awbResponse = jsonTestUtility.convertValueToList(List.of(testHawb), AwbResponse.class);
        when(jsonHelper.convertValueToList(any(), eq(AwbResponse.class))).thenReturn(awbResponse);

        ResponseEntity<IRunnerResponse> response = awbService.retrieveByAwbByMawb(commonRequestModel);
        assertEquals(response.getStatusCode(), HttpStatus.OK);
    }

    @Test
    void retrieveByAwbByMawbFailsWhenAwbNotPresent() {
        // Build request
        Long mawbId = 3L;
        CommonGetRequest commonGetRequest  = CommonGetRequest.builder().id(mawbId).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);


        var spyBean = Mockito.spy(awbService);
        doReturn(null).when(spyBean).getLinkedAwbFromMawb(anyLong());

        ResponseEntity<IRunnerResponse> response = spyBean.retrieveByAwbByMawb(commonRequestModel);
        assertEquals(response.getStatusCode(), HttpStatus.BAD_REQUEST);
    }

    @Test
    void generateUpdatedNatureAndQuantGoodsField() throws RunnerException, JsonProcessingException {
        GenerateAwbPaymentInfoRequest request =
            objectMapper.readValue(
                "{\"awbShipmentInfo\":{\"entityId\":152616,\"entityType\":\"DMAWB\",\"shipperName\":\"TEST VG CREDIT LIMIT\",\"shipperAddress\":\"\\r\\nADDRESS 1\\r\\nMH\\r\\nPUNE\\r\\nIND\",\"consigneeName\":\"NATIONWIDE PAPER LTD\",\"consigneeAddress\":\"\\r\\nABC CO LLC\\nAEJEA\\r\\nARE\",\"consigneeReferenceNumber\":\"2436215\",\"iataCode\":\"5678\",\"awbNumber\":\"235-88998641\",\"originAirport\":\"Kempegowda International Airport BLR\",\"destinationAirport\":\"George Bush Intercontinental Airport IAH, TX\",\"firstCarrier\":\"Turkish Airlines\",\"agentCASSCode\":\"8975\"},\"awbNotifyPartyInfo\":[{}],\"awbRoutingInfo\":[{\"entityId\":152616,\"entityType\":\"DMAWB\",\"byCarrier\":\"Turkish Airlines\",\"flightNumber\":\"6457\",\"flightDate\":\"2024-02-02T01:00:00\",\"destinationPortName\":\"USIAH_AIR\",\"originPortName\":\"INBLR_AIR\",\"isShipmentCreated\":true}],\"awbCargoInfo\":{\"entityId\":152616,\"entityType\":\"DMAWB\",\"ntrQtyGoods\":\"\",\"currency\":\"EGP\",\"customsValue\":0},\"awbOtherInfo\":{\"entityId\":152616,\"entityType\":\"DMAWB\",\"shipper\":\"TEST VG CREDIT LIMIT\",\"carrier\":\"carrier for mawb from master\",\"executedOn\":\"2024-03-07T12:56:16\"},\"awbGoodsDescriptionInfo\":[{\"entityId\":152616,\"entityType\":\"DMAWB\",\"piecesNo\":6,\"guid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\",\"awbPackingInfo\":[{\"guid\":\"38bc225a-fa41-40a5-afae-71a11a524dfa\",\"packs\":\"5\",\"packsType\":\"BAG\",\"weight\":25,\"weightUnit\":\"KG\",\"volume\":1.2,\"volumeUnit\":\"M3\",\"commodity\":\"530521\",\"length\":400,\"lengthUnit\":\"CM\",\"width\":300,\"widthUnit\":\"CM\",\"height\":2,\"heightUnit\":\"CM\",\"hsCode\":\"530521\",\"volumeWeight\":200.0004,\"volumeWeightUnit\":\"KG\",\"awbNumber\":\"235-88998641\",\"awbGoodsDescriptionInfoGuid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\"},{\"guid\":\"4fbd511c-26cc-48ae-a2ae-5fe5d9baf803\",\"packs\":\"1\",\"packsType\":\"BAG\",\"weight\":100,\"weightUnit\":\"KG\",\"volume\":0.2,\"volumeUnit\":\"M3\",\"commodity\":\"530529\",\"length\":100,\"lengthUnit\":\"CM\",\"width\":200,\"widthUnit\":\"CM\",\"height\":10,\"heightUnit\":\"CM\",\"minTempUnit\":\"CEL\",\"hsCode\":\"530529\",\"goodsDescription\":\"DESC\",\"netWeightUnit\":\"KG\",\"volumeWeight\":33.3334,\"volumeWeightUnit\":\"KG\",\"awbNumber\":\"235-88998641\",\"awbGoodsDescriptionInfoGuid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\"}],\"isShipmentCreated\":true}],\"prepaid\":{\"weightCharges\":\"\",\"valuationCharge\":\"\",\"tax\":\"\",\"dueAgentCharges\":\"\",\"dueCarrierCharges\":\"\"},\"collect\":{\"weightCharges\":\"\",\"valuationCharge\":\"\",\"tax\":\"\",\"dueAgentCharges\":\"\",\"dueCarrierCharges\":\"\"},\"awbOciInfo\":[],\"awbPackingInfo\":[{\"guid\":\"38bc225a-fa41-40a5-afae-71a11a524dfa\",\"packs\":\"5\",\"packsType\":\"BAG\",\"weight\":25,\"weightUnit\":\"KG\",\"volume\":1.2,\"volumeUnit\":\"M3\",\"commodity\":\"530521\",\"length\":400,\"lengthUnit\":\"CM\",\"width\":300,\"widthUnit\":\"CM\",\"height\":2,\"heightUnit\":\"CM\",\"hsCode\":\"530521\",\"volumeWeight\":200.0004,\"volumeWeightUnit\":\"KG\",\"awbNumber\":\"235-88998641\",\"awbGoodsDescriptionInfoGuid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\"},{\"guid\":\"4fbd511c-26cc-48ae-a2ae-5fe5d9baf803\",\"packs\":\"1\",\"packsType\":\"BAG\",\"weight\":100,\"weightUnit\":\"KG\",\"volume\":0.2,\"volumeUnit\":\"M3\",\"commodity\":\"530529\",\"length\":100,\"lengthUnit\":\"CM\",\"width\":200,\"widthUnit\":\"CM\",\"height\":10,\"heightUnit\":\"CM\",\"minTempUnit\":\"CEL\",\"hsCode\":\"530529\",\"goodsDescription\":\"DESC\",\"netWeightUnit\":\"KG\",\"volumeWeight\":33.3334,\"volumeWeightUnit\":\"KG\",\"awbNumber\":\"235-88998641\",\"awbGoodsDescriptionInfoGuid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\"}],\"awbOtherChargesInfo\":[],\"shcIdList\":[],\"chargeDetails\":{\"Id\":\"\",\"ItemType\":\"\",\"ItemValue\":\"\",\"ItemDescription\":\"\",\"ValuenDesc\":\"\",\"Identifier1\":\"\",\"Identifier2\":\"\",\"Identifier3\":\"\",\"Identifier4\":\"\",\"TenantId\":\"\",\"IsActive\":\"\",\"AirLinePrefixValue\":\"\",\"DPAManifestMaster\":\"\",\"label\":\"\",\"value\":\"\"},\"awbPaymentInfo\":{\"weightCharges\":\"\",\"valuationCharge\":\"\",\"tax\":\"\",\"dueAgentCharges\":\"\",\"totalPrepaid\":\"\",\"totalCollect\":\"\"},\"id\":68,\"guid\":\"4545424c-fe16-48c4-b1e9-230589324dba\",\"awbNumber\":\"235-88998641\",\"shipmentId\":152616}",
                GenerateAwbPaymentInfoRequest.class);

        ResponseEntity<IRunnerResponse> response = awbService.generateUpdatedNatureAndQuantGoodsField(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, response.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(response.getBody(), RunnerResponse.class);
        assertEquals(
                "DIMS: In CMS\r\n" + "5=400X300X2,1=100X200X10\r\n",
                runnerResponse.getData());
      }

    @Test
    void getChargeTypeMasterData_fails_when_empty_id() {
        // build req
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        Exception e = assertThrows(RunnerException.class, () -> awbService.getChargeTypeMasterData(getRequest));

        assertEquals("Please provide a valid Id", e.getMessage());
    }

    @Test
    void getChargeTypeMasterData_success() throws RunnerException {
        // build req
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();

        ChargeTypeIntegrations c1 = new ChargeTypeIntegrations();
        c1.setIntegrationType(ChargeTypeCode.IATA_Charge_Code);
        c1.setIntegrationCode("ER");
        ChargeTypeIntegrations c2 = new ChargeTypeIntegrations();
        c2.setIntegrationType(ChargeTypeCode.Due_To_Party);
        c2.setChargeDue(2);

        // Mocking
        when(v1Service.retrieveChargeCodeData(any())).thenReturn(new V1RetrieveResponse());
        when(jsonHelper.convertValue(any(), eq(EntityTransferChargeType.class))).thenReturn(EntityTransferChargeType.
                builder().ChargeTypeIntegrations(List.of(c1,c2)).build());
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = awbService.getChargeTypeMasterData(getRequest);
        var mockResponse = new AwbChargeTypeMasterDataResponse();
        mockResponse.setIataDescription("ER");
        mockResponse.setChargeDue(2);
        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
    }

    @Test
    void testValidateIataAgentFromShipment() {

        IataAgentResponse mockResponse = IataAgentResponse.builder().iataAgent(true).message("FWB will be sent before printing, do you want to proceed?").build();

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.IATAAgent = true;
        mockTenantModel.AgentIATACode = "test-code";
        mockTenantModel.AgentCASSCode = "test-code";
        mockTenantModel.PIMAAddress = "test-addr";
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        var httpResponse = awbService.validateIataAgent(true, Optional.empty());

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
    }

    @Test
    void testValidateIataAgent() {

        IataAgentResponse mockResponse = IataAgentResponse.builder().iataAgent(true).message("FWB & FZB  will be sent before printing, do you want to proceed?").build();

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.IATAAgent = true;
        mockTenantModel.AgentIATACode = "test-code";
        mockTenantModel.AgentCASSCode = "test-code";
        mockTenantModel.PIMAAddress = "test-addr";
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        var httpResponse = awbService.validateIataAgent(false, Optional.empty());

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
    }

    @Test
    void testValidateIataAgent2() {
        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.IATAAgent = true;
        mockTenantModel.AgentIATACode = "test-code";
        mockTenantModel.AgentCASSCode = "test-code";
        mockTenantModel.PIMAAddress = "test-addr";
        mockTenantModel.DefaultOrgId = 1L;

        var mockShipmentConsoleMapping1 = ConsoleShipmentMapping.builder().shipmentId(10L).consolidationId(100L).build();
        var mockShipmentConsoleMapping2 = ConsoleShipmentMapping.builder().shipmentId(11L).consolidationId(100L).build();

        var mockShipmentDetails1 = new ShipmentDetails();
        mockShipmentDetails1.setId(10L);
        mockShipmentDetails1.setShipmentId("SHP00001");

        var mockShipmentDetails2 = new ShipmentDetails();
        mockShipmentDetails2.setId(11L);
        mockShipmentDetails2.setShipmentId("SHP00011");

        var mockAWB1 = new Awb();
        mockAWB1.setShipmentId(10L);
        mockAWB1.setPrintType(PrintType.ORIGINAL_PRINTED);

        var mockAWB2 = new Awb();
        mockAWB2.setShipmentId(12L);
        mockAWB2.setPrintType(PrintType.DRAFT_PRINTED);

        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(mockShipmentConsoleMapping1, mockShipmentConsoleMapping2));
        when(shipmentDao.getShipmentNumberFromId(anyList())).thenReturn(List.of(mockShipmentDetails1, mockShipmentDetails2));
        when(awbDao.findByShipmentIdsByQuery(anyList())).thenReturn(List.of(mockAWB1, mockAWB2));

        var throwable = assertThrows(Throwable.class, () -> awbService.validateIataAgent(false, Optional.of(100L)));
        assertEquals(ValidationException.class.getSimpleName(), throwable.getClass().getSimpleName());
        assertFalse(throwable.getMessage().isEmpty());
    }

    @Test
    void testValidateIataAgentWithNullPIMAAddress() {

        IataAgentResponse mockResponse = IataAgentResponse.builder().iataAgent(false).message("PIMA address is not added in the branch settings, FWB and FZB will not be sent").build();

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.IATAAgent = true;
        mockTenantModel.AgentIATACode = "test-code";
        mockTenantModel.AgentCASSCode = "test-code";
//        mockTenantModel.PIMAAddress = "test-addr";
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        var httpResponse = awbService.validateIataAgent(false, Optional.empty());

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
    }

    @Test
    void testValidateIataAgentDefaultResponse() {

        IataAgentResponse mockResponse = IataAgentResponse.builder().iataAgent(false).build();

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        var httpResponse = awbService.validateIataAgent(false, Optional.empty());

        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
    }


    @Test
    void testFnmStatusMessageForHawbFailure() {
        Long shipmentId = 1L;

        AirMessagingLogs statusLog = new AirMessagingLogs();
        statusLog.setStatus(AirMessagingStatus.FAILED.name());
        String responseMessage = String.format(AirMessagingLogsConstants.SHIPMENT_FNM_FAILURE_ERROR, statusLog.getErrorMessage());
        FnmStatusMessageResponse fnmStatusMessageResponse = FnmStatusMessageResponse.builder().fnmStatus(false).response(responseMessage).build();


        // Mock
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(testHawb));
        when(airMessagingLogsService.getRecentLogForEntityGuid(testHawb.getGuid())).thenReturn(statusLog);

        // Test
        var httpResponse = awbService.getFnmStatusMessage(Optional.of(shipmentId), Optional.empty());

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(fnmStatusMessageResponse), httpResponse);
    }

    @Test
    void testFnmStatusMessageForHawbFailure1() {
        Long shipmentId = 1L;

        AirMessagingLogs statusLog = new AirMessagingLogs();
        statusLog.setStatus(AirMessagingStatus.FAILED.name());

        // Mock
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of());

        // Test
        var httpResponse = awbService.getFnmStatusMessage(Optional.of(shipmentId), Optional.empty());

        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testFnmStatusMessageForHawbSuccess() {
        Long shipmentId = 1L;

        AirMessagingLogs statusLog = new AirMessagingLogs();
        statusLog.setStatus(AirMessagingStatus.SUCCESS.name());
        String responseMessage = "FZB submission is accepted";
        FnmStatusMessageResponse fnmStatusMessageResponse = FnmStatusMessageResponse.builder().fnmStatus(true).response(responseMessage).build();


        // Mock
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(testHawb));
        when(airMessagingLogsService.getRecentLogForEntityGuid(testHawb.getGuid())).thenReturn(statusLog);

        // Test
        var httpResponse = awbService.getFnmStatusMessage(Optional.of(shipmentId), Optional.empty());

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(fnmStatusMessageResponse), httpResponse);
    }

    @Test
    void testFnmStatusMessageForDmawb() {
        Long shipmentId = 1L;

        AirMessagingLogs statusLog = new AirMessagingLogs();
        statusLog.setStatus(AirMessagingStatus.SUCCESS.name());
        String responseMessage = "FZB submission is accepted";
        FnmStatusMessageResponse fnmStatusMessageResponse = FnmStatusMessageResponse.builder().fnmStatus(true).response(responseMessage).build();


        // Mock
        when(awbDao.findByShipmentId(shipmentId)).thenReturn(List.of(testDmawb));
        when(airMessagingLogsService.getRecentLogForEntityGuid(testDmawb.getGuid())).thenReturn(statusLog);

        // Test
        var httpResponse = awbService.getFnmStatusMessage(Optional.of(shipmentId), Optional.empty());

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(fnmStatusMessageResponse), httpResponse);
    }

    @Test
    void testFnmStatusMessageForMawbAndHawbSuccess() {
        Long consolidationId = 1L;

        AirMessagingLogs statusLog = new AirMessagingLogs();
        statusLog.setStatus(AirMessagingStatus.SUCCESS.name());
        String responseMessage = "FWB and FZB submissions are accepted";
        FnmStatusMessageResponse fnmStatusMessageResponse = FnmStatusMessageResponse.builder().fnmStatus(true).response(responseMessage).build();

        // Mock
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(testMawb));
        when(airMessagingLogsService.getRecentLogForEntityGuid(testMawb.getGuid())).thenReturn(statusLog);

        // Test
        var httpResponse = awbService.getFnmStatusMessage(Optional.empty(),Optional.of(consolidationId));

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(fnmStatusMessageResponse), httpResponse);

    }

    @Test
    void testFnmStatusMessageForMawbFailureHawbSuccess() {
        Long consolidationId = 1L;

        AirMessagingLogs successLog = new AirMessagingLogs();
        successLog.setStatus(AirMessagingStatus.SUCCESS.name());
        AirMessagingLogs failureLog = new AirMessagingLogs();
        failureLog.setStatus(AirMessagingStatus.FAILED.name());

        String responseMessage = String.format(
                AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_FAILURE_HAWB_SUCCESS_ERROR, failureLog.getErrorMessage());
        FnmStatusMessageResponse fnmStatusMessageResponse = FnmStatusMessageResponse.builder().fnmStatus(false).response(responseMessage).build();



        // Mock
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(testMawb));
        when(airMessagingLogsService.getRecentLogForEntityGuid(testMawb.getGuid())).thenReturn(failureLog);

        // Test
        var httpResponse = awbService.getFnmStatusMessage(Optional.empty(),Optional.of(consolidationId));

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(fnmStatusMessageResponse), httpResponse);

    }

    @Test
    void testFnmStatusMessageForMawbFailureHawbFailure() {
        Long consolidationId = 1L;

        AirMessagingLogs successLog = new AirMessagingLogs();
        successLog.setStatus(AirMessagingStatus.SUCCESS.name());
        AirMessagingLogs failureLog = new AirMessagingLogs();
        failureLog.setStatus(AirMessagingStatus.FAILED.name());

        Page<ShipmentDetails> shipmentDetailsPage = new PageImpl(List.of(testShipment));
        List<String> shipmentNumbers = shipmentDetailsPage.getContent().stream().map(i -> i.getShipmentId()).toList();
        String shipmentNumbersString = String.join(" ", shipmentNumbers);

        String responseMessage = String.format(
                AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_FAILURE_HAWB_FAILURE_ERROR, shipmentNumbersString);
        FnmStatusMessageResponse fnmStatusMessageResponse = FnmStatusMessageResponse.builder().fnmStatus(false).response(responseMessage).build();

        // Mock
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(testMawb));
        when(airMessagingLogsService.getRecentLogForEntityGuid(testMawb.getGuid())).thenReturn(failureLog);
        when(airMessagingLogsService.getRecentLogForEntityGuid(testHawb.getGuid())).thenReturn(failureLog);
        when(awbDao.findByIds(any())).thenReturn(List.of(testHawb));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(testShipment));

        // Test
        var httpResponse = awbService.getFnmStatusMessage(Optional.empty(),Optional.of(consolidationId));

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(fnmStatusMessageResponse), httpResponse);

    }

    @Test
    void testFnmStatusMessageForMawbSuccessHawbFailure() {
        Long consolidationId = 1L;

        AirMessagingLogs successLog = new AirMessagingLogs();
        successLog.setStatus(AirMessagingStatus.SUCCESS.name());
        AirMessagingLogs failureLog = new AirMessagingLogs();
        failureLog.setStatus(AirMessagingStatus.FAILED.name());

        Page<ShipmentDetails> shipmentDetailsPage = new PageImpl(List.of(testShipment));
        List<String> shipmentNumbers = shipmentDetailsPage.getContent().stream().map(i -> i.getShipmentId()).toList();
        String shipmentNumbersString = String.join(" ", shipmentNumbers);

        String responseMessage = String.format(
                AirMessagingLogsConstants.CONSOLIDATION_FNM_MAWB_SUCCESS_HAWB_FAILURE_ERROR, shipmentNumbersString);
        FnmStatusMessageResponse fnmStatusMessageResponse = FnmStatusMessageResponse.builder().fnmStatus(false).response(responseMessage).build();

        // Mock
        when(awbDao.findByConsolidationId(consolidationId)).thenReturn(List.of(testMawb));
        when(airMessagingLogsService.getRecentLogForEntityGuid(testMawb.getGuid())).thenReturn(successLog);
        when(airMessagingLogsService.getRecentLogForEntityGuid(testHawb.getGuid())).thenReturn(failureLog);
        when(awbDao.findByIds(any())).thenReturn(List.of(testHawb));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(testShipment));

        // Test
        var httpResponse = awbService.getFnmStatusMessage(Optional.empty(),Optional.of(consolidationId));

        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());

    }

    private ListCommonRequest constructListCommonRequest(String fieldName, Object value, String operator) {
        ListCommonRequest request = new ListCommonRequest();
        request.setPageNo(1);
        request.setPageSize(Integer.MAX_VALUE);

        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilters.add(filterCriteria);
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        request.setFilterCriteria(criterias);
        return request;
    }

    private FetchAwbListRequest contructFetchAwbListRequest(String fieldName, Object value, String operator) {
        FetchAwbListRequest request = new FetchAwbListRequest();
        request.setPageNo(1);
        request.setPageSize(Integer.MAX_VALUE);

        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList();
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        FilterCriteria filterCriteria = FilterCriteria.builder().criteria(criteria).build();
        innerFilters.add(filterCriteria);
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        request.setFilterCriteria(criterias);
        return request;
    }


    private void addShipmentDataForAwbGeneration(ShipmentDetails shipment) {
        Parties shipmentAddress = Parties.builder().type(Constants.FAG).build();
        shipmentAddress.setOrgData(Map.ofEntries(
                Map.entry(ReportConstants.COUNTRY, "test"),
                Map.entry(ReportConstants.CITY, "test_city"),
                Map.entry("OrgCode", "223532_5")
        ));

        Routings routing = new Routings();
        routing.setLeg(1L);

        shipment.setShipmentAddresses(List.of(shipmentAddress));
//        shipment.setRoutingsList(List.of(routing));
    }

    private void addConsolDataForMawbGeneration(ConsolidationDetails consolidationDetails) {
        List<String> guids = List.of("12522638-fb00-44c9-8c3f-f8c85c2d387d","fdbe4ba4-3a20-4e2c-ba83-9390a027b940","7e719d65-8a45-4537-a5ad-f675e91d641d");
        Parties consolidationAddress1 = Parties.builder().type("Notify Part 1").build();
        consolidationAddress1.setOrgData(Map.ofEntries(
                Map.entry("Id", 1),
                Map.entry("Type", "Notify Part 1")
        ));
        consolidationAddress1.setGuid(UUID.fromString(guids.get(0)));
        consolidationAddress1.setOrgCode("223532_4");
        Parties consolidationAddress2 = Parties.builder().type("Notify Part 2").build();
        consolidationAddress2.setOrgData(Map.ofEntries(Map.entry("Id", 2)));
        consolidationAddress2.setGuid(UUID.fromString(guids.get(1)));
        consolidationAddress2.setOrgCode("223532_5");
        Parties consolidationAddress3 = Parties.builder().type("Notify Part 3").build();
        consolidationAddress3.setOrgData(Map.ofEntries(Map.entry("Id", 3)));
        consolidationAddress3.setGuid(UUID.fromString(guids.get(2)));
        consolidationAddress3.setOrgCode("223532_6");
        Parties consolidationAddress4 = Parties.builder().type(Constants.FAG).build();
        consolidationAddress4.setOrgData(Map.ofEntries(Map.entry(ReportConstants.COUNTRY, "test-country")));
        consolidationAddress4.setOrgCode("LL_QR_25300");


        consolidationDetails.setConsolidationAddresses(List.of(
                consolidationAddress1, consolidationAddress2, consolidationAddress3,
                consolidationAddress4
        ));
    }


    @Test
    void getAllMasterDataForHawb() {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);
        boolean isShipment = true;

        Awb mockAwb = testHawb;
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);

        when(awbDao.findByShipmentId(id)).thenReturn(List.of(mockAwb));
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        var res = awbService.getAllMasterData(commonRequestModel, isShipment);

        assertNotNull(res);
        assertEquals(HttpStatus.OK, res.getStatusCode());
    }

    @Test
    void getAllMasterDataForMawb() {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);
        boolean isShipment = false;

        Awb mockAwb = testMawb;
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);

        when(awbDao.findByConsolidationId(id)).thenReturn(List.of(mockAwb));
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockAwbResponse);

        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });

        var res = awbService.getAllMasterData(commonRequestModel, isShipment);

        assertNotNull(res);
        assertEquals(HttpStatus.OK, res.getStatusCode());
    }

    @Test
    void getAllMasterDataFailsOnNoAwbPresent() {
        Long id = 1L;
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(id);
        boolean isShipment = true;

        Awb mockAwb = testMawb;
        AwbResponse mockAwbResponse = objectMapper.convertValue(mockAwb, AwbResponse.class);

        when(awbDao.findByShipmentId(id)).thenReturn(Collections.emptyList());

        var res = awbService.getAllMasterData(commonRequestModel, isShipment);

        assertNotNull(res);
        assertEquals(HttpStatus.BAD_REQUEST, res.getStatusCode());
    }

    static Stream<Arguments> provideRequestsForValidationException() {
        return Stream.of(
                Arguments.of(BigDecimal.valueOf(12.0), RateClass.N.getId(), "5.0"),
                Arguments.of(BigDecimal.valueOf(10.0), RateClass.M.getId(), "52.0"),
                Arguments.of(BigDecimal.valueOf(100.0), RateClass.Q.getId(), "3.0")
        );
    }

    @ParameterizedTest
    @MethodSource("provideRequestsForValidationException")
    void testGetFetchIataRates_Success_case1(BigDecimal chargeableWeight, int rateClass, String rateCharge) throws RunnerException {
        IataFetchRateRequest iataFetchRateRequest = IataFetchRateRequest.builder()
                .chargeableWeight(chargeableWeight)
                .destinationPort("NAKMP_AIR")
                .originPort("NAGOG_AIR")
                .flightCarrier("Aegean Airlines")
                .currency("INR")
                .build();
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setIataCode("A12");
        unlocationsResponse1.setLocationsReferenceGUID("NAKMP_AIR");
        UnlocationsResponse unlocationsResponse2 = new UnlocationsResponse();
        unlocationsResponse2.setIataCode("B34");
        unlocationsResponse2.setLocationsReferenceGUID("NAGOG_AIR");
        EntityTransferCarrier carrier = EntityTransferCarrier.builder().ItemValue("Aegean Airlines").IATACode("A3").build();

        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        carriersMap.put("Aegean Airlines", carrier);

        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        unlocationsResponseMap.put("NAKMP_AIR", unlocationsResponse1);
        unlocationsResponseMap.put("NAGOG_AIR", unlocationsResponse2);
        Map<String, Object> extraResponseParams = new HashMap<>();
        extraResponseParams.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, 200);

        IataTactRatesApiResponse iataTactRatesApiResponse = IataTactRatesApiResponse.builder()
                .rates(List.of(IataTactRatesApiResponse.Rates.builder()
                                .standardCharge(IataTactRatesApiResponse.StandardCharge.builder()
                                        .minimumCharge(BigDecimal.valueOf(52.0))
                                        .normalCharge(BigDecimal.valueOf(5.0))
                                        .weightBreak(List.of(IataTactRatesApiResponse.WeightBreak.builder()
                                                        .charge(BigDecimal.valueOf(3.0))
                                                        .weightMeasure(BigDecimal.valueOf(100))
                                                .build()))
                                        .build())
                        .build()))
                .build();


        BridgeServiceResponse bridgeServiceResponse = BridgeServiceResponse.builder()
                .payload(iataTactRatesApiResponse)
                .extraResponseParams(extraResponseParams)
                .build();

        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsResponseMap);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        when(bridgeServiceAdapter.requestTactResponse(any())).thenReturn(bridgeServiceResponse);
        when(jsonHelper.convertValue(any(), eq(IataTactRatesApiResponse.class))).thenReturn(iataTactRatesApiResponse);
        ResponseEntity<IRunnerResponse> responseEntity = awbService.getFetchIataRates(CommonRequestModel.buildRequest(iataFetchRateRequest));
        RunnerResponse runnerResponse = (RunnerResponse)responseEntity.getBody();
        IataFetchRateResponse response = (IataFetchRateResponse) runnerResponse.getData();
        assertEquals(rateClass, response.getRateClass());
        assertEquals(rateCharge, response.getRateCharge().toString());
    }

    @Test
    void testGetFetchIataRates_Success_case2() throws RunnerException {
        IataFetchRateRequest iataFetchRateRequest = IataFetchRateRequest.builder()
                .chargeableWeight(BigDecimal.valueOf(156.0))
                .destinationPort("NAKMP_AIR")
                .originPort("NAGOG_AIR")
                .flightCarrier("Aegean Airlines")
                .currency("INR")
                .build();
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setIataCode("A12");
        unlocationsResponse1.setLocationsReferenceGUID("NAKMP_AIR");
        UnlocationsResponse unlocationsResponse2 = new UnlocationsResponse();
        unlocationsResponse2.setIataCode("B34");
        unlocationsResponse2.setLocationsReferenceGUID("NAGOG_AIR");
        EntityTransferCarrier carrier = EntityTransferCarrier.builder().ItemValue("Aegean Airlines").IATACode("A3").build();

        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        carriersMap.put("Aegean Airlines", carrier);

        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        unlocationsResponseMap.put("NAKMP_AIR", unlocationsResponse1);
        unlocationsResponseMap.put("NAGOG_AIR", unlocationsResponse2);
        Map<String, Object> extraResponseParams = new HashMap<>();
        extraResponseParams.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, 200);

        IataTactRatesApiResponse iataTactRatesApiResponse = IataTactRatesApiResponse.builder()
                .rates(List.of(IataTactRatesApiResponse.Rates.builder()
                        .standardCharge(IataTactRatesApiResponse.StandardCharge.builder()
                                .minimumCharge(BigDecimal.valueOf(52.0))
                                .normalCharge(BigDecimal.valueOf(5.0))
                                .weightBreak(List.of(IataTactRatesApiResponse.WeightBreak.builder()
                                        .charge(BigDecimal.valueOf(3.0))
                                        .weightMeasure(BigDecimal.valueOf(100))
                                        .build(),
                                        IataTactRatesApiResponse.WeightBreak.builder()
                                                .charge(BigDecimal.valueOf(4.0))
                                                .weightMeasure(BigDecimal.valueOf(200))
                                                .build()))
                                .build())
                        .build()))
                .build();


        BridgeServiceResponse bridgeServiceResponse = BridgeServiceResponse.builder()
                .payload(iataTactRatesApiResponse)
                .extraResponseParams(extraResponseParams)
                .build();

        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsResponseMap);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        when(bridgeServiceAdapter.requestTactResponse(any())).thenReturn(bridgeServiceResponse);
        when(jsonHelper.convertValue(any(), eq(IataTactRatesApiResponse.class))).thenReturn(iataTactRatesApiResponse);
        ResponseEntity<IRunnerResponse> responseEntity = awbService.getFetchIataRates(CommonRequestModel.buildRequest(iataFetchRateRequest));
        RunnerResponse runnerResponse = (RunnerResponse)responseEntity.getBody();
        IataFetchRateResponse response = (IataFetchRateResponse) runnerResponse.getData();
        assertEquals(RateClass.Q.getId(), response.getRateClass());
        assertEquals("3.0", response.getRateCharge().toString());
    }

    @Test
    void testGetFetchIataRates_Success_case3() throws RunnerException {
        IataFetchRateRequest iataFetchRateRequest = IataFetchRateRequest.builder()
                .chargeableWeight(BigDecimal.valueOf(156.0))
                .destinationPort("NAKMP_AIR")
                .originPort("NAGOG_AIR")
                .flightCarrier("Aegean Airlines")
                .currency("INR")
                .build();
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setIataCode("A12");
        unlocationsResponse1.setLocationsReferenceGUID("NAKMP_AIR");
        UnlocationsResponse unlocationsResponse2 = new UnlocationsResponse();
        unlocationsResponse2.setIataCode("B34");
        unlocationsResponse2.setLocationsReferenceGUID("NAGOG_AIR");
        EntityTransferCarrier carrier = EntityTransferCarrier.builder().ItemValue("Aegean Airlines").IATACode("A3").build();

        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        carriersMap.put("Aegean Airlines", carrier);

        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        unlocationsResponseMap.put("NAKMP_AIR", unlocationsResponse1);
        unlocationsResponseMap.put("NAGOG_AIR", unlocationsResponse2);
        Map<String, Object> extraResponseParams = new HashMap<>();
        extraResponseParams.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, 200);

        IataTactRatesApiResponse iataTactRatesApiResponse = IataTactRatesApiResponse.builder()
                .rates(List.of())
                .build();
        IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder().error("IATA did not return any value - please add the rate/ rate class manually").build();


        BridgeServiceResponse bridgeServiceResponse = BridgeServiceResponse.builder()
                .payload(iataTactRatesApiResponse)
                .extraResponseParams(extraResponseParams)
                .build();

        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsResponseMap);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        when(bridgeServiceAdapter.requestTactResponse(any())).thenReturn(bridgeServiceResponse);
        when(jsonHelper.convertValue(any(), eq(IataTactRatesApiResponse.class))).thenReturn(iataTactRatesApiResponse);
        ResponseEntity<IRunnerResponse> responseEntity = awbService.getFetchIataRates(CommonRequestModel.buildRequest(iataFetchRateRequest));
        assertEquals(ResponseHelper.buildSuccessResponse(iataFetchRateResponse), responseEntity);
    }

    @Test
    void testGetFetchIataRates_Failure_Case1() throws RunnerException {
        IataFetchRateRequest iataFetchRateRequest = IataFetchRateRequest.builder()
                .chargeableWeight(BigDecimal.valueOf(156.0))
                .destinationPort("NAKMP_AIR")
                .originPort("NAGOG_AIR")
                .flightCarrier("Aegean Airlines")
                .currency("INR")
                .build();
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setIataCode("A12");
        unlocationsResponse1.setLocationsReferenceGUID("NAKMP_AIR");
        UnlocationsResponse unlocationsResponse2 = new UnlocationsResponse();
        unlocationsResponse2.setIataCode("B34");
        unlocationsResponse2.setLocationsReferenceGUID("NAGOG_AIR");
        EntityTransferCarrier carrier = EntityTransferCarrier.builder().ItemValue("Aegean Airlines").IATACode("A3C").build();

        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        carriersMap.put("Aegean Airlines", carrier);

        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        unlocationsResponseMap.put("NAKMP_AIR", unlocationsResponse1);
        unlocationsResponseMap.put("NAGOG_AIR", unlocationsResponse2);
        Map<String, Object> extraResponseParams = new HashMap<>();
        extraResponseParams.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, 400);

        IataTactRatesApiResponse iataTactRatesApiResponse = IataTactRatesApiResponse.builder()
                .responseType("validation-failed")
                .errors(List.of(IataTactRatesApiResponse.Errors.builder()
                                .parameterName("Carrier")
                                .errorType("InvalidParameterValue")
                        .message("Value ACC in Carrier is not valid. Please correct and try again")
                        .build()))
                .build();
        IataFetchRateResponse iataFetchRateResponse = IataFetchRateResponse.builder().error("IATA did not return any value - please add the rate/ rate class manually").build();


        BridgeServiceResponse bridgeServiceResponse = BridgeServiceResponse.builder()
                .payload(iataTactRatesApiResponse)
                .extraResponseParams(extraResponseParams)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(iataFetchRateRequest);

        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsResponseMap);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        when(bridgeServiceAdapter.requestTactResponse(any())).thenReturn(bridgeServiceResponse);
        when(jsonHelper.convertValue(any(), eq(IataTactRatesApiResponse.class))).thenReturn(iataTactRatesApiResponse);
        assertThrows(ValidationException.class, () -> awbService.getFetchIataRates(commonRequestModel));
    }

    @Test
    void testGetFetchIataRates_Failure_Case2() throws RunnerException {
        IataFetchRateRequest iataFetchRateRequest = IataFetchRateRequest.builder()
                .chargeableWeight(BigDecimal.valueOf(156.0))
                .destinationPort("NAKMP_AIR")
                .originPort("NAGOG_AIR")
                .flightCarrier("Aegean Airlines")
                .currency("INR")
                .build();
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setIataCode("A12");
        unlocationsResponse1.setLocationsReferenceGUID("NAKMP_AIR");
        UnlocationsResponse unlocationsResponse2 = new UnlocationsResponse();
        unlocationsResponse2.setIataCode("B34");
        unlocationsResponse2.setLocationsReferenceGUID("NAGOG_AIR");
        EntityTransferCarrier carrier = EntityTransferCarrier.builder().ItemValue("Aegean Airlines").IATACode("A3").build();

        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        carriersMap.put("Aegean Airlines", carrier);

        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        unlocationsResponseMap.put("NAKMP_AIR", unlocationsResponse1);
        unlocationsResponseMap.put("NAGOG_AIR", unlocationsResponse2);
        Map<String, Object> extraResponseParams = new HashMap<>();
        extraResponseParams.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, 400);

        IataTactRatesApiResponse iataTactRatesApiResponse = IataTactRatesApiResponse.builder()
                .responseType("validation-failed")
                .errors(List.of(IataTactRatesApiResponse.Errors.builder()
                        .parameterName("Carrier")
                        .errorType("InvalidParameterValue")
                        .message("Value ACC in Carrier is not valid. Please correct and try again")
                        .build()))
                .build();


        BridgeServiceResponse bridgeServiceResponse = BridgeServiceResponse.builder()
                .payload(iataTactRatesApiResponse)
                .extraResponseParams(extraResponseParams)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(iataFetchRateRequest);

        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsResponseMap);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        when(bridgeServiceAdapter.requestTactResponse(any())).thenReturn(bridgeServiceResponse);
        when(jsonHelper.convertValue(any(), eq(IataTactRatesApiResponse.class))).thenReturn(iataTactRatesApiResponse);
        assertThrows(ValidationException.class, () -> awbService.getFetchIataRates(commonRequestModel));
    }

    @Test
    void testGetFetchIataRates_Failure_Case3() throws RunnerException {
        IataFetchRateRequest iataFetchRateRequest = IataFetchRateRequest.builder()
                .chargeableWeight(BigDecimal.valueOf(156.0))
                .destinationPort("NAKMP_AIR")
                .originPort("NAGOG_AIR")
                .flightCarrier("Aegean Airlines")
                .currency("INR")
                .build();
        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setIataCode("A12");
        unlocationsResponse1.setLocationsReferenceGUID("NAKMP_AIR");
        UnlocationsResponse unlocationsResponse2 = new UnlocationsResponse();
        unlocationsResponse2.setIataCode("B34");
        unlocationsResponse2.setLocationsReferenceGUID("NAGOG_AIR");
        EntityTransferCarrier carrier = EntityTransferCarrier.builder().ItemValue("Aegean Airlines").IATACode("A3").build();

        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        carriersMap.put("Aegean Airlines", carrier);

        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        unlocationsResponseMap.put("NAKMP_AIR", unlocationsResponse1);
        unlocationsResponseMap.put("NAGOG_AIR", unlocationsResponse2);
        Map<String, Object> extraResponseParams = new HashMap<>();
        extraResponseParams.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, 500);

        IataTactRatesApiResponse iataTactRatesApiResponse = IataTactRatesApiResponse.builder()
                .build();


        BridgeServiceResponse bridgeServiceResponse = BridgeServiceResponse.builder()
                .payload(iataTactRatesApiResponse)
                .extraResponseParams(extraResponseParams)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(iataFetchRateRequest);

        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsResponseMap);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        when(bridgeServiceAdapter.requestTactResponse(any())).thenReturn(bridgeServiceResponse);
        assertThrows(RunnerException.class, () -> awbService.getFetchIataRates(commonRequestModel));
    }

    @Test
    void testGetFetchIataRates_Failure_Case4() {
        IataFetchRateRequest iataFetchRateRequest = IataFetchRateRequest.builder()
                .chargeableWeight(null)
                .destinationPort(null)
                .originPort(null)
                .flightCarrier(null)
                .currency(null)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(iataFetchRateRequest);

        assertThrows(ValidationException.class, () -> awbService.getFetchIataRates(commonRequestModel));
    }

    @Test
    void testGetFetchIataRates_Failure_Case5() {
        IataFetchRateRequest iataFetchRateRequest = IataFetchRateRequest.builder()
                .chargeableWeight(BigDecimal.valueOf(156.0))
                .destinationPort("NAKMP_AIR")
                .originPort("NAGOG_AIR")
                .flightCarrier("Aegean Airlines")
                .currency("INR")
                .build();

        UnlocationsResponse unlocationsResponse1 = new UnlocationsResponse();
        unlocationsResponse1.setLocationsReferenceGUID("NAKMP_AIR");
        UnlocationsResponse unlocationsResponse2 = new UnlocationsResponse();
        unlocationsResponse2.setLocationsReferenceGUID("NAGOG_AIR");
        EntityTransferCarrier carrier = EntityTransferCarrier.builder().ItemValue("Aegean Airlines").build();

        Map<String, EntityTransferCarrier> carriersMap = new HashMap<>();
        carriersMap.put("Aegean Airlines", carrier);

        Map<String, UnlocationsResponse> unlocationsResponseMap = new HashMap<>();
        unlocationsResponseMap.put("NAKMP_AIR", unlocationsResponse1);
        unlocationsResponseMap.put("NAGOG_AIR", unlocationsResponse2);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(iataFetchRateRequest);

        when(masterDataUtils.getLocationData(any())).thenReturn(unlocationsResponseMap);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(carriersMap);
        assertThrows(ValidationException.class, () -> awbService.getFetchIataRates(commonRequestModel));
    }

    @ParameterizedTest
    @CsvSource({
            "true, 1",
            "true, 2",
            "false, 1",
            "false, 2",
            "true, 0"
    })
    void createAwb_success_Dg(boolean hazardous, int packs) throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ShipmentId(1L).AwbType("DMAWB").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);

        testShipment.setHouseBill("custom-house-bill");
        testShipment.setContainsHazardous(hazardous);
        testShipment.getPackingList().get(0).setHazardous(hazardous);
        testShipment.getPackingList().get(0).setPacks(String.valueOf(packs));  // Set packs value
        testShipment.getAdditionalDetails().setExportBroker(Parties.builder().orgCode("org").addressCode("add").build());
        addShipmentDataForAwbGeneration(testShipment);

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
//        Mockito.when(shipmentService.generateCustomHouseBL(any())).thenReturn("test_hbl_123");
//        Mockito.when(shipmentDao.save(any(), anyBoolean())).thenReturn(testShipment);

//        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));
        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        List<EntityTransferOrganizations> mockOrgList = List.of(EntityTransferOrganizations.builder().build());
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockOrgList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);

        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(List.of(new EntityTransferMasterLists())).build();
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
//        when(jsonHelper.convertValue(any(), eq(EntityTransferMasterLists.class))).thenReturn(null);

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(
                objectMapper.convertValue(testDmawb, AwbResponse.class)
        );

        // mocking for populateCsdInfo
        testShipment.getAdditionalDetails().setScreeningStatus(List.of("EDD", "ETD", "XRY"));
        testShipment.setSecurityStatus("SPX");
        OrgAddressResponse mockOrgAddressResponse = createOrgAddressResponse();

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);
        when(masterDataUtils.shipmentAddressCountryMasterData(any())).thenReturn(Map.of("PE", "Peru", "CA", "Canada"));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.createAwb(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testValidateAwbThrowsExceptionIfNotAllHawbGenerated() {
        Awb mockAwb = testMawb;
        mockAwb.setAirMessageResubmitted(false);
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(1L).build();

        when(consoleShipmentMappingDao.findByConsolidationId(testMawb.getConsolidationId())).thenReturn(
            List.of(consoleShipmentMapping)
        );
        List<String> errors = new ArrayList();

        assertThrows(RunnerException.class, () -> awbService.validateAwb(mockAwb));
    }

    @Test
    void testValidateAwbGivesErrorWhenNewShipmentIsAttached() {
        Awb mockAwb = testMawb;
        List<String> errors = new ArrayList();
        mockAwb.setAirMessageResubmitted(true);
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder().shipmentId(1L).consolidationId(1L).build();

        when(consoleShipmentMappingDao.findByConsolidationId(testMawb.getConsolidationId())).thenReturn(
            List.of(consoleShipmentMapping)
        );
        when(mawbHawbLinkDao.findByMawbId(mockAwb.getId())).thenReturn(Collections.EMPTY_LIST);
        when(awbDao.findByShipmentIdList(Arrays.asList(1L))).thenReturn(List.of(testHawb));

        errors.add("Additional Shipments have been attached, please reset data as required.");

        try{
            var res = awbService.validateAwb(mockAwb);
            assertEquals(errors.toString(), res);
        } catch (Exception e){
            fail(e);
        }
    }

    @Test
    void testPopulateCsdInfo() {
        String expectedCsdInfo = "123/SPX/EDD+ETD+XRY/";

        testShipment.getAdditionalDetails().setExportBroker(Parties.builder().orgCode("org").addressCode("add").build());
        testShipment.getAdditionalDetails().setScreeningStatus(List.of("EDD", "ETD", "XRY"));
        testShipment.setSecurityStatus("SPX");

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(createOrgAddressResponse());

        String csdInfo = awbService.populateCsdInfo(testShipment);
        assertEquals(expectedCsdInfo, csdInfo);
    }

    @Test
    void testPopulateCsdInfo_WithoutScreeningStatus() {
        String expectedCsdInfo = "123/SPX/";

        testShipment.getAdditionalDetails().setExportBroker(Parties.builder().orgCode("org").addressCode("add").build());
        testShipment.setSecurityStatus("SPX");

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(createOrgAddressResponse());

        String csdInfo = awbService.populateCsdInfo(testShipment);
        assertEquals(expectedCsdInfo, csdInfo);
    }

    @Test
    void testPopulateCsdInfo_WithoutSecurityStatus() {
        String expectedCsdInfo = "123/";

        testShipment.getAdditionalDetails().setExportBroker(Parties.builder().orgCode("org").addressCode("add").build());

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(createOrgAddressResponse());

        String csdInfo = awbService.populateCsdInfo(testShipment);
        assertEquals(expectedCsdInfo, csdInfo);
    }

    @Test
    void testPopulateCsdInfo_WithExpiredAgent() {
        testShipment.getAdditionalDetails().setExportBroker(Parties.builder().orgCode("org").addressCode("add").build());

        OrgAddressResponse mockOrgAddressResponse = new OrgAddressResponse();
        mockOrgAddressResponse.setAddresses(Map.ofEntries(
                Map.entry("org#add", Map.ofEntries(
                        Map.entry(REGULATED_AGENT, true),
                        Map.entry(KCRA_NUMBER, 123),
                        Map.entry(KCRA_EXPIRY, LocalDateTime.now().minusDays(1))
                ))
        ));

        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        String csdInfo = awbService.populateCsdInfo(testShipment);
        assertEquals(Strings.EMPTY, csdInfo);
    }

    private OrgAddressResponse createOrgAddressResponse() {
        OrgAddressResponse mockOrgAddressResponse = new OrgAddressResponse();

        mockOrgAddressResponse.setAddresses(Map.ofEntries(
                Map.entry("org#add", Map.ofEntries(
                        Map.entry(REGULATED_AGENT, true),
                        Map.entry(KCRA_NUMBER, 123),
                        Map.entry(KCRA_EXPIRY, LocalDateTime.now().plusDays(1))
                ))
        ));
        return mockOrgAddressResponse;
    }

    @ParameterizedTest
    @ValueSource(strings = {Constants.DMAWB, Constants.HAWB})
    void testValidateAwbGivesErrorIfAirMessageNotResubmitted(String entityType) {
        Awb mockAwb = testHawb;
        mockAwb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SENT);
        mockAwb.getAwbShipmentInfo().setEntityType(entityType);
        mockAwb.setAirMessageResubmitted(false);

        List<String> errors = new ArrayList();
        errors.add(Constants.DMAWB.equalsIgnoreCase(entityType) ? AwbConstants.RESUBMIT_FWB_VALIDATION : AwbConstants.RESUBMIT_FZB_VALIDATION);

        try{
            var res = awbService.validateAwb(mockAwb);
            assertEquals(errors.toString(), res);
        } catch (Exception e){
            fail(e);
        }

    }

    @Test
    void createMawb_successWithTaxRegistrationNumber() throws RunnerException {
        EntityTransferOrganizations entityTransferOrganization1 = new EntityTransferOrganizations();
        entityTransferOrganization1.setOrganizationCode("LL_QR_25300");
        entityTransferOrganization1.setId(1L);
        entityTransferOrganization1.setTaxRegistrationNumber("TRP12343");
        EntityTransferOrganizations entityTransferOrganization2 = new EntityTransferOrganizations();
        entityTransferOrganization2.setOrganizationCode("22481_1");
        entityTransferOrganization2.setId(2L);
        entityTransferOrganization2.setTaxRegistrationNumber("TRP130839");
        EntityTransferOrganizations entityTransferOrganization3 = new EntityTransferOrganizations();
        entityTransferOrganization3.setOrganizationCode("223532_4");
        entityTransferOrganization3.setId(3L);
        entityTransferOrganization3.setTaxRegistrationNumber("TRP130840");
        EntityTransferOrganizations entityTransferOrganization4 = new EntityTransferOrganizations();
        entityTransferOrganization4.setOrganizationCode("1");
        entityTransferOrganization4.setId(4L);
        entityTransferOrganization4.setTaxRegistrationNumber("TRP123678");
        EntityTransferOrganizations entityTransferOrganization5 = new EntityTransferOrganizations();
        entityTransferOrganization5.setOrganizationCode("223532_5");
        entityTransferOrganization5.setId(5L);
        EntityTransferOrganizations entityTransferOrganization6 = new EntityTransferOrganizations();
        entityTransferOrganization6.setOrganizationCode("223532_6");
        entityTransferOrganization6.setId(6L);
        entityTransferOrganization6.setTaxRegistrationNumber("TRP130840");
        List<EntityTransferOrganizations> mockEntityTransferOrganizationList = List.of(entityTransferOrganization1, entityTransferOrganization2, entityTransferOrganization3,entityTransferOrganization4, entityTransferOrganization5, entityTransferOrganization6);

        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ConsolidationId(1L).AwbType(Constants.MAWB).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);
        Long shipmentId = 1L;
        addConsolDataForMawbGeneration(testConsol);
        testShipment.setId(shipmentId);
        testConsol.setShipmentsList(List.of(testShipment));
        testConsol.setSecurityStatus(Constants.SCO);
        testConsol.setInterBranchConsole(true);
        PackSummaryResponse packSummaryResponse = new PackSummaryResponse();
        packSummaryResponse.setVolumeUnit("M3");
        packSummaryResponse.setPacksVolume(new BigDecimal("1000.567"));
        Mockito.when(packingService.calculatePackSummary(any(),any(),any(),any())).thenReturn(packSummaryResponse);

        AwbResponse mockMawbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);

        Mockito.when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(testConsol));
        when(awbDao.findByShipmentIdList(Arrays.asList(shipmentId))).thenReturn(List.of(testHawb));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockEntityTransferOrganizationList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);

        when(awbDao.save(any())).thenReturn(testMawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));


        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(mockMawbResponse);

        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.createMawb(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void createAwb_successWithTaxRegistrationNumber() throws RunnerException {
        EntityTransferOrganizations entityTransferOrganization1 = new EntityTransferOrganizations();
        entityTransferOrganization1.setOrganizationCode("LL_QR_25300");
        entityTransferOrganization1.setId(1L);
        entityTransferOrganization1.setTaxRegistrationNumber("TRP12343");
        EntityTransferOrganizations entityTransferOrganization2 = new EntityTransferOrganizations();
        entityTransferOrganization2.setOrganizationCode("22481_1");
        entityTransferOrganization2.setId(2L);
        entityTransferOrganization2.setTaxRegistrationNumber("TRP130839");
        EntityTransferOrganizations entityTransferOrganization3 = new EntityTransferOrganizations();
        entityTransferOrganization3.setOrganizationCode("223532_4");
        entityTransferOrganization3.setId(3L);
        entityTransferOrganization3.setTaxRegistrationNumber("TRP130840");
        EntityTransferOrganizations entityTransferOrganization4 = new EntityTransferOrganizations();
        entityTransferOrganization4.setOrganizationCode("1");
        entityTransferOrganization4.setId(4L);
        entityTransferOrganization4.setTaxRegistrationNumber("TRP123678");
        EntityTransferOrganizations entityTransferOrganization5 = new EntityTransferOrganizations();
        entityTransferOrganization5.setOrganizationCode("223532_5");
        entityTransferOrganization5.setId(5L);
        EntityTransferOrganizations entityTransferOrganization6 = new EntityTransferOrganizations();
        entityTransferOrganization6.setOrganizationCode("223532_6");
        entityTransferOrganization6.setId(6L);
        entityTransferOrganization6.setTaxRegistrationNumber("TRP130840");
        List<EntityTransferOrganizations> mockEntityTransferOrganizationList = List.of(entityTransferOrganization1, entityTransferOrganization2, entityTransferOrganization3,entityTransferOrganization4, entityTransferOrganization5, entityTransferOrganization6);

        CreateAwbRequest awbRequest = CreateAwbRequest.builder().ShipmentId(1L).AwbType("DMAWB").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(awbRequest);

        testShipment.setHouseBill("custom-house-bill");
        testShipment.setSecurityStatus(Constants.SHR);
        addShipmentDataForAwbGeneration(testShipment);

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
//        Mockito.when(shipmentService.generateCustomHouseBL(any())).thenReturn("test_hbl_123");
//        Mockito.when(shipmentDao.save(any(), anyBoolean())).thenReturn(testShipment);

//        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
        when(awbDao.save(any())).thenReturn(testDmawb);

        // UnLocation response mocking
        when(v1Service.fetchUnlocation(any())).thenReturn(new V1DataResponse());
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferUnLocations.class))).thenReturn(List.of(
                EntityTransferUnLocations.builder().LocationsReferenceGUID("8F39C4F8-158E-4A10-A9B6-4E8FDF52C3BA").Name("Chennai (ex Madras)").build(),
                EntityTransferUnLocations.builder().LocationsReferenceGUID("428A59C1-1B6C-4764-9834-4CC81912DAC0").Name("John F. Kennedy Apt/New York, NY").build()
        ));

        // TenantModel Response mocking
        TenantModel mockTenantModel = new TenantModel();
        mockTenantModel.DefaultOrgId = 1L;
        when(v1Service.retrieveTenant()).thenReturn(V1RetrieveResponse.builder().entity(mockTenantModel).build());
        when(jsonHelper.convertValue(any(), eq(TenantModel.class))).thenReturn(mockTenantModel);

        V1DataResponse mockV1DataResponse = V1DataResponse.builder().entities("").build();
        when(v1Service.fetchOrganization(any())).thenReturn(mockV1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EntityTransferOrganizations.class))).thenReturn(mockEntityTransferOrganizationList);
        when(v1Service.addressList(any())).thenReturn(mockV1DataResponse);

        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
//        when(jsonHelper.convertValue(any(), eq(EntityTransferMasterLists.class))).thenReturn(null);

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(
                objectMapper.convertValue(testDmawb, AwbResponse.class)
        );

        OrgAddressResponse mockOrgAddressResponse = new OrgAddressResponse();
        when(v1ServiceUtil.fetchOrgInfoFromV1(anyList())).thenReturn(mockOrgAddressResponse);

        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> httpResponse = awbService.createAwb(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

}