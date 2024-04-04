package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.AwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.CustomAwbRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.awb.GenerateAwbPaymentInfoRequest;
import com.dpw.runner.shipment.services.dto.response.AwbCalculationResponse;
import com.dpw.runner.shipment.services.dto.response.AwbChargeTypeMasterDataResponse;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.MawbHawbLink;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.AwbReset;
import com.dpw.runner.shipment.services.entity.enums.ChargeTypeCode;
import com.dpw.runner.shipment.services.entitytransfer.dto.ChargeTypeIntegrations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferChargeType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IAwbSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.checkerframework.checker.units.qual.C;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class AwbServiceTest {

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
    @InjectMocks
    private AwbService awbService;

    private static JsonTestUtility jsonTestUtility;
    private static ShipmentDetails testShipment;
    private static ObjectMapper objectMapper;
    private static Awb testHawb;
    private static Awb testDmawb;
    private static Awb testMawb;

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
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build());

        testShipment = jsonTestUtility.getTestShipment();

        testHawb = jsonTestUtility.getTestHawb();
        testDmawb = jsonTestUtility.getTestDmawb();
        testMawb = jsonTestUtility.getTestMawb();
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

        Mockito.when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
//        Mockito.when(shipmentService.generateCustomHouseBL(any())).thenReturn("test_hbl_123");
//        Mockito.when(shipmentDao.save(any(), anyBoolean())).thenReturn(testShipment);

        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
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

        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
//        when(jsonHelper.convertValue(any(), eq(EntityTransferMasterLists.class))).thenReturn(null);

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(
                objectMapper.convertValue(testDmawb, AwbResponse.class)
        );
        ResponseEntity<IRunnerResponse> httpResponse = awbService.createAwb(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
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
    void updateAwb_consolidation() throws RunnerException {
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
    @Disabled
    void list() {
        FetchAwbListRequest listCommonRequest = contructFetchAwbListRequest("id" , 1L, "=");
        Page<Awb> resultPage = new PageImpl<Awb>(List.of(testHawb));
        Mockito.when(awbDao.findAll(any(), any())).thenReturn(resultPage);

        ResponseEntity<IRunnerResponse> listResponse = awbService.list(CommonRequestModel.buildRequest(listCommonRequest));
        assertEquals(HttpStatus.OK, listResponse.getStatusCode());
        assertNotNull(listResponse.getBody());
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
    @Disabled
    void retrieveById_success_consolidation_awb() {
        // retrieve request works with id
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(3L).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(commonGetRequest);
        // Mock
        Mockito.when(awbDao.findById(1L)).thenReturn(Optional.of(testMawb));
        AwbResponse awbResponse = objectMapper.convertValue(testMawb, AwbResponse.class);
        Mockito.when(jsonHelper.convertValue(any(Awb.class), eq(AwbResponse.class))).thenReturn(awbResponse);
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
    void createMawb() {
      }

    @Test
    void updateGoodsAndPacksForMawb() {
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



    @Test
    void reset() throws RunnerException {
        ResetAwbRequest resetAwbRequest = ResetAwbRequest.builder().id(2L).shipmentId(1L).awbType("DMAWB").resetType(AwbReset.ALL).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(resetAwbRequest);

        testShipment.setHouseBill("custom-house-bill");

        when(awbDao.findById(anyLong())).thenReturn(Optional.of(testDmawb));
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
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

        // OtherInfo Master data mocking
        when(jsonHelper.convertValue(any(), eq(LocalDateTime.class))).thenReturn(
                objectMapper.convertValue(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_T_HH_MM_SS).format(LocalDateTime.now()), LocalDateTime.class)
        );
        when(v1Service.fetchMasterData(any())).thenReturn(new V1DataResponse());
//        when(jsonHelper.convertValue(any(), eq(EntityTransferMasterLists.class))).thenReturn(null);

        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(
                objectMapper.convertValue(testDmawb, AwbResponse.class)
        );
        ResponseEntity<IRunnerResponse> httpResponse = awbService.reset(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void partialAutoUpdateAwb() {
      }

    @Test
    void partialAutoUpdateMawb() {
      }

  @Test
  void generateAwbPaymentInfo() throws JsonProcessingException, RunnerException {
        GenerateAwbPaymentInfoRequest request = jsonTestUtility.getJson("AWB_GENERATE_PAYMENT_INFO_PAYLOAD", GenerateAwbPaymentInfoRequest.class);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        Mockito.when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build()));
        AwbCalculationResponse generatePaymentResponse = jsonTestUtility.getJson("AWB_CALCULATION_RESPONSE", AwbCalculationResponse.class);
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
        Page<Awb> resultPage = new PageImpl<Awb>(List.of(testHawb));
        when(awbDao.findAll(any(), any())).thenReturn(resultPage);
        List<AwbResponse> awbResponse = jsonTestUtility.convertValueToList(List.of(testHawb), AwbResponse.class);
        when(jsonHelper.convertValueToList(any(), eq(AwbResponse.class))).thenReturn(awbResponse);

        ResponseEntity<IRunnerResponse> response = awbService.retrieveByAwbByMawb(commonRequestModel);
        assertEquals(response.getStatusCode(), HttpStatus.OK);
    }

    @Test
    void generateUpdatedNatureAndQuantGoodsField() throws RunnerException, JsonProcessingException {
        GenerateAwbPaymentInfoRequest request =
            objectMapper.readValue(
                "{\"awbShipmentInfo\":{\"entityId\":152616,\"entityType\":\"DMAWB\",\"shipperName\":\"TEST VG CREDIT LIMIT\",\"shipperAddress\":\"\\r\\nADDRESS 1\\r\\nMH\\r\\nPUNE\\r\\nIND\",\"consigneeName\":\"NATIONWIDE PAPER LTD\",\"consigneeAddress\":\"\\r\\nABC CO LLC\\nAEJEA\\r\\nARE\",\"consigneeReferenceNumber\":\"2436215\",\"iataCode\":\"5678\",\"awbNumber\":\"235-88998641\",\"originAirport\":\"Kempegowda International Airport BLR\",\"destinationAirport\":\"George Bush Intercontinental Airport IAH, TX\",\"firstCarrier\":\"Turkish Airlines\",\"agentCASSCode\":\"8975\"},\"awbNotifyPartyInfo\":[{}],\"awbRoutingInfo\":[{\"entityId\":152616,\"entityType\":\"DMAWB\",\"byCarrier\":\"Turkish Airlines\",\"flightNumber\":\"6457\",\"flightDate\":\"2024-02-02T01:00:00\",\"destinationPortName\":\"USIAH_AIR\",\"originPortName\":\"INBLR_AIR\",\"isShipmentCreated\":true}],\"awbCargoInfo\":{\"entityId\":152616,\"entityType\":\"DMAWB\",\"ntrQtyGoods\":\"\",\"currency\":\"EGP\",\"customsValue\":0},\"awbOtherInfo\":{\"entityId\":152616,\"entityType\":\"DMAWB\",\"shipper\":\"TEST VG CREDIT LIMIT\",\"carrier\":\"carrier for mawb from master\",\"executedOn\":\"2024-03-07T12:56:16\"},\"awbGoodsDescriptionInfo\":[{\"entityId\":152616,\"entityType\":\"DMAWB\",\"piecesNo\":6,\"guid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\",\"awbPackingInfo\":[{\"guid\":\"38bc225a-fa41-40a5-afae-71a11a524dfa\",\"packs\":\"5\",\"packsType\":\"BAG\",\"weight\":25,\"weightUnit\":\"KG\",\"volume\":1.2,\"volumeUnit\":\"M3\",\"commodity\":\"530521\",\"length\":400,\"lengthUnit\":\"CM\",\"width\":300,\"widthUnit\":\"CM\",\"height\":2,\"heightUnit\":\"CM\",\"hsCode\":\"530521\",\"volumeWeight\":200.0004,\"volumeWeightUnit\":\"KG\",\"awbNumber\":\"235-88998641\",\"awbGoodsDescriptionInfoGuid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\"},{\"guid\":\"4fbd511c-26cc-48ae-a2ae-5fe5d9baf803\",\"packs\":\"1\",\"packsType\":\"BAG\",\"weight\":100,\"weightUnit\":\"KG\",\"volume\":0.2,\"volumeUnit\":\"M3\",\"commodity\":\"530529\",\"length\":100,\"lengthUnit\":\"CM\",\"width\":200,\"widthUnit\":\"CM\",\"height\":10,\"heightUnit\":\"CM\",\"minTempUnit\":\"CEL\",\"hsCode\":\"530529\",\"goodsDescription\":\"DESC\",\"netWeightUnit\":\"KG\",\"volumeWeight\":33.3334,\"volumeWeightUnit\":\"KG\",\"awbNumber\":\"235-88998641\",\"awbGoodsDescriptionInfoGuid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\"}],\"isShipmentCreated\":true}],\"prepaid\":{\"weightCharges\":\"\",\"valuationCharge\":\"\",\"tax\":\"\",\"dueAgentCharges\":\"\",\"dueCarrierCharges\":\"\"},\"collect\":{\"weightCharges\":\"\",\"valuationCharge\":\"\",\"tax\":\"\",\"dueAgentCharges\":\"\",\"dueCarrierCharges\":\"\"},\"awbOciInfo\":[],\"awbPackingInfo\":[{\"guid\":\"38bc225a-fa41-40a5-afae-71a11a524dfa\",\"packs\":\"5\",\"packsType\":\"BAG\",\"weight\":25,\"weightUnit\":\"KG\",\"volume\":1.2,\"volumeUnit\":\"M3\",\"commodity\":\"530521\",\"length\":400,\"lengthUnit\":\"CM\",\"width\":300,\"widthUnit\":\"CM\",\"height\":2,\"heightUnit\":\"CM\",\"hsCode\":\"530521\",\"volumeWeight\":200.0004,\"volumeWeightUnit\":\"KG\",\"awbNumber\":\"235-88998641\",\"awbGoodsDescriptionInfoGuid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\"},{\"guid\":\"4fbd511c-26cc-48ae-a2ae-5fe5d9baf803\",\"packs\":\"1\",\"packsType\":\"BAG\",\"weight\":100,\"weightUnit\":\"KG\",\"volume\":0.2,\"volumeUnit\":\"M3\",\"commodity\":\"530529\",\"length\":100,\"lengthUnit\":\"CM\",\"width\":200,\"widthUnit\":\"CM\",\"height\":10,\"heightUnit\":\"CM\",\"minTempUnit\":\"CEL\",\"hsCode\":\"530529\",\"goodsDescription\":\"DESC\",\"netWeightUnit\":\"KG\",\"volumeWeight\":33.3334,\"volumeWeightUnit\":\"KG\",\"awbNumber\":\"235-88998641\",\"awbGoodsDescriptionInfoGuid\":\"d305bff6-72f9-4435-acf2-c10f9b729310\"}],\"awbOtherChargesInfo\":[],\"shcIdList\":[],\"chargeDetails\":{\"Id\":\"\",\"ItemType\":\"\",\"ItemValue\":\"\",\"ItemDescription\":\"\",\"ValuenDesc\":\"\",\"Identifier1\":\"\",\"Identifier2\":\"\",\"Identifier3\":\"\",\"Identifier4\":\"\",\"TenantId\":\"\",\"IsActive\":\"\",\"AirLinePrefixValue\":\"\",\"DPAManifestMaster\":\"\",\"label\":\"\",\"value\":\"\"},\"awbPaymentInfo\":{\"weightCharges\":\"\",\"valuationCharge\":\"\",\"tax\":\"\",\"dueAgentCharges\":\"\",\"totalPrepaid\":\"\",\"totalCollect\":\"\"},\"id\":68,\"guid\":\"4545424c-fe16-48c4-b1e9-230589324dba\",\"awbNumber\":\"235-88998641\",\"shipmentId\":152616}",
                GenerateAwbPaymentInfoRequest.class);

        Mockito.when(shipmentSettingsDao.getSettingsByTenantIds(any())).thenReturn(List.of(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build()));

        ResponseEntity<IRunnerResponse> response = awbService.generateUpdatedNatureAndQuantGoodsField(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, response.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(response.getBody(), RunnerResponse.class);
        assertEquals(
        "DIMS: In CMS\r\n" + "5=400X300X2,1=100X200X10\r\n" + "Total Volumetric Weight 233.33 KGS",
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
}