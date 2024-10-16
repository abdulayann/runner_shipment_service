package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.commons.constants.ConsolidationConstants;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.ITrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.patchRequest.ConsolidationPatchRequest;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.awb.AwbCargoInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.dto.request.notification.PendingNotificationRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.notification.PendingConsolidationActionResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.dto.v1.response.GuidsListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.AwbStatus;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.mapper.CarrierDetailsMapper;
import com.dpw.runner.shipment.services.mapper.ConsolidationDetailsMapper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.response.CarrierResponse;
import com.dpw.runner.shipment.services.projection.ConsolidationDetailsProjection;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.service_bus.AzureServiceBusTopic;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
  class ConsolidationServiceTest extends CommonMocks {

    @Mock
    private PartialFetchUtils partialFetchUtils;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private IPartiesDao partiesDao;

    @Mock
    private CSVParsingUtil<ConsolidationDetails> parser;

    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private IPackingDao packingDao;

    @Mock
    private IEventDao eventDao;

    @Mock
    private INotesDao notesDao;

    @Mock
    private IReferenceNumbersDao referenceNumbersDao;

    @Mock
    private ITruckDriverDetailsDao truckDriverDetailsDao;

    @Mock
    private IRoutingsDao routingsDao;
    @Mock
    private IAwbDao awbDao;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private IContainerService containerService;

    @Mock
    private IPackingService packingService;

    @Mock
    private UserContext userContext;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IShipmentSync shipmentSync;

    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    private ISBUtils sbUtils;

    @Mock
    private ISBProperties isbProperties;

    @Mock
    private AzureServiceBusTopic azureServiceBusTopic;

    @Mock
    private IConsolidationSync consolidationSync;

    @Mock
    private BillingServiceAdapter billingServiceAdapter;

    @Mock
    private IAuditLogService auditLogService;

    @Mock
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    private IV1Service v1Service;

    @Mock
    private MasterDataUtils masterDataUtils;

    @Mock
    CacheManager cacheManager;

    @Mock
    CustomKeyGenerator keyGenerator;

    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;

    @Mock
    private ConsolidationDetailsMapper consolidationDetailsMapper;

    @Mock
    private CarrierDetailsMapper carrierDetailsMapper;

    @Mock
    private ProductIdentifierUtility productEngine;

    @Mock
    private ITrackingServiceAdapter trackingServiceAdapter;

    @Mock
    private KafkaProducer producer;

    @Mock
    private GetNextNumberHelper getNextNumberHelper;

    @Mock
    private IPackingsSync packingsADSync;
    @Mock
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Mock
    private V1ServiceUtil v1ServiceUtil;

    @InjectMocks
    private ConsolidationService consolidationService;

    private static JsonTestUtility jsonTestUtility;
    private static ShipmentDetails testShipment;
    private static ObjectMapper objectMapperTest;
    private static ConsolidationDetails testConsol;
    private static ShipmentDetails shipmentDetails;

    private static ConsolidationDetailsResponse testConsolResponse;
    private static ConsolidationDetailsRequest testConsolRequest;

    private static ModelMapper modelMapperTest = new ModelMapper();
    private ConsolidationDetails consolidationDetails;
    private List<ModuleValidationFieldType> missingFields;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapperTest = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().build());
        testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        testConsolResponse = modelMapperTest.map(testConsol , ConsolidationDetailsResponse.class);
        testConsolRequest = modelMapperTest.map(testConsol , ConsolidationDetailsRequest.class);
        consolidationService.executorService = Executors.newFixedThreadPool(2);
        shipmentDetails = jsonTestUtility.getCompleteShipment();
        consolidationDetails = new ConsolidationDetails();
        missingFields = new ArrayList<>();
    }

    @AfterEach
    void tearDown() {
        consolidationService.executorService.shutdown();
    }
//    @AfterEach
//    void tearDown() {
//        reset(CommonUtils.class);
//    }

    @Test
    public void testRetrieveByIdAsync_ValidId_Success() {

        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));

        mockShipmentSettings();

        CompletableFuture<ResponseEntity<IRunnerResponse>> future = consolidationService.retrieveByIdAsync(requestModel);


        assertNotNull(future);
        assertTrue(future.join().getBody() instanceof IRunnerResponse);
        assertEquals(200, future.join().getStatusCodeValue());
    }

    @Test
    void testMblCheck_MblNumberFoundInDifferentTenant() {
        String mblNumber = "MBL123";
        ConsolidationDetailsProjection projection1 = new ConsolidationDetailsProjection() {
            @Override
            public Integer getTenantId() { return 2; }
            @Override
            public String getConsolidationNumber() { return "CON123"; }
            @Override
            public String getMawb() { return "MAWB123"; }
            @Override
            public String getBol() { return "BOL123"; }
        };

        ConsolidationDetailsProjection projection2 = new ConsolidationDetailsProjection() {
            @Override
            public Integer getTenantId() { return 3; }
            @Override
            public String getConsolidationNumber() { return "CON456"; }
            @Override
            public String getMawb() { return "MAWB456"; }
            @Override
            public String getBol() { return "BOL456"; }
        };

        List<ConsolidationDetailsProjection> projections = Arrays.asList(projection1, projection2);

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(mblNumber)).thenReturn(projections);

        ResponseEntity<IRunnerResponse> response = consolidationService.mblCheck(mblNumber);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof MblCheckResponse);

        MblCheckResponse mblCheckResponse = (MblCheckResponse) data;
        assertNotNull(mblCheckResponse.getMessage());

        verify(consolidationDetailsDao).findMblNumberInDifferentTenant(mblNumber);
    }

    @Test
    void testMblCheck_MblNumberFoundInDifferentTenant_getTenantName() {
        String mblNumber = "MBL123";
        ConsolidationDetailsProjection projection1 = new ConsolidationDetailsProjection() {
            @Override
            public Integer getTenantId() { return 2; }
            @Override
            public String getConsolidationNumber() { return "CON123"; }
            @Override
            public String getMawb() { return "MAWB123"; }
            @Override
            public String getBol() { return "BOL123"; }
        };

        ConsolidationDetailsProjection projection2 = new ConsolidationDetailsProjection() {
            @Override
            public Integer getTenantId() { return 3; }
            @Override
            public String getConsolidationNumber() { return "CON456"; }
            @Override
            public String getMawb() { return "MAWB456"; }
            @Override
            public String getBol() { return "BOL456"; }
        };

        List<ConsolidationDetailsProjection> projections = Arrays.asList(projection1, projection2);

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(mblNumber)).thenReturn(projections);
        when(v1Service.getTenantName(anyList())).thenReturn(List.of("TestTenant"));

        ResponseEntity<IRunnerResponse> response = consolidationService.mblCheck(mblNumber);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof MblCheckResponse);

        MblCheckResponse mblCheckResponse = (MblCheckResponse) data;
        assertNotNull(mblCheckResponse.getMessage());

        verify(consolidationDetailsDao).findMblNumberInDifferentTenant(mblNumber);
    }

    @Test
    void testMblCheck_MblNumberFoundInDifferentTenant_getTenantName_exception() {
        String mblNumber = "MBL123";
        ConsolidationDetailsProjection projection1 = new ConsolidationDetailsProjection() {
            @Override
            public Integer getTenantId() { return 2; }
            @Override
            public String getConsolidationNumber() { return "CON123"; }
            @Override
            public String getMawb() { return "MAWB123"; }
            @Override
            public String getBol() { return "BOL123"; }
        };

        ConsolidationDetailsProjection projection2 = new ConsolidationDetailsProjection() {
            @Override
            public Integer getTenantId() { return 3; }
            @Override
            public String getConsolidationNumber() { return "CON456"; }
            @Override
            public String getMawb() { return "MAWB456"; }
            @Override
            public String getBol() { return "BOL456"; }
        };

        List<ConsolidationDetailsProjection> projections = Arrays.asList(projection1, projection2);

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(mblNumber)).thenReturn(projections);
        when(v1Service.getTenantName(anyList())).thenThrow(new RuntimeException());

        ResponseEntity<IRunnerResponse> response = consolidationService.mblCheck(mblNumber);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof MblCheckResponse);

        MblCheckResponse mblCheckResponse = (MblCheckResponse) data;
        assertNotNull(mblCheckResponse.getMessage());

        verify(consolidationDetailsDao).findMblNumberInDifferentTenant(mblNumber);
    }

    @Test
    void testMblCheck_MblNumberNotFoundInDifferentTenant() {
        String mblNumber = "MBL123";
        List<ConsolidationDetailsProjection> projections = Collections.emptyList();

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(mblNumber)).thenReturn(projections);

        ResponseEntity<IRunnerResponse> response = consolidationService.mblCheck(mblNumber);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof MblCheckResponse);

        MblCheckResponse mblCheckResponse = (MblCheckResponse) data;
        assertNull(mblCheckResponse.getMessage());

        verify(consolidationDetailsDao).findMblNumberInDifferentTenant(mblNumber);
    }

    @Test
    void testMblCheck_MblNumberIsNull() {
        String mblNumber = null;
        List<ConsolidationDetailsProjection> projections = Collections.emptyList();

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(mblNumber)).thenReturn(projections);

        ResponseEntity<IRunnerResponse> response = consolidationService.mblCheck(mblNumber);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof MblCheckResponse);

        MblCheckResponse mblCheckResponse = (MblCheckResponse) data;
        assertNull(mblCheckResponse.getMessage());

        verify(consolidationDetailsDao).findMblNumberInDifferentTenant(mblNumber);
    }

    @Test
    void testMblCheck_MblNumberFoundInDifferentTenantWithSingleEntry() {
        String mblNumber = "MBL123";
        ConsolidationDetailsProjection projection = new ConsolidationDetailsProjection() {
            @Override
            public Integer getTenantId() { return 2; }
            @Override
            public String getConsolidationNumber() { return "CON123"; }
            @Override
            public String getMawb() { return "MAWB123"; }
            @Override
            public String getBol() { return "BOL123"; }
        };

        List<ConsolidationDetailsProjection> projections = Collections.singletonList(projection);

        when(consolidationDetailsDao.findMblNumberInDifferentTenant(mblNumber)).thenReturn(projections);

        ResponseEntity<IRunnerResponse> response = consolidationService.mblCheck(mblNumber);

        assertNotNull(response);
        assertNotNull(response.getBody());
        assertTrue(response.getBody() instanceof RunnerResponse);

        RunnerResponse runnerResponse = (RunnerResponse) response.getBody();

        IRunnerResponse data = (IRunnerResponse) runnerResponse.getData();
        assertTrue(data instanceof MblCheckResponse);

        MblCheckResponse mblCheckResponse = (MblCheckResponse) data;
        assertNotNull(mblCheckResponse.getMessage());

        verify(consolidationDetailsDao).findMblNumberInDifferentTenant(mblNumber);
    }


    @Test
    void testValidateCarrierDetails_EmptyCarrierDetails() {
        consolidationDetails.setCarrierDetails(null);

        consolidationService.validateCarrierDetails(consolidationDetails, missingFields);

        assertTrue(missingFields.contains(ModuleValidationFieldType.CARRIER));
    }

    @Test
    void testValidateCarrierDetails_EmptyCarrierETA() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEta(null);
        consolidationDetails.setCarrierDetails(carrierDetails);

        consolidationService.validateCarrierDetails(consolidationDetails, missingFields);

        assertTrue(missingFields.contains(ModuleValidationFieldType.CARRIER_ETA));
    }

    @Test
    void testValidateCarrierDetails_EmptyCarrierETD() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEta(LocalDateTime.now());
        carrierDetails.setEtd(null);
        consolidationDetails.setCarrierDetails(carrierDetails);

        consolidationService.validateCarrierDetails(consolidationDetails, missingFields);

        assertTrue(missingFields.contains(ModuleValidationFieldType.CARRIER_ETD));
    }

    @Test
    void testValidateCarrierDetails_ValidCarrierDetailsAndETA() {
        CarrierDetails carrierDetails = new CarrierDetails();
        carrierDetails.setEta(LocalDateTime.now());
        carrierDetails.setShippingLine("ShippingLine");
        consolidationDetails.setCarrierDetails(carrierDetails);

        consolidationService.validateCarrierDetails(consolidationDetails, missingFields);

        assertFalse(missingFields.contains(ModuleValidationFieldType.CARRIER));
        assertFalse(missingFields.contains(ModuleValidationFieldType.CARRIER_ETA));
    }

    @Test
    void testValidateContainerDetails_EmptyContainersList() {
        consolidationDetails.setBol(null);

        consolidationService.validateMawbDetails(consolidationDetails, missingFields);

        assertTrue(missingFields.contains(ModuleValidationFieldType.MAWB_DETAILS));
    }

    @Test
    public void testRetrieveByIdAsync_NullId_Failure() {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        requestModel.setData(getRequest);

        CompletableFuture<ResponseEntity<IRunnerResponse>> future = consolidationService.retrieveByIdAsync(requestModel);


        assertNotNull(future);
        assertTrue(future.join().hasBody());
        assertEquals(400, future.join().getStatusCodeValue());
    }

    @Test
    public void testRetrieveByIdAsync_DetailsNotFound_Failure() {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());


        CompletableFuture<ResponseEntity<IRunnerResponse>> future = consolidationService.retrieveByIdAsync(requestModel);


        assertNotNull(future);
//        assertTrue(future.join().getBody() instanceof String);
        assertEquals(400, future.join().getStatusCodeValue());
    }

    @Test
    public void testRetrieveByIdAsync_ExceptionThrown_Failure() {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        when(consolidationDetailsDao.findById(1L)).thenThrow(new RuntimeException("Test Exception"));


        CompletableFuture<ResponseEntity<IRunnerResponse>> future = consolidationService.retrieveByIdAsync(requestModel);


        assertNotNull(future);
        assertTrue(future.join().hasBody());
        assertEquals(400, future.join().getStatusCodeValue());
    }

    @Test
    void testFetchConsolidation_Success_with_data(){
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        ConsolidationListResponse response = modelMapperTest.map(testConsol, ConsolidationListResponse.class);

        when(consolidationDetailsDao.findAll(any() , any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(modelMapper.map(any() , any())).thenReturn(response);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        mockShipmentSettings();

        consolidationService.fetchConsolidations(CommonRequestModel.builder().data(sampleRequest).build());

        verify(consolidationDetailsDao, times(1)).findAll(any(), any());
    }

    @Test
    public void testCompleteRetrieveById_Success_NoColumnsIncluded() throws ExecutionException, InterruptedException {
        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        getRequest.setId(1L);
        requestModel.setData(getRequest);
        var spyService = Mockito.spy(consolidationService);
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(testConsolResponse));
//        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
        Mockito.doReturn(future).when(spyService).retrieveByIdAsync(requestModel);

        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeRetrieveById(requestModel);

        assertNotNull(responseEntity);
        assertEquals(200, responseEntity.getStatusCodeValue());
    }

    @Test
    @Disabled
    public void testCompleteRetrieveById_Success_WithColumnsIncluded() throws ExecutionException, InterruptedException {

        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        getRequest.setId(1L);
        getRequest.setIncludeColumns(List.of("guid", "id"));
        requestModel.setData(getRequest);
        var spyService = Mockito.spy(consolidationService);
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(testConsolResponse));
        Mockito.doReturn(future).when(spyService).retrieveByIdAsync(requestModel);
        when(partialFetchUtils.fetchPartialListData(any(), any())).thenReturn(testConsolResponse);
        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeRetrieveById(requestModel);

        assertNotNull(responseEntity);
        assertEquals(200, responseEntity.getStatusCodeValue());
    }

    @Test
    public void testCompleteRetrieveById_RequestIsNull() throws ExecutionException, InterruptedException {

        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.completeRetrieveById(requestModel);


        assertNotNull(responseEntity);
        assertEquals(400, responseEntity.getStatusCodeValue());
    }

    @Test
    public void testCompleteRetrieveById_RequestIdIsNull() throws ExecutionException, InterruptedException {

        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        requestModel.setData(getRequest);


        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.completeRetrieveById(requestModel);


        assertNotNull(responseEntity);
        assertEquals(400, responseEntity.getStatusCodeValue());
    }

    @Test
    public void testCompleteRetrieveById_ExceptionThrown() throws ExecutionException, InterruptedException {

        CommonRequestModel requestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        getRequest.setId(1L);
        requestModel.setData(getRequest);
        var spyService = Mockito.spy(consolidationService);
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = CompletableFuture.failedFuture(new RuntimeException());
        Mockito.doReturn(future).when(spyService).retrieveByIdAsync(requestModel);


        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeRetrieveById(requestModel);


        assertNotNull(responseEntity);
        assertEquals(400, responseEntity.getStatusCodeValue());
    }

    @Test
    void testCompleteUpdate_Failure() throws Exception {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = testConsolRequest;
        commonRequestModel.setData(copy);

        var spyService = Mockito.spy(consolidationService);
        Mockito.doReturn(Optional.empty()).when(spyService).retrieveByIdOrGuid(any());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.completeUpdate(commonRequestModel));
        verify(auditLogService, times(0)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    public void testGetGuidFromId_Success() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        commonRequestModel.setData(getRequest);

        ConsolidationDetails consolidationDetails = testConsol;
        var guid = UUID.randomUUID();
        consolidationDetails.setGuid(guid);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> response = consolidationService.getGuidFromId(commonRequestModel);

        assertEquals(200, response.getStatusCodeValue());
    }

    @Test
    public void testGetGuidFromId_RequestIsNull() {
        ResponseEntity<IRunnerResponse> response = consolidationService.getGuidFromId(null);
        assertEquals(400, response.getStatusCodeValue());
    }

    @Test
    public void testGetGuidFromId_ConsolidationDetailsNotFound() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        commonRequestModel.setData(getRequest);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> response = consolidationService.getGuidFromId(commonRequestModel);

        assertEquals(400, response.getStatusCodeValue());
    }

    @Test
    public void testShowCreateBooking_CreatePermissionDenied() {
        PermissionsContext.setPermissions(Collections.emptyList());
        assertThrows(RunnerException.class, () -> consolidationService.showCreateBooking("CREATE"));
    }

    @Test
    public void testShowCreateBooking_ViewPermissionDenied() {
        PermissionsContext.setPermissions(Collections.emptyList());
        assertThrows(RunnerException.class, () -> consolidationService.showCreateBooking("VIEW"));
    }

    @Test
    public void testUpdateConsoleBookingFields_Success() {
        ConsoleBookingRequest request = new ConsoleBookingRequest();
        request.setGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        when(consolidationDetailsDao.updateConsoleBookingFields(request)).thenReturn(1);
        ResponseEntity<IRunnerResponse> response = consolidationService.updateConsoleBookingFields(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    public void testUpdateConsoleBookingFields_Failure() {
        ConsoleBookingRequest request = new ConsoleBookingRequest();
        request.setGuid(UUID.randomUUID());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        when(consolidationDetailsDao.updateConsoleBookingFields(request)).thenReturn(0);
        assertThrows(com.dpw.runner.shipment.services.exception.exceptions.ValidationException.class, () -> consolidationService.updateConsoleBookingFields(commonRequestModel));
    }

    @Test
    public void testCreateFromBooking_Success() {
        // Setup
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;
        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        when(jsonHelper.convertValue(request, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean(), eq(false))).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> response = consolidationService.createFromBooking(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testCreateFromBooking_AuditLogException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Setup
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;
        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        when(jsonHelper.convertValue(request, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        doThrow(new IllegalAccessException("IllegalAccessException")).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        // mockTenantSettings();
        assertThrows(ValidationException.class, () -> consolidationService.createFromBooking(commonRequestModel));
    }

    @Test
    void testCreateFromBooking_RequestIsNull() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(null).build();
        assertThrows(ValidationException.class, () -> consolidationService.createFromBooking(commonRequestModel));
    }


    @Test
    public void testCreateFromBooking_ThrowsValidationException() {
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        ConsolidationDetails consolidationDetails = testConsol;

        when(jsonHelper.convertValue(request, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenThrow(new ValidationException("TEST"));
        mockShipmentSettings();
        mockTenantSettings();
        assertThrows(ValidationException.class, () -> consolidationService.createFromBooking(commonRequestModel));
    }

    @Test
    public void testValidateMawbNumber_Success() {
        String mawb = "ABC123";
        String type = "type";
        ValidateMawbNumberRequest request = ValidateMawbNumberRequest.builder().mawb(mawb).type(type).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        ValidateMawbNumberResponse expectedResponse = ValidateMawbNumberResponse.builder().success(true).build();
        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);


        CarrierResponse carrierResponse = CarrierResponse.builder().build();
        when(consolidationDetailsDao.isMAWBNumberValid(mawb)).thenReturn(true);
        when(v1Service.fetchCarrierMasterData(any(CarrierListObject.class), anyBoolean())).thenReturn(V1DataResponse.builder().entities(carrierResponse).build());
        when(jsonHelper.convertValueToList(any(), eq(CarrierResponse.class))).thenReturn(List.of(carrierResponse));

        ResponseEntity<IRunnerResponse> response = consolidationService.validateMawbNumber(commonRequestModel);

        assertEquals(200, response.getStatusCodeValue());
    }

    @Test
    public void testValidateMawbNumber_InvalidMawb() {
        String mawb = "INVALID_MAWB";
        ValidateMawbNumberRequest request = ValidateMawbNumberRequest.builder().mawb(mawb).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(request).build();
        when(consolidationDetailsDao.isMAWBNumberValid(any())).thenReturn(false);
        assertThrows(ValidationException.class, () -> consolidationService.validateMawbNumber(commonRequestModel));
    }

    @Test
    public void testGenerateCustomHouseBLNumber_Success() throws RunnerException {
        String generatedNumber = "ABC123";
        GenerateCustomHblResponse expectedResponse = GenerateCustomHblResponse.builder().hblNumber(generatedNumber).build();
        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        ConsolidationService spyService = spy(consolidationService);
        doReturn(generatedNumber).when(spyService).generateCustomBolNumber();

        ResponseEntity<IRunnerResponse> response = spyService.generateCustomHouseBLNumber();

        assertEquals(expectedEntity, response);
    }

    @Test
    public void testGenerateCustomHouseBLNumber_Exception(){
        ConsolidationService spyService = spy(consolidationService);
        doThrow(new RuntimeException("Test exception")).when(spyService).generateCustomBolNumber();
        assertThrows(RunnerException.class, ()->spyService.generateCustomHouseBLNumber());
    }

    @Test
//    @Disabled
    public void testGetDefaultConsolidation_Success() {
        ShipmentSettingsDetails tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setDefaultTransportMode("Sea");
        tenantSettings.setDefaultContainerType("ContainerType");
        tenantSettings.setDefaultShipmentType("ShipmentType");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        TenantModel tenantModel = new TenantModel();

        when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);

        UserContext.setUser(UsersDto.builder().Username("Username").TenantId(1).build());
        LocalDateTime currentTime = LocalDateTime.now();
        mockShipmentSettings();

        ResponseEntity<IRunnerResponse> response = consolidationService.getDefaultConsolidation();

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());

        ConsolidationDetailsResponse responseBody = (ConsolidationDetailsResponse)((RunnerResponse) response.getBody()).getData();
        assertNotNull(responseBody);
        assertEquals("ContainerType", responseBody.getContainerCategory());
        assertEquals("ShipmentType", responseBody.getShipmentType());
        assertEquals("Username", responseBody.getCreatedBy());
        assertEquals(currentTime.getDayOfYear(), responseBody.getCreatedAt().getDayOfYear());
        assertEquals(1, responseBody.getSourceTenantId());
    }

    @Test
    void testCreate_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean(), eq(false))).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), eq(true))).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(truckDriverDetailsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(List.of());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> response = spyService.create(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testCompleteUpdate_Success_LCLConsolidation() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);
        ShipmentDetails shipmentDetails2 = new ShipmentDetails();
        ShipmentRequest shipmentDetails1 = new ShipmentRequest();
        shipmentDetails1.setShipmentGateInDate(LocalDateTime.now());
        shipmentDetails2.setShipmentGateInDate(LocalDateTime.now());

        ConsolidationDetails consolidationDetails = testConsol;
        copy.setCfsCutOffDate(LocalDateTime.MIN);
        copy.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        copy.setShipmentType(Constants.DIRECTION_EXP);
        List<ShipmentRequest> shipmentRequests = new ArrayList<>();
        shipmentRequests.add(shipmentDetails1);
        copy.setShipmentsList(shipmentRequests);
        copy.setContainersList(null);

        consolidationDetails.setCfsCutOffDate(LocalDateTime.MIN);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
        List<ShipmentDetails> shipmentDetailsRequests = new ArrayList<>();
        shipmentDetailsRequests.add(shipmentDetails2);
        consolidationDetails.setShipmentsList(shipmentDetailsRequests);
        consolidationDetails.setContainersList(null);

        var spyService = Mockito.spy(consolidationService);

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        mockShipmentSettings();
        mockTenantSettings();
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnableLclConsolidation(true);
        assertThrows(RuntimeException.class, () -> spyService.completeUpdate(commonRequestModel));
    }

    @Test
    void testCompleteUpdate_Success_LCLConsolidation_cutoffvalidation() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);
        ShipmentDetails shipmentDetails2 = new ShipmentDetails();
        ShipmentRequest shipmentDetails1 = new ShipmentRequest();
        shipmentDetails1.setShipmentGateInDate(LocalDateTime.now());
        shipmentDetails2.setShipmentGateInDate(LocalDateTime.now());

        ConsolidationDetails consolidationDetails = testConsol;
        copy.setCfsCutOffDate(LocalDateTime.MIN);
        copy.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        copy.setShipmentType(Constants.DIRECTION_EXP);
        List<ShipmentRequest> shipmentRequests = new ArrayList<>();
        shipmentRequests.add(shipmentDetails1);
        copy.setShipmentsList(shipmentRequests);
        copy.setContainersList(null);

        consolidationDetails.setCfsCutOffDate(LocalDateTime.MIN);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
        List<ShipmentDetails> shipmentDetailsRequests = new ArrayList<>();
        shipmentDetailsRequests.add(shipmentDetails2);
        consolidationDetails.setShipmentsList(shipmentDetailsRequests);
        consolidationDetails.setContainersList(null);

        var spyService = Mockito.spy(consolidationService);

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails2)));
        mockShipmentSettings();
        mockTenantSettings();
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnableLclConsolidation(true);
        assertThrows(RuntimeException.class, () -> spyService.completeUpdate(commonRequestModel));
    }

    @Test
    void testCreate_SyncFailure() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean(), eq(false))).thenReturn(consolidationDetails);
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenThrow(new RunnerException("Test"));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> response = spyService.create(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testCreate_AuditLogFailure() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.save(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        doThrow(new IllegalAccessException("IllegalAccessException")).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        mockTenantSettings();
        assertThrows(ValidationException.class, ()->spyService.create(commonRequestModel));
    }

    @Test
    void testRetrieveByIdOrGuid_IdSuccess() throws RunnerException {
        ConsolidationDetailsRequest request = ConsolidationDetailsRequest.builder().id(1L).build();
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        var consolidationDetailsResponse = consolidationService.retrieveByIdOrGuid(request);

        assertEquals(request.getId(), consolidationDetailsResponse.get().getId());
    }

    @Test
    void testRetrieveByIdOrGuid_GuidSuccess() throws RunnerException {
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        request.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb5ba31f0"));
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
        var consolidationDetailsResponse = consolidationService.retrieveByIdOrGuid(request);

        assertEquals(request.getGuid(), consolidationDetailsResponse.get().getGuid());
    }

    @Test
    void testRetrieveByIdOrGuid_NullRequest_Failure() throws RunnerException {
        ConsolidationDetailsRequest request = null;
        assertThrows(NullPointerException.class, ()-> consolidationService.retrieveByIdOrGuid(request));
    }
    @Test
    void testRetrieveByIdOrGuid_EmptyRequest_Failure() throws RunnerException {
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        assertThrows(RunnerException.class, ()-> consolidationService.retrieveByIdOrGuid(request));
    }

    @Test
    void testRetrieveByIdOrGuid_DataRetrieveForId_Failure() throws RunnerException {
        ConsolidationDetailsRequest request = ConsolidationDetailsRequest.builder().id(1L).build();
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, ()-> consolidationService.retrieveByIdOrGuid(request));
    }
    @Test
    void testRetrieveByIdOrGuid_DataRetrieveForGuid_Failure() throws RunnerException {
        ConsolidationDetailsRequest request = new ConsolidationDetailsRequest();
        request.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb5ba31f0"));
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, ()-> consolidationService.retrieveByIdOrGuid(request));
    }

    @Test
    void testUpdate_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        commonRequestModel.setData(copy);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> response = spyService.update(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testUpdate_DataRetrieval_Failure() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        var spyService = Mockito.spy(consolidationService);
        Mockito.doReturn(Optional.empty()).when(spyService).retrieveByIdOrGuid(any());
        assertThrows(ValidationException.class, () -> spyService.update(commonRequestModel));
    }

    @Test
    void testUpdate_ContainerListNull() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        copy.setContainersList(null);
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        commonRequestModel.setData(copy);
        ConsolidationDetails consolidationDetails = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        consolidationDetails.setContainersList(null);
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        var spyService = Mockito.spy(consolidationService);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> response = spyService.update(commonRequestModel);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testUpdate_GuidNotMatched_Failure() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetails entity = new ConsolidationDetails();
        entity.setId(1L);
        entity.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb5ba31f0"));
        ConsolidationDetailsRequest copy = modelMapperTest.map(entity , ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails oldEntity = new ConsolidationDetails();
        oldEntity.setId(1L);
        oldEntity.setGuid(UUID.fromString("1d27fe99-0874-4587-9a83-460bb7ba23f0"));

        var spyService = Mockito.spy(consolidationService);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(entity);
        Mockito.doReturn(Optional.of(oldEntity)).when(spyService).retrieveByIdOrGuid(any());
        assertThrows(ValidationException.class, () -> spyService.update(commonRequestModel));
    }

    @Test
    void testAttachShipments_Success_Sea() throws RunnerException {
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(1L);
        consoleShipmentMapping.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = new Containers();
        containers.setId(1L);
        shipmentDetails.setContainersList(List.of(containers));
        shipmentDetails.setId(2L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        shipmentDetails.setTenantId(UserContext.getUser().TenantId);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.setInterBranchConsole(false);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(2L);
        consoleShipmentMapping1.setConsolidationId(1L);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(1L);
        shipmentDetails1.setCarrierDetails(new CarrierDetails());
        shipmentDetails1.setTenantId(UserContext.getUser().TenantId);

        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));
        when(consoleShipmentMappingDao.assignShipments(any(), anyLong(), any(), any(), any(), any())).thenReturn(new HashSet<>(List.of(2L)));
//        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(containerDao.saveAll(anyList())).thenReturn(shipmentDetails.getContainersList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(consoleShipmentMapping, consoleShipmentMapping1));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails, shipmentDetails1)));
        when(shipmentDao.saveAll(anyList())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        doNothing().when(containerService).afterSaveList(anyList(),anyBoolean());
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.attachShipments(ShipmentRequestedType.APPROVE, 1L, shipmentIds);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAttachShipments_Success_Sea_LCLConsole() throws RunnerException {
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(1L);
        consoleShipmentMapping.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = new Containers();
        containers.setId(1L);
        shipmentDetails.setContainersList(List.of(containers));
        shipmentDetails.setId(2L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        shipmentDetails.setShipmentGateInDate(LocalDateTime.now());
        shipmentDetails.setTenantId(UserContext.getUser().TenantId);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setInterBranchConsole(true);
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);
        consolidationDetails.setCfsCutOffDate(LocalDateTime.MIN);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(2L);
        consoleShipmentMapping1.setConsolidationId(1L);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(1L);
        shipmentDetails1.setCarrierDetails(new CarrierDetails());
        shipmentDetails1.setTenantId(UserContext.getUser().TenantId);

        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));
        when(consoleShipmentMappingDao.assignShipments(any(), anyLong(), any(), any(), any(), any())).thenReturn(new HashSet<>(List.of(2L)));
//        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(containerDao.saveAll(anyList())).thenReturn(shipmentDetails.getContainersList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(consoleShipmentMapping, consoleShipmentMapping1));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails, shipmentDetails1)));
        doNothing().when(containerService).afterSaveList(anyList(),anyBoolean());
        mockShipmentSettings();
        mockTenantSettings();
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnableLclConsolidation(true);
        assertThrows(RunnerException.class, () -> consolidationService.attachShipments(ShipmentRequestedType.REJECT, 1L, shipmentIds));
    }

    @Test
    void testAttachShipments_Success_Air() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(1L);
        consoleShipmentMapping.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setId(2L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        shipmentDetails.setTenantId(UserContext.getUser().TenantId);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.setInterBranchConsole(false);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setShipmentType(Constants.DIRECTION_EXP);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(2L);
        consoleShipmentMapping1.setConsolidationId(1L);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(1L);
        shipmentDetails1.setCarrierDetails(new CarrierDetails());
        shipmentDetails1.setTenantId(UserContext.getUser().TenantId);

        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));
        when(consoleShipmentMappingDao.assignShipments(any(), anyLong(), any(), any(), any(), any())).thenReturn(new HashSet<>(List.of(2L)));
//        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(consoleShipmentMapping, consoleShipmentMapping1));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails, shipmentDetails1)));
        when(shipmentDao.saveAll(anyList())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.attachShipments(ShipmentRequestedType.APPROVE, 1L, shipmentIds);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAttachShipments_Success_Air_HazardousAirConsole() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(1L);
        consoleShipmentMapping.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setId(2L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        shipmentDetails.setContainsHazardous(true);
        shipmentDetails.setTenantId(UserContext.getUser().TenantId);
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setHazardous(false);
        consolidationDetails.setInterBranchConsole(false);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(2L);
        consoleShipmentMapping1.setConsolidationId(1L);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(1L);
        shipmentDetails1.setCarrierDetails(new CarrierDetails());
        shipmentDetails1.setTenantId(UserContext.getUser().TenantId);

        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));
        when(consoleShipmentMappingDao.assignShipments(any(), anyLong(), any(), any(), any(), any())).thenReturn(new HashSet<>(List.of(2L)));
//        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(consoleShipmentMapping, consoleShipmentMapping1));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails, shipmentDetails1)));
        when(shipmentDao.saveAll(anyList())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.attachShipments(ShipmentRequestedType.APPROVE, 1L, shipmentIds);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAttachShipments_ConsoleMapping_Failure() throws RunnerException {
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(2L);
        consoleShipmentMapping.setShipmentId(1L);
        consoleShipmentMapping.setIsAttachmentDone(true);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setInterBranchConsole(false);

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(new ArrayList<>());
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));

        RunnerException exception = assertThrows(RunnerException.class, () -> {
            consolidationService.attachShipments(ShipmentRequestedType.REJECT, 1L, shipmentIds);
        });

        assertEquals("Multiple consolidations are attached to the shipment, please verify.", exception.getMessage());
    }

    @Test
    void testAttachShipments_Success_Air_SavesPackUtilisation() throws RunnerException {
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        List<Long> shipmentIds = List.of(1L, 2L);
        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setConsolidationId(1L);
        consoleShipmentMapping.setShipmentId(1L);

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setId(2L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        shipmentDetails.setTenantId(UserContext.getUser().TenantId);
        shipmentDetails.setEventsList(List.of(new Events()));
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setCarrierDetails(new CarrierDetails());
        consolidationDetails.setInterBranchConsole(false);

        ConsoleShipmentMapping consoleShipmentMapping1 = new ConsoleShipmentMapping();
        consoleShipmentMapping1.setShipmentId(2L);
        consoleShipmentMapping1.setConsolidationId(1L);

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(1L);
        shipmentDetails1.setCarrierDetails(new CarrierDetails());
        shipmentDetails1.setTenantId(UserContext.getUser().TenantId);

        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping)));
        when(consoleShipmentMappingDao.assignShipments(any(), anyLong(), any(), any(), any(), any())).thenReturn(new HashSet<>(List.of(2L)));
//        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(consoleShipmentMapping, consoleShipmentMapping1));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails, shipmentDetails1)));
        when(shipmentDao.saveAll(anyList())).thenReturn(List.of(shipmentDetails, shipmentDetails1));
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.attachShipments(ShipmentRequestedType.APPROVE, 1L, shipmentIds);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        verify(packingService, times(1)).savePackUtilisationCalculationInConsole(any());
    }

    @Test
    void testDetachShipments_disabled_consolsplitflag() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = new Containers();
        containers.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setContainersList(List.of(containers));
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(1);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder()
                .enableConsolSplitBillCharge(false).build());
        mockTenantSettings();

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
        when(shipmentDao.findShipmentsByIds(shipmentIds.stream().collect(Collectors.toSet()))).thenReturn(List.of(shipmentDetails));
        doNothing().when(shipmentsContainersMappingDao).detachShipments(anyLong(), any(), anyBoolean());
        when(containerDao.saveAll(anyList())).thenReturn(shipmentDetails.getContainersList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_Success_Sea() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = new Containers();
        containers.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setContainersList(List.of(containers));
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(1);
        shipmentDetails.setEventsList(List.of(new Events()));

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder()
                .enableConsolSplitBillCharge(true).build());
        mockTenantSettings();

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
        when(shipmentDao.findShipmentsByIds(shipmentIds.stream().collect(Collectors.toSet()))).thenReturn(List.of(shipmentDetails));
        doNothing().when(shipmentsContainersMappingDao).detachShipments(anyLong(), any(), anyBoolean());
        when(containerDao.saveAll(anyList())).thenReturn(shipmentDetails.getContainersList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_Success_Air() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(1);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder()
                .enableConsolSplitBillCharge(true).build());
        mockTenantSettings();

        when(billingServiceAdapter.fetchBillingBulkSummaryBranchWise(any())).thenReturn(List.of(createTestBillingSummary()));

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(billingServiceAdapter.checkActiveCharges(any())).thenReturn(false);
        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_Success_Air_DgCase_NoShipment() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setContainsHazardous(true);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(1);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder()
                .enableConsolSplitBillCharge(true).build());
        mockTenantSettings();

        when(billingServiceAdapter.fetchBillingBulkSummaryBranchWise(any())).thenReturn(List.of(createTestBillingSummary()));

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(billingServiceAdapter.checkActiveCharges(any())).thenReturn(false);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_Success_Air_DgCase() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(1);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder()
                .enableConsolSplitBillCharge(true).build());
        mockTenantSettings();

        when(billingServiceAdapter.fetchBillingBulkSummaryBranchWise(any())).thenReturn(List.of(createTestBillingSummary()));

        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setId(2L);
        shipmentDetails1.setContainsHazardous(false);
        shipmentDetails1.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(2);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setGuid(UUID.randomUUID());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setShipmentsList(new ArrayList<>(List.of(shipmentDetails1)));

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(billingServiceAdapter.checkActiveCharges(any())).thenReturn(false);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(true);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_Success_Air_DgCase_NoShipment_DgFlagFalse() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setContainsHazardous(true);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(1);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder()
                .enableConsolSplitBillCharge(true).build());
        mockTenantSettings();

        when(billingServiceAdapter.fetchBillingBulkSummaryBranchWise(any())).thenReturn(List.of(createTestBillingSummary()));

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(billingServiceAdapter.checkActiveCharges(any())).thenReturn(false);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setAirDGFlag(false);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipments_activeCharges() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setContainsHazardous(true);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(1);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder()
                .enableConsolSplitBillCharge(true).build());
        mockTenantSettings();

        when(billingServiceAdapter.fetchBillingBulkSummaryBranchWise(any())).thenReturn(List.of(createTestBillingSummary()));

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setHazardous(true);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setGuid(UUID.randomUUID());

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentDao.findShipmentsByIds(shipmentIds.stream().collect(Collectors.toSet()))).thenReturn(List.of(shipmentDetails));
        when(billingServiceAdapter.checkActiveCharges(any())).thenReturn(true);

        assertThrows(BillingException.class, () -> consolidationService.detachShipments(1L, shipmentIds));
    }

    private BillingSummary createTestBillingSummary() {
        BillingSummary billingSummary = new BillingSummary();

        billingSummary.setTotalCount(1);
        billingSummary.setTotalRevenue(1000.0);
        billingSummary.setTotalCost(500.0);
        billingSummary.setTotalEstimatedCost(BigDecimal.valueOf(600.0));
        billingSummary.setTotalEstimatedRevenue(BigDecimal.valueOf(1200.0));
        billingSummary.setTotalEstimatedProfit(BigDecimal.valueOf(600.0));
        billingSummary.setTotalEstimatedProfitPercent(BigDecimal.valueOf(50.0));
        billingSummary.setTotalProfit(BigDecimal.valueOf(400.0));
        billingSummary.setTotalProfitPercent(BigDecimal.valueOf(40.0));
        billingSummary.setTotalPostedCost(BigDecimal.valueOf(500.0));
        billingSummary.setTotalPostedRevenue(BigDecimal.valueOf(1000.0));
        billingSummary.setTotalPostedProfit(BigDecimal.valueOf(500.0));
        billingSummary.setTotalPostedProfitPercent(BigDecimal.valueOf(50.0));
        billingSummary.setAccruedRevenue(800.0);
        billingSummary.setAccruedCost(300.0);
        billingSummary.setInvoicedRevenue(700.0);
        billingSummary.setInvoicedCost(350.0);
        billingSummary.setDisbursementAccruedRevenue(400.0);
        billingSummary.setDisbursementAccruedCost(200.0);
        billingSummary.setDisbursementInvoicedRevenue(300.0);
        billingSummary.setDisbursementInvoicedCost(150.0);
        billingSummary.setDisbursementRevenue(250.0);
        billingSummary.setDisbursementCost(100.0);
        billingSummary.setCumulativeGP(1500.0);
        billingSummary.setCumulativeGPPercentage(30.0);

        return billingSummary;
    }

    @Test
    void testDetachShipments_Sync_Failure() throws RunnerException {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Packing packing = new Packing();
        packing.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        shipmentDetails.setPackingList(List.of(packing));
        shipmentDetails.setGuid(UUID.randomUUID());
        shipmentDetails.setDirection(Constants.DIRECTION_EXP);
        shipmentDetails.setTenantId(1);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder()
                .enableConsolSplitBillCharge(true).build());
        mockTenantSettings();

        when(billingServiceAdapter.fetchBillingBulkSummaryBranchWise(any())).thenReturn(List.of(createTestBillingSummary()));

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consolidationDetails.setGuid(UUID.randomUUID());
        BillingSummary sampleBillingSummary = createTestBillingSummary();

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(shipmentDetails));
        when(shipmentDao.findShipmentsByIds(shipmentIds.stream().collect(Collectors.toSet()))).thenReturn(List.of(shipmentDetails));
        when(billingServiceAdapter.checkActiveCharges(any())).thenReturn(false);
        when(packingDao.saveAll(anyList())).thenReturn(shipmentDetails.getPackingList());
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(packingsADSync.sync(anyList(), any())).thenThrow(new RuntimeException("Test"));
        when(consolidationSync.sync(any(), any(), anyBoolean())).thenThrow(new RunnerException("Test"));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachShipments(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachShipmentsReverseSync_Success() {
        List<Long> shipmentIds = List.of(1L);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        Containers containers = new Containers();
        containers.setId(1L);
        shipmentDetails.setId(1L);
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setContainersList(List.of(containers));

        when(consoleShipmentMappingDao.detachShipments(anyLong(), any())).thenReturn(shipmentIds);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        doNothing().when(shipmentsContainersMappingDao).detachShipments(anyLong(), any(), anyBoolean());
        when(containerDao.saveAll(anyList())).thenReturn(shipmentDetails.getContainersList());
        ResponseEntity<HttpStatus> responseEntity = consolidationService.detachShipmentsReverseSync(1L, shipmentIds);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    // TODO: static method CommonUtils fix
    @Test
    void testPartialUpdate_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest consoleRequest = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        ConsolidationPatchRequest copy = modelMapperTest.map(consoleRequest, ConsolidationPatchRequest.class);
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        commonRequestModel.setData(copy);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());


        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(truckDriverDetailsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(List.of());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> response = spyService.partialUpdate(commonRequestModel, false);

        assertEquals(expectedEntity, response);
    }

    @Test
    void testPartialUpdate_DaoUpdate_Failure() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest consoleRequest = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        ConsolidationPatchRequest copy = modelMapperTest.map(consoleRequest, ConsolidationPatchRequest.class);
        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        commonRequestModel.setData(copy);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenThrow(new RuntimeException("Test"));
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> response = spyService.partialUpdate(commonRequestModel, false);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testCompleteUpdate_Success() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertToJson(consolidationDetails)).thenReturn("");
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), anyBoolean())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(truckDriverDetailsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(List.of());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(commonRequestModel);
        assertEquals(expectedEntity, responseEntity);
    }

    @Test
    void completeUpdateReverseSyncsFieldsIntoLinkedShipment() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        copy.setCarrierBookingRef("BookingRef#TEST_NEW");
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = objectMapperTest.convertValue(testConsol, ConsolidationDetails.class);
        consolidationDetails.setCarrierBookingRef("BookingRef#TEST");
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(consolidationDetails)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(objectMapperTest.convertValue(copy, ConsolidationDetails.class));
        when(jsonHelper.convertToJson(consolidationDetails)).thenReturn("");
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), anyBoolean())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(truckDriverDetailsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(List.of());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(commonRequestModel);
        assertEquals(expectedEntity, responseEntity);
    }

    @Test
    void testCompleteUpdate_Success_Air() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsRequest.class);
        copy.setEfreightStatus("newEfreightStatus");
        commonRequestModel.setData(copy);
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH, true));
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setEfreightStatus("newEfreightStatus");
        consolidationDetails.setInterBranchConsole(true);
        ConsolidationDetailsResponse expectedResponse = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsResponse.class);
        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.getCarrierDetails().setShippingLine("ABC Airline");

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIataTactFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        Awb awb = new Awb().setAwbGoodsDescriptionInfo(List.of(new AwbGoodsDescriptionInfo()));

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(oldEntity)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(oldEntity, ConsolidationDetails.class)).thenReturn(oldEntity);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertToJson(oldEntity)).thenReturn("");
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(awbDao.findByConsolidationId(consolidationDetails.getId())).thenReturn(List.of(awb));
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), anyBoolean())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
//        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
//        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        mockShipmentSettings();
        mockTenantSettings();

        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(commonRequestModel);
        assertEquals(expectedEntity, responseEntity);
    }

    @Test
    void testCompleteUpdate_Success_Air_LinkedShipmentUpdate1() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsRequest.class);
        copy.setEfreightStatus("newEfreightStatus");
        commonRequestModel.setData(copy);
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH, true));
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setEfreightStatus("newEfreightStatus");
        consolidationDetails.setInterBranchConsole(true);
        ConsolidationDetailsResponse expectedResponse = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsResponse.class);
        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.getCarrierDetails().setEta(LocalDateTime.of(2024, 10,3,4,30));

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIataTactFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        Awb awb = new Awb().setAwbGoodsDescriptionInfo(List.of(new AwbGoodsDescriptionInfo()));

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(oldEntity)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(oldEntity, ConsolidationDetails.class)).thenReturn(oldEntity);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertToJson(oldEntity)).thenReturn("");
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), anyBoolean())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        mockShipmentSettings();
        mockTenantSettings();

        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(commonRequestModel);
        assertEquals(expectedEntity, responseEntity);
    }

    @Test
    void testCompleteUpdate_Success_Air_LinkedShipmentUpdate2() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsRequest.class);
        copy.setEfreightStatus("newEfreightStatus");
        commonRequestModel.setData(copy);
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH, true));
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setEfreightStatus("newEfreightStatus");
        consolidationDetails.setInterBranchConsole(true);
        ConsolidationDetailsResponse expectedResponse = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsResponse.class);
        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.getCarrierDetails().setFlightNumber("232");

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIataTactFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);


        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(oldEntity)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(oldEntity, ConsolidationDetails.class)).thenReturn(oldEntity);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertToJson(oldEntity)).thenReturn("");
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), anyBoolean())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        mockShipmentSettings();
        mockTenantSettings();

        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(commonRequestModel);
        assertEquals(expectedEntity, responseEntity);
    }
    @Test
    void testCompleteUpdate_Success_Air_LinkedShipmentUpdate3() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsRequest.class);
        copy.setEfreightStatus("newEfreightStatus");
        commonRequestModel.setData(copy);
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH, true));
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setEfreightStatus("newEfreightStatus");
        consolidationDetails.setInterBranchConsole(true);
        ConsolidationDetailsResponse expectedResponse = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsResponse.class);
        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.getCarrierDetails().setAta(LocalDateTime.of(2024, 10,3,4,30));

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIataTactFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);


        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(oldEntity)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(oldEntity, ConsolidationDetails.class)).thenReturn(oldEntity);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertToJson(oldEntity)).thenReturn("");
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), anyBoolean())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
        mockShipmentSettings();
        mockTenantSettings();

        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(commonRequestModel);
        assertEquals(expectedEntity, responseEntity);
    }

    @Test
    void testCompleteUpdate_Failure_Air_WithInterBrnachPermission() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsRequest.class);
        copy.setEfreightStatus("newEfreightStatus");
        commonRequestModel.setData(copy);
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.CONSOLIDATIONS_AIR_INTER_BRANCH, false));
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setEfreightStatus("newEfreightStatus");
        consolidationDetails.setInterBranchConsole(true);
        ConsolidationDetailsResponse expectedResponse = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsResponse.class);
        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.getCarrierDetails().setOriginPort("INBLR_AIR");

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIataTactFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        Awb awb = new Awb().setAwbGoodsDescriptionInfo(List.of(new AwbGoodsDescriptionInfo()));

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(oldEntity)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(oldEntity, ConsolidationDetails.class)).thenReturn(oldEntity);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertToJson(oldEntity)).thenReturn("");
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(awbDao.findByConsolidationId(consolidationDetails.getId())).thenReturn(List.of(awb));
        mockShipmentSettings();
        mockTenantSettings();
        assertThrows(RuntimeException.class, () -> spyService.completeUpdate(commonRequestModel));
    }

    @Test
    void testCompleteUpdate_Success_Air_Sci() throws RunnerException {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsRequest.class);
        copy.setSci("T1");
        copy.setEfreightStatus("newEfreightStatus");
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        consolidationDetails.setSci("T1");
        consolidationDetails.setEfreightStatus("newEfreightStatus");
        ConsolidationDetailsResponse expectedResponse = jsonTestUtility.getJson("CONSOLIDATION_AIR", ConsolidationDetailsResponse.class);
        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.getCarrierDetails().setShippingLine("ABC Airline");

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIataTactFlag(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        Awb awb = new Awb().setAwbGoodsDescriptionInfo(List.of(new AwbGoodsDescriptionInfo()));

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        Mockito.doReturn(Optional.of(oldEntity)).when(spyService).retrieveByIdOrGuid(any());
        when(jsonHelper.convertValue(oldEntity, ConsolidationDetails.class)).thenReturn(oldEntity);
        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(jsonHelper.convertToJson(oldEntity)).thenReturn("");
        when(jsonHelper.convertValue(any(), eq(CarrierDetails.class))).thenReturn(CarrierDetails.builder().build());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(awbDao.findByConsolidationId(consolidationDetails.getId())).thenReturn(List.of(awb));
        when(consolidationDetailsDao.update(any(ConsolidationDetails.class), anyBoolean())).thenReturn(consolidationDetails);
        when(commonUtils.convertToEntityList(anyList(), any(), anyBoolean())).thenReturn(List.of());
        when(containerDao.updateEntityFromShipmentConsole(any(), any(), any(), anyBoolean())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(routingsDao.updateEntityFromConsole(any(), anyLong())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(consolidationSync.sync(any(), anyString(), anyBoolean())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(expectedResponse);
//        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
//        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        mockShipmentSettings();
        mockTenantSettings();

        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(commonRequestModel);
        assertEquals(expectedEntity, responseEntity);
    }

    @Test
    void testCalculateUtilization_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_M3);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateUtilization(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateUtilization_Success_WithWeightAndVolume() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeight(BigDecimal.TEN);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolume(BigDecimal.TEN);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateUtilization(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateUtilization_ConsoleNotFound_Failure() {
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        when(consolidationDetailsDao.findById(anyLong())).thenThrow(new RuntimeException("Test"));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateUtilization(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateAchieved_AllocatedForSameUnit_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_T);
        consolidationDetails.getAllocations().setWeight(BigDecimal.TEN);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_CC);
        consolidationDetails.getAllocations().setVolume(BigDecimal.TEN);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateAchieved_AllocatedForSameUnit_Failure() {
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        when(consolidationDetailsDao.findById(anyLong())).thenThrow(new RuntimeException("Test"));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateAchieved_AllocatedForSameUnit(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateChargeable_Success_Sea() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_T);
        consolidationDetails.getAllocations().setWeight(BigDecimal.TEN);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_CC);
        consolidationDetails.getAllocations().setVolume(BigDecimal.TEN);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateChargeable(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateChargeable_Success_Air() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.getAchievedQuantities().setConsolidatedWeightUnit(Constants.WEIGHT_UNIT_KG);
        consolidationDetails.getAllocations().setWeightUnit(Constants.WEIGHT_UNIT_T);
        consolidationDetails.getAllocations().setWeight(BigDecimal.TEN);
        consolidationDetails.getAchievedQuantities().setConsolidatedVolumeUnit(Constants.VOLUME_UNIT_M3);
        consolidationDetails.getAllocations().setVolumeUnit(Constants.VOLUME_UNIT_CC);
        consolidationDetails.getAllocations().setVolume(BigDecimal.TEN);
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        request.setTransportMode(consolidationDetails.getTransportMode());
        request.setContainerCategory(consolidationDetails.getContainerCategory());

        ConsoleCalculationsResponse response = new ConsoleCalculationsResponse();
        response.setAllocations(modelMapperTest.map(consolidationDetails.getAllocations(), AllocationsResponse.class));
        response.setAchievedQuantities(modelMapperTest.map(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class));

        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(request.getAllocations(), Allocations.class)).thenReturn(consolidationDetails.getAllocations());
        when(jsonHelper.convertValue(request.getAchievedQuantities(), AchievedQuantities.class)).thenReturn(consolidationDetails.getAchievedQuantities());
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(response.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(response.getAchievedQuantities());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateChargeable(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateChargeable_Failure() {
        ConsoleCalculationsRequest request = new ConsoleCalculationsRequest();
        request.setId(1L);
        when(consolidationDetailsDao.findById(anyLong())).thenThrow(new RuntimeException("Test"));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateChargeable(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateAchievedValues_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        Map<String, EntityTransferContainerType> keyMasterDataMap = new HashMap<>();
        EntityTransferContainerType containerTypeMasterData = jsonTestUtility.getJson("CONTAINER_TYPE_MASTER_DATA", EntityTransferContainerType.class);
        keyMasterDataMap.put("20GP", containerTypeMasterData);
        Cache cache = mock(Cache.class);

        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().WeightDecimalPlace(2)
                .WVGroupingNumber(0).WVDigitGrouping(1).VolumeDecimalPlace(2).build());

        var spyService = Mockito.spy(consolidationService);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(masterDataUtils.fetchInBulkContainerTypes(anyList())).thenReturn(keyMasterDataMap);
        when(cacheManager.getCache(anyString())).thenReturn(cache);
        when(jsonHelper.convertValue(consolidationDetails.getAllocations(), AllocationsResponse.class)).thenReturn(expectedResponse.getAllocations());
        when(jsonHelper.convertValue(consolidationDetails.getAchievedQuantities(), AchievedQuantitiesResponse.class)).thenReturn(expectedResponse.getAchievedQuantities());
        when(cache.get(any())).thenReturn(() -> containerTypeMasterData);
        mockShipmentSettings();
        mockTenantSettings();
        ResponseEntity<IRunnerResponse> responseEntity = spyService.calculateAchievedValues(CommonRequestModel.buildRequest(1L));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateAchievedValues_Failure() {
        when(consolidationDetailsDao.findById(anyLong())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateAchievedValues(CommonRequestModel.buildRequest(1L));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateContainerSummary_Success() throws RunnerException {
        CalculateContainerSummaryRequest request = new CalculateContainerSummaryRequest();
        request.setContainersList(List.of());
        Containers containers = new Containers();
        containers.setId(1L);
        ContainerSummaryResponse response = new ContainerSummaryResponse();

        when(jsonHelper.convertValueToList(request.getContainersList(), Containers.class)).thenReturn(List.of(containers));
        when(containerService.calculateContainerSummary(anyList(), any(), any())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateContainerSummary(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculateContainerSummary_Failure() throws RunnerException {
        CalculateContainerSummaryRequest request = new CalculateContainerSummaryRequest();
        request.setContainersList(List.of());
        Containers containers = new Containers();
        containers.setId(1L);

        when(jsonHelper.convertValueToList(request.getContainersList(), Containers.class)).thenReturn(List.of(containers));
        when(containerService.calculateContainerSummary(anyList(), any(), any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculateContainerSummary(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculatePackSummary_Success() throws RunnerException {
        CalculatePackSummaryRequest request = new CalculatePackSummaryRequest();
        request.setPackingList(List.of());
        Packing packing = new Packing();
        packing.setId(1L);
        PackSummaryResponse response = new PackSummaryResponse();

        when(jsonHelper.convertValueToList(request.getPackingList(), Packing.class)).thenReturn(List.of(packing));
        when(packingService.calculatePackSummary(anyList(), any(), any(), any())).thenReturn(response);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculatePackSummary(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculatePackSummary_Failure() throws RunnerException {
        CalculatePackSummaryRequest request = new CalculatePackSummaryRequest();
        request.setPackingList(List.of());
        Packing packing = new Packing();
        packing.setId(1L);

        when(jsonHelper.convertValueToList(request.getPackingList(), Packing.class)).thenReturn(List.of(packing));
        when(packingService.calculatePackSummary(anyList(), any(), any(), any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculatePackSummary(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCalculatePackUtilisationSuccess() throws RunnerException {
        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setPackingList(List.of());
        Packing packing = new Packing();
        packing.setId(1L);
        PackSummaryResponse response = new PackSummaryResponse();

        when(packingService.calculatePacksUtilisationForConsolidation(any())).thenReturn(response);
        when(jsonHelper.convertValue(any(), eq(CalculatePackUtilizationResponse.class))).thenReturn(new CalculatePackUtilizationResponse());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculatePackUtilisation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculatePackUtilisationSuccessForRegionalBranch() throws RunnerException {
        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setPackingList(List.of());
        request.setIsHub(false);
        Packing packing = new Packing();
        packing.setId(1L);
        PackSummaryResponse response = new PackSummaryResponse();

        when(packingService.calculatePacksUtilisationForConsolidation(any())).thenReturn(response);
        when(jsonHelper.convertValue(any(), eq(CalculatePackUtilizationResponse.class))).thenReturn(new CalculatePackUtilizationResponse());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculatePackUtilisation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCalculatePackUtilisationFailure() throws RunnerException {
        CalculatePackUtilizationRequest request = new CalculatePackUtilizationRequest();
        request.setPackingList(List.of());
        Packing packing = new Packing();
        packing.setId(1L);
        PackSummaryResponse response = new PackSummaryResponse();

        when(packingService.calculatePacksUtilisationForConsolidation(any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.calculatePackUtilisation(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }



    @Test
    void testListPacksForAssignDetach_Success_IsAssignTrue() {
        ConsolePacksListRequest request = new ConsolePacksListRequest();
        request.setIsAssign(true);
        request.setConsolidationId(1L);
        request.setContainerId(1L);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(2L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(3L).consolidationId(1L).build()));
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(1L);
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>(List.of(ShipmentsContainersMapping.builder()
                .containerId(1L).shipmentId(2L).build()));

        ConsolePacksListResponse.PacksList packsList = modelMapperTest.map(packing, ConsolePacksListResponse.PacksList.class);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        ShipmentDetails shipmentDetails3 = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        shipmentDetails3.setId(3L);
        shipmentDetails3.setShipmentType(Constants.CARGO_TYPE_FCL);
        PartiesResponse partiesResponse = modelMapperTest.map(shipmentDetails1.getClient(), PartiesResponse.class);

        Runnable mockRunnable = mock(Runnable.class);
        // Define the behavior of the mock
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(consoleShipmentMappingList);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(shipmentsContainersMappingList);
        when(jsonHelper.convertValue(packing, ConsolePacksListResponse.PacksList.class)).thenReturn(packsList);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));
        when(jsonHelper.convertValue(shipmentDetails1.getClient(), PartiesResponse.class)).thenReturn(partiesResponse);
        when(shipmentDao.findById(3L)).thenReturn(Optional.of(shipmentDetails3));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListPacksForAssignDetach_Success_IsAssignFalse() {
        ConsolePacksListRequest request = new ConsolePacksListRequest();
        request.setIsAssign(false);
        request.setConsolidationId(1L);
        request.setContainerId(1L);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(2L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(3L).consolidationId(1L).build()));
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(1L);
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>(List.of(ShipmentsContainersMapping.builder()
                .containerId(1L).shipmentId(2L).build()));

        ConsolePacksListResponse.PacksList packsList = modelMapperTest.map(packing, ConsolePacksListResponse.PacksList.class);

        ShipmentDetails shipmentDetails1 = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        PartiesResponse partiesResponse = modelMapperTest.map(shipmentDetails1.getClient(), PartiesResponse.class);

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(consoleShipmentMappingList);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(shipmentsContainersMappingList);
        when(jsonHelper.convertValue(packing, ConsolePacksListResponse.PacksList.class)).thenReturn(packsList);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails1));
        when(jsonHelper.convertValue(shipmentDetails1.getClient(), PartiesResponse.class)).thenReturn(partiesResponse);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListPacksForAssignDetach_Success_IsAssignFalse_NoPack() {
        ConsolePacksListRequest request = new ConsolePacksListRequest();
        request.setIsAssign(false);
        request.setConsolidationId(1L);
        request.setContainerId(1L);
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>(List.of(ConsoleShipmentMapping.builder()
                .shipmentId(1L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(2L).consolidationId(1L).build(), ConsoleShipmentMapping.builder()
                .shipmentId(3L).consolidationId(1L).build()));
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>(List.of(ShipmentsContainersMapping.builder()
                .containerId(1L).shipmentId(2L).build()));

        ShipmentDetails shipmentDetails2 = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        shipmentDetails2.setId(2L);
        shipmentDetails2.setShipmentType(Constants.CARGO_TYPE_FCL);
        PartiesResponse partiesResponse = modelMapperTest.map(shipmentDetails2.getClient(), PartiesResponse.class);

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(consoleShipmentMappingList);
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(shipmentsContainersMappingDao.findByContainerId(anyLong())).thenReturn(shipmentsContainersMappingList);
        when(shipmentDao.findById(2L)).thenReturn(Optional.of(shipmentDetails2));
        when(jsonHelper.convertValue(shipmentDetails2.getClient(), PartiesResponse.class)).thenReturn(partiesResponse);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListPacksForAssignDetach_Failure() {
        ConsolePacksListRequest request = new ConsolePacksListRequest();
        request.setIsAssign(true);
        request.setConsolidationId(1L);
        request.setContainerId(1L);

        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.listPacksForAssignDetach(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testAssignPacksAndShipments_Success() {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(2L);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);
        ContainerShipmentADInConsoleRequest.PacksList packsList = modelMapperTest.map(packing, ContainerShipmentADInConsoleRequest.PacksList.class);
        packsList.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        request.setPacksList(List.of(packsList));

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(containers));
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(packingDao.saveAll(anyList())).thenReturn(List.of(packing));
        when(containerService.calculateUtilization(any())).thenReturn(containers);
        when(containerDao.save(any())).thenReturn(containers);
        doNothing().when(shipmentsContainersMappingDao).assignShipments(anyLong(), any(), anyBoolean());
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAssignPacksAndShipments_Error() {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(2L);
        packing.setHazardous(true);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);
        ContainerShipmentADInConsoleRequest.PacksList packsList = modelMapperTest.map(packing, ContainerShipmentADInConsoleRequest.PacksList.class);
        packsList.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        request.setPacksList(List.of(packsList));

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(containers));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testAssignPacksAndShipments_Error1() {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        containers.setDgClass("2.1");
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(2L);
        packing.setHazardous(true);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);
        ContainerShipmentADInConsoleRequest.PacksList packsList = modelMapperTest.map(packing, ContainerShipmentADInConsoleRequest.PacksList.class);
        packsList.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        request.setPacksList(List.of(packsList));

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(containers));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testAssignPacksAndShipments_Error2() {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        containers.setDgClass("2.1");
        containers.setUnNumber("unNum");
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(2L);
        packing.setHazardous(true);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);
        ContainerShipmentADInConsoleRequest.PacksList packsList = modelMapperTest.map(packing, ContainerShipmentADInConsoleRequest.PacksList.class);
        packsList.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        request.setPacksList(List.of(packsList));

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(containers));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testAssignPacksAndShipments_Success2() throws RunnerException {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        containers.setDgClass("2.1");
        containers.setUnNumber("unNum");
        containers.setProperShippingName("psn");
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(2L);
        packing.setHazardous(true);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);
        ContainerShipmentADInConsoleRequest.PacksList packsList = modelMapperTest.map(packing, ContainerShipmentADInConsoleRequest.PacksList.class);
        packsList.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        request.setPacksList(List.of(packsList));

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(containers));
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(packingDao.saveAll(anyList())).thenReturn(List.of(packing));
        when(containerService.calculateUtilization(any())).thenReturn(containers);
        when(containerDao.save(any())).thenReturn(containers);
        doNothing().when(shipmentsContainersMappingDao).assignShipments(anyLong(), any(), anyBoolean());
        mockShipmentSettings();
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(new ShipmentDetails())));
        when(shipmentDao.saveAll(any())).thenReturn(List.of(new ShipmentDetails()));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAssignPacksAndShipments_Success3() throws RunnerException {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        containers.setDgClass("1.1");
        containers.setUnNumber("unNum");
        containers.setProperShippingName("psn");
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(2L);
        packing.setHazardous(true);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);
        ContainerShipmentADInConsoleRequest.PacksList packsList = modelMapperTest.map(packing, ContainerShipmentADInConsoleRequest.PacksList.class);
        packsList.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        request.setPacksList(List.of(packsList));

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(containers));
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(packing)));
        when(packingDao.saveAll(anyList())).thenReturn(List.of(packing));
        when(containerService.calculateUtilization(any())).thenReturn(containers);
        when(containerDao.save(any())).thenReturn(containers);
        doNothing().when(shipmentsContainersMappingDao).assignShipments(anyLong(), any(), anyBoolean());
        mockShipmentSettings();
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setOceanDGStatus(OceanDGStatus.OCEAN_DG_ACCEPTED);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails1)));
        when(shipmentDao.saveAll(any())).thenReturn(List.of(shipmentDetails1));
        when(shipmentSync.sync(any(), any(), any(), any(), anyBoolean())).thenThrow(new RunnerException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testAssignPacksAndShipments_Failure() {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(2L);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);
        ContainerShipmentADInConsoleRequest.PacksList packsList = modelMapperTest.map(packing, ContainerShipmentADInConsoleRequest.PacksList.class);
        packsList.setShipmentType(Constants.CARGO_TYPE_FCL);
        request.setPacksList(List.of(packsList));

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(containers));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.assignPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDetachPacksAndShipments_Success() {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        request.setIsFCL(true);
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setId(1L);
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        containers.setShipmentsList(List.of(shipmentDetails));
        Packing packing = jsonTestUtility.getJson("PACKING", Packing.class);
        packing.setId(1L);
        packing.setShipmentId(2L);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);
        ContainerShipmentADInConsoleRequest.PacksList packsList = modelMapperTest.map(packing, ContainerShipmentADInConsoleRequest.PacksList.class);
        packsList.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        request.setPacksList(List.of(packsList));

        ContainerResponse response = modelMapperTest.map(containers, ContainerResponse.class);

        when(containerDao.findById(anyLong())).thenReturn(Optional.of(containers));
        when(packingDao.findById(anyLong())).thenReturn(Optional.of(packing));
        when(packingDao.save(any())).thenReturn(packing);
        when(containerService.calculateUtilization(any())).thenReturn(containers);
        when(packingDao.findAll(any(), any())).thenReturn(null);
        when(containerDao.save(any())).thenReturn(containers);
        doNothing().when(shipmentsContainersMappingDao).detachShipments(anyLong(), any(), anyBoolean());
        doNothing().when(containerService).afterSave(any(), anyBoolean());
        when(jsonHelper.convertValue(containers, ContainerResponse.class)).thenReturn(response);

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDetachPacksAndShipments_Failure() {
        ContainerShipmentADInConsoleRequest request = new ContainerShipmentADInConsoleRequest();
        request.setIsFCL(true);
        Containers containers = jsonTestUtility.getJson("CONTAINER", Containers.class);
        ContainerRequest containerRequest = modelMapperTest.map(containers, ContainerRequest.class);
        request.setContainer(containerRequest);

        when(containerDao.findById(anyLong())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.detachPacksAndShipments(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testFullConsolidationsList_Success() {
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);

        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(modelMapper.map(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.fullConsolidationsList(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    @Test
    void testFullConsolidationsList_Success1() {
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        List<String> includeColumns = new ArrayList<>();
        includeColumns.add("id");
        includeColumns.add("consolidationNumber");
        sampleRequest.setIncludeColumns(includeColumns);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        Map<String, Object>  responseMap = new HashMap<>();
        responseMap.put("id", 1);
        responseMap.put("consolidationNumber", "CONS000231188");

        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.fullConsolidationsList(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testFullConsolidationsList_Failure() {
        ListCommonRequest sampleRequest = null;
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.fullConsolidationsList(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @ParameterizedTest
    @ValueSource(booleans = {
            true, false
    })
    void testList_Success1(boolean shipmentLevelContainer) {
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationListResponse response = modelMapperTest.map(testConsol, ConsolidationListResponse.class);

        consolidationDetails.setShipmentsList(Arrays.asList(shipmentDetails));
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(modelMapper.map(consolidationDetails, ConsolidationListResponse.class)).thenReturn(response);
        mockShipmentSettings();
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsShipmentLevelContainer(shipmentLevelContainer);
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.list(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testList_Success2() {
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        List<String> includeColumns = new ArrayList<>();
        includeColumns.add("id");
        includeColumns.add("consolidationNumber");
        sampleRequest.setIncludeColumns(includeColumns);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationListResponse response = modelMapperTest.map(testConsol, ConsolidationListResponse.class);
        Map<String, Object>  responseMap = new HashMap<>();
        responseMap.put("id", 1);
        responseMap.put("consolidationNumber", "CONS000231188");

        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(modelMapper.map(consolidationDetails, ConsolidationListResponse.class)).thenReturn(response);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.list(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testListReturnsConsolWithPendingNotifications() {
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        List<String> includeColumns = new ArrayList<>();
        includeColumns.add("id");
        includeColumns.add("consolidationNumber");
        sampleRequest.setIncludeColumns(includeColumns);
        sampleRequest.setNotificationFlag(true);
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationListResponse response = modelMapperTest.map(testConsol, ConsolidationListResponse.class);
        Map<String, Object>  responseMap = new HashMap<>();
        responseMap.put("id", 1);
        responseMap.put("consolidationNumber", "CONS000231188");

        Page<Long> consolIdPage = new PageImpl<>(List.of(1L));
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(consolidationDetailsDao.getIdWithPendingActions(any(), any())).thenReturn(consolIdPage);
        when(modelMapper.map(consolidationDetails, ConsolidationListResponse.class)).thenReturn(response);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.list(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testList_Success_BookingIdFilter() {
        ListCommonRequest sampleRequest = constructListCommonRequest("bookingId", 1, "=");

        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationListResponse response = modelMapperTest.map(testConsol, ConsolidationListResponse.class);
        GuidsListResponse resp = new GuidsListResponse();
        resp.setGuidsList(List.of(UUID.randomUUID()));

        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(modelMapper.map(consolidationDetails, ConsolidationListResponse.class)).thenReturn(response);
        when(v1Service.fetchBookingIdFilterGuids(any())).thenReturn(resp);
        mockShipmentSettings();

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.list(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testList_Failure() {
        ListCommonRequest sampleRequest = null;
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.list(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListAsync_Success() throws ExecutionException, InterruptedException {
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationListResponse response = modelMapperTest.map(testConsol, ConsolidationListResponse.class);

        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(modelMapper.map(consolidationDetails, ConsolidationListResponse.class)).thenReturn(response);
        mockShipmentSettings();

        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = consolidationService.listAsync(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.OK, responseEntity.get().getStatusCode());
    }
    @Test
    void testListAsync_Failure() throws ExecutionException, InterruptedException {
        ListCommonRequest sampleRequest = constructListCommonRequest("id", 1, "=");

        when(consolidationDetailsDao.findAll(any(), any())).thenThrow(new RuntimeException());
        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = consolidationService.listAsync(CommonRequestModel.buildRequest(sampleRequest));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.get().getStatusCode());
    }

    @Test
    void testDelete_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDelete_DataRetrieval_Failure() {
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_Success_byId() {
        ConsolidationDetails consolidationDetails = testConsol;
        for (Containers container: consolidationDetails.getContainersList()) {
            container.setContainerNumber("TCLU1666663");
        }
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setMergeContainers(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(consolidationDetails, ConsolidationDetailsResponse.class);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);
        mockShipmentSettings();

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_Success_byGuid() {
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(consolidationDetails, ConsolidationDetailsResponse.class);
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid("1d27fe99-0874-4587-9a83-460bb5ba31f0").build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_DataRetrieval_Failure() {
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_NullRequest_Failure() {
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetAllMasterData_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        Map<String, Object> response = new HashMap<>();
        var spyService = Mockito.spy(consolidationService);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);
        Mockito.doReturn(response).when(spyService).fetchAllMasterDataByKey(any(), any());

        ResponseEntity<IRunnerResponse> responseEntity = spyService.getAllMasterData(CommonRequestModel.buildRequest(1L));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    @Test
    void testGetAllMasterData_Failure() {
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAllMasterData(CommonRequestModel.buildRequest(1L));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testToggleLock_Success() throws RunnerException {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(consolidationDetailsDao.save(any(), anyBoolean())).thenReturn(consolidationDetails);
        doNothing().when(consolidationSync).syncLockStatus(any());

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.toggleLock(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testToggleLock_Failure() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setIsLocked(true);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));

        assertThrows(RunnerException.class, () -> consolidationService.toggleLock(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build())));
    }

    @Test
    void testExportExcel_Success() throws IOException, IllegalAccessException {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", 1, "=");
        MockHttpServletResponse response = new MockHttpServletResponse();
        ConsolidationDetails consolidationDetails = testConsol;
        UsersDto usersDto = UserContext.getUser();
        usersDto.setEnableTimeZone(false);
        UserContext.setUser(usersDto);
        ConsolidationListResponse consolidationListResponse = modelMapperTest.map(consolidationDetails, ConsolidationListResponse.class);

        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(consolidationDetails)));
        when(modelMapper.map(consolidationDetails, ConsolidationListResponse.class)).thenReturn(consolidationListResponse);
        mockShipmentSettings();

        consolidationService.exportExcel(response, CommonRequestModel.buildRequest(listCommonRequest));
        assertEquals(200,response.getStatus());
    }

//    @Test
//    void testPushShipmentDataToDependentService_Success() {
//        ConsolidationDetails consolidationDetails = testConsol;
//        when(trackingServiceAdapter.checkIfConsolContainersExist(any())).thenReturn(true);
//        when(trackingServiceAdapter.checkIfAwbExists(any())).thenReturn(true);
//
//
//        consolidationService.pushShipmentDataToDependentService(consolidationDetails, false)
//    }

    @Test
    void testGetConsolFromShipment_Success() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        ShipmentDetailsResponse shipmentDetailsResponse = modelMapperTest.map(shipmentDetails, ShipmentDetailsResponse.class);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setShipmentLite(false);
        PartiesResponse exportBrokerResponse = modelMapperTest.map(shipmentDetailsResponse.getAdditionalDetails().getExportBroker(), PartiesResponse.class);
        PartiesResponse importBrokerResponse = modelMapperTest.map(shipmentDetailsResponse.getAdditionalDetails().getImportBroker(), PartiesResponse.class);

        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(modelMapper.map(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(shipmentDetailsResponse);
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.of(shipmentSettingsDetails));
        when(jsonHelper.convertValue(shipmentDetailsResponse.getAdditionalDetails().getImportBroker(), PartiesResponse.class)).thenReturn(importBrokerResponse);
        when(jsonHelper.convertValue(shipmentDetailsResponse.getAdditionalDetails().getExportBroker(), PartiesResponse.class)).thenReturn(exportBrokerResponse);

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getConsolFromShipment(shipmentDetails.getId());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetConsolFromShipment_Failure_ShipmentDataRetrievalError() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> consolidationService.getConsolFromShipment(shipmentDetails.getId()));
    }

    @Test
    void testGetConsolFromShipment_Failure_TenantSettingsDataRetrievalError() {
        ShipmentDetails shipmentDetails = jsonTestUtility.getJson("SHIPMENT", ShipmentDetails.class);
        ShipmentDetailsResponse shipmentDetailsResponse = modelMapperTest.map(shipmentDetails, ShipmentDetailsResponse.class);
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        when(modelMapper.map(shipmentDetails, ShipmentDetailsResponse.class)).thenReturn(shipmentDetailsResponse);
        when(shipmentSettingsDao.findByTenantId(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> consolidationService.getConsolFromShipment(shipmentDetails.getId()));
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithMasterBill() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValueToList(Arrays.asList(testConsol), ConsolidationDetailsResponse.class)).thenReturn(Arrays.asList(consolidationDetailsResponse));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @NotNull
    private static AutoAttachConsolidationRequest getAutoAttachConsolidationRequest() {
        AutoAttachConsolidationRequest request = new AutoAttachConsolidationRequest();
        request.setMasterBill(testConsol.getBol());
        request.setTransportMode(testConsol.getTransportMode());
        request.setVessel(testConsol.getCarrierDetails().getVessel());
        request.setVoyageNumber(testConsol.getCarrierDetails().getVoyage());
        request.setEta(testConsol.getCarrierDetails().getEta());
        request.setEtd(testConsol.getCarrierDetails().getEtd());
        request.setPol(testConsol.getCarrierDetails().getOriginPort());
        request.setPod(testConsol.getCarrierDetails().getDestinationPort());
        return request;
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithoutMasterBill() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setMasterBill(null);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValueToList(Arrays.asList(testConsol), ConsolidationDetailsResponse.class)).thenReturn(Arrays.asList(consolidationDetailsResponse));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithMasterBill_AIR_EXP() {
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        request.setShipmentType(Constants.SHIPMENT_TYPE_LSE);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        var interBranchDto = InterBranchDto.builder().hubTenantIds(Arrays.asList()).build();
        interBranchDto.setHubTenantIds(List.of(2,3));
        interBranchDto.setCoLoadStation(true);
        InterBranchContext.setContext(interBranchDto);

        V1TenantSettingsResponse tenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettingsResponse.setIsMAWBColoadingEnabled(true);
        mockTenantSettings();
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValueToList(Arrays.asList(testConsol), ConsolidationDetailsResponse.class)).thenReturn(Arrays.asList(consolidationDetailsResponse));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithMasterBill_AIR_EXP_emptyHubTenantIds() {
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        var interBranchDto = InterBranchDto.builder().hubTenantIds(Arrays.asList()).build();
        interBranchDto.setHubTenantIds(List.of());
        interBranchDto.setCoLoadStation(true);
        InterBranchContext.setContext(interBranchDto);

        V1TenantSettingsResponse tenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettingsResponse.setIsMAWBColoadingEnabled(true);
        mockTenantSettings();
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValueToList(Arrays.asList(testConsol), ConsolidationDetailsResponse.class)).thenReturn(Arrays.asList(consolidationDetailsResponse));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithMasterBill_AIR_EXP_CoLoadFlag_False() {
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        V1TenantSettingsResponse tenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettingsResponse.setIsMAWBColoadingEnabled(false);
        mockTenantSettings();
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValueToList(Arrays.asList(testConsol), ConsolidationDetailsResponse.class)).thenReturn(Arrays.asList(consolidationDetailsResponse));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithMasterBill_AIR_IMP() {
        testConsol.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_IMP);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        var interBranchDto = InterBranchDto.builder().hubTenantIds(Arrays.asList()).build();
        interBranchDto.setHubTenantIds(List.of(2,3));
        interBranchDto.setCoLoadStation(true);
        InterBranchContext.setContext(interBranchDto);

        V1TenantSettingsResponse tenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        tenantSettingsResponse.setIsMAWBColoadingEnabled(true);
        mockTenantSettings();
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValueToList(Arrays.asList(testConsol), ConsolidationDetailsResponse.class)).thenReturn(Arrays.asList(consolidationDetailsResponse));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithoutMasterBill_PartiesCheck() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        request.setMasterBill(null);
        request.setShipmentType(Constants.CARGO_TYPE_FCL);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnablePartyCheckForConsolidation(true);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValue(testConsol, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithoutMasterBill_PartiesCheck1() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        request.setMasterBill(null);
        request.setShipmentType(Constants.CARGO_TYPE_FCL);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnablePartyCheckForConsolidation(true);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setShipmentType(Constants.CARGO_TYPE_FCL);
        shipmentDetails1.getConsigner().setOrgCode("NewOrgCode");
        shipmentDetails1.getClient().setAddressCode("NewAddressCode");
        request.setConsignee(modelMapperTest.map(shipmentDetails.getConsignee(), PartiesRequest.class));
        testConsol.setShipmentsList(new ArrayList<>(List.of(shipmentDetails, shipmentDetails1)));
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();


        mockShipmentSettings();
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValue(testConsol, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);
        when(jsonHelper.convertValue(new Parties(), PartiesResponse.class)).thenReturn(new PartiesResponse());
        when(jsonHelper.convertValue(shipmentDetails.getConsignee(), PartiesResponse.class)).thenReturn(modelMapperTest.map(shipmentDetails.getConsignee(), PartiesResponse.class));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithoutMasterBill_PartiesCheck_LCL() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        request.setMasterBill(null);
        request.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnablePartyCheckForConsolidation(true);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        shipmentDetails1.getConsigner().setOrgCode("NewOrgCode");
        shipmentDetails1.getClient().setAddressCode("NewAddressCode");
        request.setConsignee(modelMapperTest.map(shipmentDetails.getConsignee(), PartiesRequest.class));
        testConsol.setShipmentsList(new ArrayList<>(List.of(shipmentDetails, shipmentDetails1)));
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();


        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValue(testConsol, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithoutMasterBill_PartiesCheck_BLK() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        request.setMasterBill(null);
        request.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnablePartyCheckForConsolidation(true);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_BLK);
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setShipmentType(Constants.SHIPMENT_TYPE_BLK);
        shipmentDetails1.getConsigner().setOrgCode("NewOrgCode");
        shipmentDetails1.getClient().setAddressCode("NewAddressCode");
        request.setConsignee(modelMapperTest.map(shipmentDetails.getConsignee(), PartiesRequest.class));
        testConsol.setShipmentsList(new ArrayList<>(List.of(shipmentDetails, shipmentDetails1)));
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        mockShipmentSettings();
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithoutMasterBill_WithoutPartiesCheck_FCL() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        request.setMasterBill(null);
        request.setShipmentType(Constants.CARGO_TYPE_FCL);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnablePartyCheckForConsolidation(false);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setShipmentType(Constants.CARGO_TYPE_FCL);
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setShipmentType(Constants.CARGO_TYPE_FCL);
        request.setConsignee(modelMapperTest.map(shipmentDetails.getConsignee(), PartiesRequest.class));
        testConsol.setShipmentsList(new ArrayList<>(List.of(shipmentDetails, shipmentDetails1)));
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(testConsol, ConsolidationDetailsResponse.class);
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        mockShipmentSettings();
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(null);
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));
        when(jsonHelper.convertValue(testConsol, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetails_Success_WithoutMasterBill_PartiesCheck_LCL_FCL() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setDirection(Constants.DIRECTION_EXP);
        request.setMasterBill(null);
        request.setShipmentType(Constants.CARGO_TYPE_FCL);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setEnablePartyCheckForConsolidation(true);
        List<EntityTransferMasterLists> masterLists = jsonTestUtility.getAutoAttachConsoleMasterData();
        ShipmentDetails shipmentDetails = jsonTestUtility.getCompleteShipment();
        shipmentDetails.setShipmentType(Constants.SHIPMENT_TYPE_LCL);
        ShipmentDetails shipmentDetails1 = jsonTestUtility.getCompleteShipment();
        shipmentDetails1.setShipmentType(Constants.CARGO_TYPE_FCL);
        request.setConsignee(modelMapperTest.map(shipmentDetails.getConsignee(), PartiesRequest.class));
        testConsol.setShipmentsList(new ArrayList<>(List.of(shipmentDetails, shipmentDetails1)));
        V1DataResponse v1DataResponse = V1DataResponse.builder().entities(masterLists).build();

        mockShipmentSettings();
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
        when(v1Service.fetchMasterData(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferMasterLists.class)).thenReturn(masterLists);
        when(consolidationDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testConsol)));

        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoAttachConsolidationDetails(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoUpdateGoodsAndHandlingInfo_Success() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(consolidationDetails, ConsolidationDetailsResponse.class);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setMultipleShipmentEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);


        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(consolidationDetails));
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoUpdateGoodsAndHandlingInfo(CommonRequestModel.buildRequest(1L));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetAutoUpdateGoodsAndHandlingInfo_Failure() {
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getAutoUpdateGoodsAndHandlingInfo(CommonRequestModel.buildRequest(1L));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetContainerPackSummary_Success() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder()
                .consolidationId(consolidationDetails.getId())
                .shipmentId(consolidationDetails.getShipmentsList().get(0).getId())
                .build();
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        List<MasterListRequest> requests = new ArrayList<>();
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenReturn(List.of(consoleShipmentMapping));
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(shipmentDetails.getPackingList()));
        when(shipmentDao.findById(anyLong())).thenReturn(Optional.of(shipmentDetails));
        shipmentDetails.getPackingList().forEach(x -> when(jsonHelper.convertValue(x, ContainerPackSummaryDto.PacksList.class)).thenReturn(modelMapperTest.map(x, ContainerPackSummaryDto.PacksList.class)));
        when(jsonHelper.convertValue(shipmentDetails.getClient(), PartiesResponse.class)).thenReturn(modelMapperTest.map(shipmentDetails.getClient(), PartiesResponse.class));
        when(masterDataUtils.createInBulkMasterListRequest(any(), any(), anyMap(), any() )).thenReturn(requests);
        when(masterDataUtils.fetchInBulkMasterList(any())).thenReturn(new HashMap<>());
        when(masterDataUtils.setMasterData(any(), any())).thenReturn(new HashMap<>());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getContainerPackSummary(CommonRequestModel.buildRequest(consolidationDetails.getId()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetContainerPackSummary_Failure() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        when(consoleShipmentMappingDao.findByConsolidationId(anyLong())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getContainerPackSummary(CommonRequestModel.buildRequest(consolidationDetails.getId()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetDefaultConsolidation_Failure() {
        var spyService = Mockito.spy(consolidationService);
        Mockito.doThrow(new RuntimeException()).when(spyService).generateCustomBolNumber();
        mockShipmentSettings();
        ResponseEntity<IRunnerResponse> responseEntity = spyService.getDefaultConsolidation();
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetIdFromGuid_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getIdFromGuid(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(consolidationDetails.getGuid().toString()).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testGetIdFromGuid_Failure() {
        ConsolidationDetails consolidationDetails = testConsol;
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = consolidationService.getIdFromGuid(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid(consolidationDetails.getGuid().toString()).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGenerateCustomBolNumber_Success_Serial() {
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setBolNumberPrefix("CONS");
        shipmentSettingsDetails.setBolNumberGeneration(GenerationType.Serial);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        when(v1Service.getMaxConsolidationId()).thenReturn("2313");
        mockShipmentSettings();

        String res = consolidationService.generateCustomBolNumber();
        assertEquals("CONS2313", res);
    }

    @Test
    void testGenerateCustomBolNumber_Success_Random() {
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setBolNumberPrefix("CONS");
        shipmentSettingsDetails.setBolNumberGeneration(GenerationType.Random);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);
        mockShipmentSettings();
        String res = consolidationService.generateCustomBolNumber();
        assertEquals(14, res.length());
    }

    @Test
    void testAutoGenerateEvents() {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setEventsList(null);
        consolidationService.autoGenerateEvents(consolidationDetails);
        verify(eventDao, times(1)).save(any());
    }

    @Test
    void testGenerateConsolidationNumber_Success() throws RunnerException {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setConsolidationNumber(null);
        consolidationDetails.setReferenceNumber(null);
        consolidationDetails.setBol(null);
        TenantProducts tenantProducts = new TenantProducts();
        tenantProducts.setId(1L);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setConsolidationLite(false);
        shipmentSettingsDetails.setCustomisedSequence(true);
        var spyService = Mockito.spy(consolidationService);
        when(shipmentSettingsDao.list()).thenReturn(List.of(shipmentSettingsDetails));
        when(productEngine.populateEnabledTenantProducts(shipmentSettingsDetails)).thenReturn(List.of(tenantProducts));
        when(productEngine.GetCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI)).thenReturn("CONS007262");
        when(productEngine.IdentifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
        when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
        when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("BOL23131");
        mockShipmentSettings();
        spyService.generateConsolidationNumber(consolidationDetails);
        assertEquals("CONS007262", consolidationDetails.getConsolidationNumber());
        assertEquals("CONS007262", consolidationDetails.getReferenceNumber());
        assertEquals("BOL23131", consolidationDetails.getBol());
    }

    @Test
    void testGenerateConsolidationNumber_Success1() throws RunnerException {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setConsolidationNumber(null);
        consolidationDetails.setReferenceNumber(null);
        consolidationDetails.setBol(null);
        TenantProducts tenantProducts = new TenantProducts();
        tenantProducts.setId(1L);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setConsolidationLite(false);
        shipmentSettingsDetails.setCustomisedSequence(true);
        var spyService = Mockito.spy(consolidationService);
        when(shipmentSettingsDao.list()).thenReturn(List.of(shipmentSettingsDetails));
        when(productEngine.populateEnabledTenantProducts(shipmentSettingsDetails)).thenReturn(List.of(tenantProducts));
        when(productEngine.GetCommonSequenceNumber(consolidationDetails.getTransportMode(), ProductProcessTypes.Consol_Shipment_TI)).thenReturn("");
        when(productEngine.IdentifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
        when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
        when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("");
        when(v1Service.getMaxConsolidationId()).thenReturn("123311");
        doReturn("BOL2121").when(spyService).generateCustomBolNumber();
        mockShipmentSettings();
        spyService.generateConsolidationNumber(consolidationDetails);
        assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
        assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
        assertEquals("BOL2121", consolidationDetails.getBol());
    }

    @Test
    void testGenerateConsolidationNumber_Success2() throws RunnerException {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setConsolidationNumber(null);
        consolidationDetails.setReferenceNumber(null);
        consolidationDetails.setBol(null);
        TenantProducts tenantProducts = new TenantProducts();
        tenantProducts.setId(1L);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setConsolidationLite(false);
        shipmentSettingsDetails.setCustomisedSequence(false);
        var spyService = Mockito.spy(consolidationService);
        when(shipmentSettingsDao.list()).thenReturn(List.of(shipmentSettingsDetails));
        when(productEngine.populateEnabledTenantProducts(shipmentSettingsDetails)).thenReturn(List.of(tenantProducts));
        when(productEngine.IdentifyProduct(any(ConsolidationDetails.class), any())).thenReturn(tenantProducts);
        when(getNextNumberHelper.getProductSequence(anyLong(), any())).thenReturn(new ProductSequenceConfig());
        when(getNextNumberHelper.generateCustomSequence(any(), anyString(), anyInt(), anyBoolean(), any(), anyBoolean())).thenReturn("");
        when(v1Service.getMaxConsolidationId()).thenReturn("123311");
        doReturn("BOL2121").when(spyService).generateCustomBolNumber();
        mockShipmentSettings();
        spyService.generateConsolidationNumber(consolidationDetails);
        assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
        assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
        assertEquals("BOL2121", consolidationDetails.getBol());
    }

    @Test
    void testGenerateConsolidationNumberWithConsolidationLiteTrue() throws RunnerException {
        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setConsolidationNumber(null);
        consolidationDetails.setReferenceNumber(null);
        consolidationDetails.setBol(null);
        TenantProducts tenantProducts = new TenantProducts();
        tenantProducts.setId(1L);
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setConsolidationLite(true);
        shipmentSettingsDetails.setCustomisedSequence(false);
        var spyService = Mockito.spy(consolidationService);
        when(shipmentSettingsDao.list()).thenReturn(List.of(shipmentSettingsDetails));
        when(v1Service.getMaxConsolidationId()).thenReturn("123311");
        mockShipmentSettings();
        spyService.generateConsolidationNumber(consolidationDetails);
        assertEquals("CONS000123311", consolidationDetails.getConsolidationNumber());
        assertEquals("CONS000123311", consolidationDetails.getReferenceNumber());
        assertNull(consolidationDetails.getBol());
    }

    @Test
    void testCompleteV1ConsolidationCreateAndUpdate_Success() throws RunnerException {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        ShipmentDetails shipmentDetails = consolidationDetails.getShipmentsList().get(0);
        ConsolidationDetailsRequest consolidationDetailsRequest = modelMapperTest.map(consolidationDetails, ConsolidationDetailsRequest.class);
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(consolidationDetails, ConsolidationDetailsResponse.class);
        var spyService = Mockito.spy(consolidationService);
        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(shipmentDetails));
        when(objectMapper.convertValue(consolidationDetailsRequest, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(consolidationDetailsDao.update(consolidationDetails, true)).thenReturn(consolidationDetails);
        doNothing().when(consolidationDetailsDao).saveCreatedDateAndUser(any(), any(), any());
        when(commonUtils.convertToEntityList(anyList(), any())).thenReturn(List.of());
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(containerDao.updateEntityFromConsolidationV1(any(), any(), any())).thenReturn(consolidationDetails.getContainersList());
        when(packingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(packingDao.updateEntityFromConsole(any(), anyLong(), any())).thenReturn(consolidationDetails.getPackingList());
        when(eventDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(eventDao.updateEntityFromOtherEntity(any(), anyLong(), anyString(), any())).thenReturn(consolidationDetails.getEventsList());
        when(referenceNumbersDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(referenceNumbersDao.updateEntityFromConsole(any(), anyLong(), any())).thenReturn(consolidationDetails.getReferenceNumbersList());
        when(routingsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(routingsDao.updateEntityFromConsole(any(), anyLong(), any())).thenReturn(consolidationDetails.getRoutingsList());
        when(partiesDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        when(partiesDao.updateEntityFromOtherEntity(any(), anyLong(), anyString(), any())).thenReturn(consolidationDetails.getConsolidationAddresses());
        when(jsonHelper.convertValue(consolidationDetails, ConsolidationDetailsResponse.class)).thenReturn(consolidationDetailsResponse);
        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeV1ConsolidationCreateAndUpdate(CommonRequestModel.buildRequest(consolidationDetailsRequest), false, "user", LocalDateTime.now());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(consolidationDetailsResponse), responseEntity);
    }

    @Test
    void testPushShipmentDataToDependentService_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        UniversalTrackingPayload universalTrackingPayload = new UniversalTrackingPayload();
        UniversalTrackingPayload.UniversalEventsPayload eventsPayload = new UniversalTrackingPayload.UniversalEventsPayload();
        var spyService = Mockito.spy(consolidationService);
        when(trackingServiceAdapter.checkIfConsolContainersExist(consolidationDetails)).thenReturn(true);
        when(trackingServiceAdapter.mapConsoleDataToTrackingServiceData(consolidationDetails)).thenReturn(universalTrackingPayload);
        when(jsonHelper.convertToJson(any())).thenReturn("");
        doNothing().when(trackingServiceAdapter).publishUpdatesToTrackingServiceQueue("", false);
        when(trackingServiceAdapter.getAllEvents(null,consolidationDetails, consolidationDetails.getReferenceNumber())).thenReturn(List.of());
        when(trackingServiceAdapter.mapEventDetailsForTracking(anyString(), anyString(), anyString(), anyList())).thenReturn(eventsPayload);
        doNothing().when(trackingServiceAdapter).publishUpdatesToTrackingServiceQueue("", true);
        spyService.pushShipmentDataToDependentService(consolidationDetails, true, null);
        verify(trackingServiceAdapter, times(1)).publishUpdatesToTrackingServiceQueue("", false);
        verify(trackingServiceAdapter, times(1)).publishUpdatesToTrackingServiceQueue("", true);
    }

    @Test
    void testPushShipmentDataToDependentService_Failure() {
        ConsolidationDetails consolidationDetails = testConsol;
        var spyService = Mockito.spy(consolidationService);
        when(producer.getKafkaResponse(any(), anyBoolean())).thenThrow(new RuntimeException());
        when(trackingServiceAdapter.checkIfConsolContainersExist(consolidationDetails)).thenThrow(new RuntimeException());
        spyService.pushShipmentDataToDependentService(consolidationDetails, true, null);
        verify(producer, times(1)).getKafkaResponse(any(), anyBoolean());
        verify(trackingServiceAdapter, times(1)).checkIfConsolContainersExist(consolidationDetails);
    }

    @Test
    void testFetchAllMasterDataByKey_Success() {
        ConsolidationDetails consolidationDetails = testConsol;
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(consolidationDetails, ConsolidationDetailsResponse.class);
        var spyService = Mockito.spy(consolidationService);
        Runnable mockRunnable = mock(Runnable.class);
        // Define the behavior of the mock
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });

        Map<String, Object> response = spyService.fetchAllMasterDataByKey(consolidationDetails, consolidationDetailsResponse);
        assertEquals(new HashMap<>(), response);
    }

    @Test
    void testCreateConsolidationPayload_Success() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getCompleteConsolidation();
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_ROA);
        ConsoleShipmentMapping consoleShipmentMapping = ConsoleShipmentMapping.builder()
                .shipmentId(consolidationDetails.getShipmentsList().get(0).getId())
                .consolidationId(consolidationDetails.getId())
                .build();
        TruckDriverDetails truckDriverDetails = jsonTestUtility.getTestTruckDriverDetails();
        truckDriverDetails.setConsolidationId(consolidationDetails.getId());
        truckDriverDetails.setContainerId(consolidationDetails.getContainersList().get(0).getId());
        truckDriverDetails.setShipmentId(consolidationDetails.getShipmentsList().get(0).getId());
        TruckDriverDetailsResponse truckDriverDetailsResponse = modelMapperTest.map(truckDriverDetails, TruckDriverDetailsResponse.class);
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(consolidationDetails, ConsolidationDetailsResponse.class);
        var spyService = Mockito.spy(consolidationService);
        Runnable mockRunnable = mock(Runnable.class);
        // Define the behavior of the mock
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        when(awbDao.findByConsolidationId(consolidationDetailsResponse.getId())).thenReturn(List.of());
        when(consoleShipmentMappingDao.findByConsolidationId(consolidationDetails.getId())).thenReturn(List.of(consoleShipmentMapping));
        when(truckDriverDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(truckDriverDetails)));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(consolidationDetails.getShipmentsList()));
        when(jsonHelper.convertValue(truckDriverDetails, TruckDriverDetailsResponse.class)).thenReturn(truckDriverDetailsResponse);
        spyService.createConsolidationPayload(consolidationDetails, consolidationDetailsResponse);
        verify(truckDriverDetailsDao, times(1)).findAll(any(), any());
        assertEquals(List.of(truckDriverDetailsResponse), consolidationDetailsResponse.getTruckDriverDetails());
    }

    @Test
    void testCreateConsolidationPayload_Success1() {
        ConsolidationDetails consolidationDetails = jsonTestUtility.getTestConsolidationAir();
        ConsolidationDetailsResponse consolidationDetailsResponse = modelMapperTest.map(consolidationDetails, ConsolidationDetailsResponse.class);
        var spyService = Mockito.spy(consolidationService);
        Runnable mockRunnable = mock(Runnable.class);
        // Define the behavior of the mock
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            // Get the argument passed to the withMdc method
            Runnable argument = invocation.getArgument(0);
            // Call the run method of the argument
            argument.run();
            // Add any additional behavior or return value as needed
            return mockRunnable;
        });
        Awb awb = new Awb();
        awb.setAirMessageStatus(AwbStatus.AIR_MESSAGE_SENT);
        awb.setLinkedHawbAirMessageStatus(AwbStatus.AIR_MESSAGE_SENT);
        when(awbDao.findByConsolidationId(consolidationDetailsResponse.getId())).thenReturn(List.of(awb));
        spyService.createConsolidationPayload(consolidationDetails, consolidationDetailsResponse);
        assertEquals(AwbStatus.AIR_MESSAGE_SENT, consolidationDetailsResponse.getAwbStatus());
        assertEquals(AwbStatus.AIR_MESSAGE_SENT, consolidationDetailsResponse.getLinkedHawbStatus());
    }

    @Test
    void testCreate_SuccessRa() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());


        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("RegulatedAgent", true);
        addressMap.put("o1#c1", map);
        orgAddressResponse.setAddresses(addressMap);

//        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        mockTenantSettings();
        String errorMessage = "Screening Status and Security Status is mandatory for RA consignor.";
        Exception e = assertThrows(RunnerException.class, () -> {
            spyService.validateRaKcForConsol(consolidationDetails);
        });

        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreate_SuccessIdNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setId(null);

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("KnownConsignor", true);
        addressMap.put("c1#o1", map);
        orgAddressResponse.setAddresses(addressMap);

        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
//        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        Exception e = assertThrows(ValidationException.class, () -> {
            spyService.create(commonRequestModel);
        });
    }

    @Test
    void testCreate_SuccessIdNullSendingAgent() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setId(null);

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("RegulatedAgent", true);
        addressMap.put("o1#c1", map);
        orgAddressResponse.setAddresses(addressMap);

//        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        mockTenantSettings();

        String errorMessage = "Screening Status and Security Status is mandatory for RA consignor.";
        Exception e = assertThrows(RunnerException.class, () -> {
            spyService.validateRaKcForConsol(consolidationDetails);
        });

        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreate_SuccessRaScreeningStatusNotNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());


        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setScreeningStatus(Collections.emptyList());

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("RegulatedAgent", true);
        addressMap.put("o1#c1", map);
        orgAddressResponse.setAddresses(addressMap);

//        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        mockTenantSettings();
        String errorMessage = "Screening Status and Security Status is mandatory for RA consignor.";
        Exception e = assertThrows(RunnerException.class, () -> {
            spyService.validateRaKcForConsol(consolidationDetails);
        });

        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreate_SuccessIdNullScreeningStatusNotNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setScreeningStatus(Collections.emptyList());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setId(null);

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("KnownConsignor", true);
        addressMap.put("o1#c1", map);
        orgAddressResponse.setAddresses(addressMap);

//        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        mockTenantSettings();
        spyService.validateRaKcForConsol(consolidationDetails);
        verify(v1ServiceUtil, times(1)).fetchOrgInfoFromV1(anyList());
    }

    @Test
    void testCreate_SuccessIdNullSendingAgentScreeningStatusNotNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setScreeningStatus(Collections.emptyList());
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setId(null);

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("RegulatedAgent", true);
        addressMap.put("o1#c1", map);
        orgAddressResponse.setAddresses(addressMap);

//        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        mockTenantSettings();
        String errorMessage = "Screening Status and Security Status is mandatory for RA consignor.";
        Exception e = assertThrows(RunnerException.class, () -> {
            spyService.validateRaKcForConsol(consolidationDetails);
        });

        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreate_SuccessRaSecurityStatusNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());


        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        List<String> status = new ArrayList<>();
        status.add("PHS");
        consolidationDetails.setScreeningStatus(status);

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("RegulatedAgent", true);
        addressMap.put("o1#c1", map);
        orgAddressResponse.setAddresses(addressMap);

//        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        mockTenantSettings();
        String errorMessage = "Screening Status and Security Status is mandatory for RA consignor.";
        Exception e = assertThrows(RunnerException.class, () -> {
            spyService.validateRaKcForConsol(consolidationDetails);
        });

        assertEquals(errorMessage, e.getMessage());
    }

    @Test
    void testCreate_SuccessIdNullSecurityStatusNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;
        List<String> status = new ArrayList<>();
        status.add("PHS");
        consolidationDetails.setScreeningStatus(status);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setId(null);

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("KnownConsignor", true);
        addressMap.put("c1#o1", map);
        orgAddressResponse.setAddresses(addressMap);

//        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        mockTenantSettings();
        spyService.validateRaKcForConsol(consolidationDetails);
        verify(v1ServiceUtil, times(1)).fetchOrgInfoFromV1(any());
    }

    @Test
    void testCreate_SuccessIdNullSendingAgentSecurityStatusNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().EnableAirMessaging(true).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        ConsolidationDetailsRequest copy = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetailsRequest.class);
        commonRequestModel.setData(copy);

        ConsolidationDetails consolidationDetails = testConsol;

        List<String> status = new ArrayList<>();
        status.add("PHS");
        consolidationDetails.setScreeningStatus(status);
        consolidationDetails.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        consolidationDetails.setId(null);

        Parties parties = Parties.builder().addressCode("c1").orgCode("o1").build();
        consolidationDetails.setSendingAgent(parties);

        ConsolidationDetailsResponse expectedResponse = testConsolResponse;

        ResponseEntity<IRunnerResponse> expectedEntity = ResponseHelper.buildSuccessResponse(expectedResponse);

        var spyService = Mockito.spy(consolidationService);

        OrgAddressResponse orgAddressResponse = OrgAddressResponse.builder().build();

        Map<String, Map<String, Object>> addressMap = new HashMap<>();
        Map<String, Object> map = new HashMap<>();
        map.put("RegulatedAgent", true);
        addressMap.put("o1#c1", map);
        orgAddressResponse.setAddresses(addressMap);

//        when(jsonHelper.convertValue(copy, ConsolidationDetails.class)).thenReturn(consolidationDetails);
        when(v1ServiceUtil.fetchOrgInfoFromV1(any())).thenReturn(orgAddressResponse);
        mockTenantSettings();
        String errorMessage = "Screening Status and Security Status is mandatory for RA consignor.";
        Exception e = assertThrows(RunnerException.class, () -> {
            spyService.validateRaKcForConsol(consolidationDetails);
        });

        assertEquals(errorMessage, e.getMessage());
    }
    @Test
    void testCheckSciForDetachConsole_Success() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        Awb hawb = Awb.builder().consolidationId(2L).awbCargoInfo(AwbCargoInfo.builder().sci("T2").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(awbDao.findByShipmentIdList(List.of(2L))).thenReturn(List.of(hawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(awbDao, times(1)).save(any());
    }
    @Test
    void testCheckSciForDetachConsole_ConsoleEmpty() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());
        consolidationService.checkSciForDetachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }
    @Test
    void testCheckSciForDetachConsole_mawbNull() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(null);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }
    @Test
    void testCheckSciForDetachConsole_mawbEmpty() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of());
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }
    @Test
    void testCheckSciForDetachConsole_awb_null() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(awbDao.findByShipmentIdList(List.of(2L))).thenReturn(null);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(awbDao, times(1)).findByShipmentIdList(any());
    }
    @Test
    void testCheckSciForDetachConsole_awb_empty() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(awbDao.findByShipmentIdList(List.of(2L))).thenReturn(new ArrayList<>());
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(awbDao, times(1)).findByShipmentIdList(any());
    }
    @Test
    void testCheckSciForDetachConsole_isShipSciT1_empty() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        Awb hawb = Awb.builder().consolidationId(2L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(awbDao.findByShipmentIdList(List.of(2L))).thenReturn(List.of(hawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(awbDao, times(1)).findByShipmentIdList(any());
    }

    @Test
    void testCheckSciForDetachConsole_FSU_Locked() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).airMessageStatus(AwbStatus.AWB_FSU_LOCKED).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }
    @Test
    void testCheckSciForDetachConsole_mawb_sciT2() throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci("T2").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }

    @Test
    void testCheckSciForDetachConsole_Success1() throws RunnerException {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(new ArrayList<>());
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForDetachConsole(1L);
        verify(awbDao, times(1)).save(any());
    }

    @Test
    void testCheckSciForAttachConsole_Success () throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci(null).build()).build();
        Awb hawb = Awb.builder().consolidationId(2L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(awbDao.findByShipmentIdList(List.of(2L))).thenReturn(List.of(hawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForAttachConsole(1L);
        verify(awbDao, times(1)).save(any());
    }

    @Test
    void testCheckSciForAttachConsole_ConsoleEmpty () throws RunnerException {
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.empty());
        consolidationService.checkSciForAttachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }
    @Test
    void testCheckSciForAttachConsole_Mawb_Null () throws RunnerException {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        when(awbDao.findByConsolidationId(1L)).thenReturn(null);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForAttachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }
    @Test
    void testCheckSciForAttachConsole_Mawb_Empty () throws RunnerException {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of());
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForAttachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }
    @Test
    void testCheckSciForAttachConsole_ShipId_Null () throws RunnerException {
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci(null).build()).build();
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForAttachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }
    @Test
    void testCheckSciForAttachConsole_Mawb_FSU_Locked () throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).airMessageStatus(AwbStatus.AWB_FSU_LOCKED).awbCargoInfo(AwbCargoInfo.builder().sci(null).build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForAttachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }

    @Test
    void testCheckSciForAttachConsole_Mawb_SciT1 () throws RunnerException {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = new ArrayList<>();
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setId(1L);
        consoleShipmentMappingList.add(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build());
        Awb mawb = Awb.builder().consolidationId(1L).awbCargoInfo(AwbCargoInfo.builder().sci("T1").build()).build();
        when(consoleShipmentMappingDao.findByConsolidationId(1L)).thenReturn(consoleShipmentMappingList);
        when(awbDao.findByConsolidationId(1L)).thenReturn(List.of(mawb));
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        consolidationService.checkSciForAttachConsole(1L);
        verify(consolidationDetailsDao, times(1)).findById(any());
    }

    @Test
    void checkIfShipmentDateGreaterThanConsole() {
        assertTrue(consolidationService.checkIfShipmentDateGreaterThanConsole(LocalDateTime.now(), LocalDateTime.MIN));
    }

    @Test
    void checkIfShipmentDateGreaterThanConsole_nullShipDate() {
        assertFalse(consolidationService.checkIfShipmentDateGreaterThanConsole(null, LocalDateTime.now()));
    }

    @Test
    void checkIfShipmentDateGreaterThanConsole_null() {
        assertFalse(consolidationService.checkIfShipmentDateGreaterThanConsole(LocalDateTime.now(), null));
    }

    @Test
    void retrieveByMeasurmentBasisTest() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();
        Containers containers = new Containers();
        containers.setContainerCode("20GP");
        containers.setContainerCount(1L);
        Packing packing = new Packing();
        packing.setPacks("1");
        packing.setPacksType("MPK");
        consolidationDetails.setPackingList(Arrays.asList(packing));
        consolidationDetails.setContainersList(Arrays.asList(containers));
        consolidationDetails.setAllocations(new Allocations());

        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
        when(modelMapper.map(any(), any())).thenReturn(MeasurementBasisResponse.builder().build());

        ResponseEntity<IRunnerResponse> httpResponse = consolidationService.consolidationRetrieveWithMeasurmentBasis(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void retrieveByMeasurmentBasisTestWithIdNull() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ResponseEntity<IRunnerResponse> httpResponse = consolidationService.consolidationRetrieveWithMeasurmentBasis(commonRequestModel);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void retrieveByMeasurmentBasisTest3() {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid(UUID.randomUUID().toString()).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(commonGetRequest).build();
        ConsolidationDetails consolidationDetails = ConsolidationDetails.builder().build();

        when(consolidationDetailsDao.findByGuid(any())).thenReturn(Optional.of(consolidationDetails));
        ResponseEntity<IRunnerResponse> httpResponse = consolidationService.consolidationRetrieveWithMeasurmentBasis(commonRequestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testGetPendingNotificationsSuccess() {
        PendingNotificationRequest request = new PendingNotificationRequest();
        request.setConsolidationIdList(List.of(1L,2L));

        ShipmentDetails mockShip1 = jsonTestUtility.getTestShipment();
        mockShip1.setTenantId(1);
        mockShip1.setId(1L);
        var mockShip2 = objectMapperTest.convertValue(mockShip1, ShipmentDetails.class);
        mockShip2.setId(2L);
        var mockShip3 = objectMapperTest.convertValue(mockShip1, ShipmentDetails.class);
        mockShip3.setId(3L);

        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        tenantSettingsResponse.setIsMAWBColoadingEnabled(true);
        tenantSettingsResponse.setIsColoadingMAWBStationEnabled(true);
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);

        List<ConsoleShipmentMapping> mappings = new ArrayList<>();
        mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(1L).consolidationId(1L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(2L).consolidationId(1L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(1L).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).isAttachmentDone(false).build());
        mappings.add(ConsoleShipmentMapping.builder()
            .shipmentId(3L).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).isAttachmentDone(false).build());

        List<ShipmentDetails> shipmentDetailsList = List.of(mockShip1, mockShip2, mockShip3);

        // mocking
        mockTenantSettings();
        when(consoleShipmentMappingDao.findAll(any(), any())).thenReturn(new PageImpl<>(mappings));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(shipmentDetailsList));
        // Test
        var httpResponse = consolidationService.getPendingNotifications(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        var runnerResponse = objectMapperTest.convertValue(httpResponse.getBody(), RunnerResponse.class);
        JavaType javaType = objectMapperTest.getTypeFactory().constructParametricType(PendingNotificationResponse.class, PendingConsolidationActionResponse.class);
        PendingNotificationResponse<PendingConsolidationActionResponse> responseBody = objectMapperTest.convertValue(runnerResponse.getData(), javaType);
        assertEquals(2, responseBody.getNotificationMap().size()); // number of consol with pending notifications
        assertEquals(2, responseBody.getNotificationMap().get(1L).size()); // notification count of consol with id 1L
    }

    @Test
    void testGetPendingNotificationsReturnsEmptyResponseIfTenantSettingsNotEnabled() {
        PendingNotificationRequest request = new PendingNotificationRequest();
        request.setShipmentIdList(List.of(1L,2L));

        V1TenantSettingsResponse tenantSettingsResponse = new V1TenantSettingsResponse();
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettingsResponse);

        // Test
        var httpResponse = consolidationService.getPendingNotifications(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        var responseBody = objectMapperTest.convertValue(httpResponse.getBody(), PendingNotificationResponse.class);
        assertNull(responseBody.getNotificationMap());
    }

    @Test
    void testGetPendingNotificationsReturnsEmptyResponseForEmptyList() {
        PendingNotificationRequest request = new PendingNotificationRequest();
        request.setShipmentIdList(null);

        var httpResponse = consolidationService.getPendingNotifications(CommonRequestModel.buildRequest(request));

        PendingNotificationResponse mockResponse  = new PendingNotificationResponse();

        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);
    }

    @Test
    void sendEmailForPullRequested() throws Exception {
        var spyService = Mockito.spy(consolidationService);
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(shipmentDetails)));
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailForPullRequested(testConsol, List.of(1L), new HashSet<>());
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForDetachShipments() throws Exception {
        var spyService = Mockito.spy(consolidationService);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        spyService.sendEmailForDetachShipments(testConsol, List.of(ShipmentDetails.builder().build()), new HashSet<>(), "remarks");
        verify(commonUtils, times(0)).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void sendEmailForDetachShipments1() throws Exception {
        var spyService = Mockito.spy(consolidationService);
        when(masterDataUtils.withMdc(any())).thenReturn(() -> mockRunnable());
        ShipmentDetails shipmentDetails1 = new ShipmentDetails();
        shipmentDetails1.setTenantId(2);
        spyService.sendEmailForDetachShipments(testConsol, List.of(shipmentDetails1), new HashSet<>(), "remarks");
        verify(commonUtils).sendEmailForPullPushRequestStatus(any(), any(), any(), any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void attachShipmentsWithAutoRejectionMail() throws RunnerException {
        var spyService = Mockito.spy(consolidationService);
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(testConsol));
        when(shipmentDao.findAll(any(), any()))
                .thenReturn(new PageImpl<>(new ArrayList<>(List.of(ShipmentDetails.builder()
                .carrierDetails(CarrierDetails.builder().build()).build()))));
        when(consoleShipmentMappingDao.findAll(any(), any()))
                .thenReturn(new PageImpl<>(new ArrayList<>(List.of(ConsoleShipmentMapping.builder().consolidationId(1L).shipmentId(2L).build()))));
        when(commonUtils.getCurrentTenantSettings()).thenReturn(V1TenantSettingsResponse.builder().build());
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().build());
        spyService.attachShipments(null, 1L, new ArrayList<>(List.of(1L)));
        verify(consolidationSync).sync(any(), any(), anyBoolean());
    }

    @Test
    void testDGShipment() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        assertThrows(ValidationException.class, () -> {
            consolidationService.getDGShipment(requestModel);
        });
    }

    @Test
    void testDGShipment2() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        assertThrows(ValidationException.class, () -> {
            consolidationService.getDGShipment(requestModel);
        });
    }

    @Test
    void testDGShipment3() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        ConsolidationDetails consolDetails = new ConsolidationDetails();
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolDetails));
        var httpResponse = consolidationService.getDGShipment(requestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testDGShipment4() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        ConsolidationDetails consolDetails = new ConsolidationDetails();
        consolDetails.setShipmentsList(new ArrayList<>());
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolDetails));
        var httpResponse = consolidationService.getDGShipment(requestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testDGShipment5() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        ConsolidationDetails consolDetails = new ConsolidationDetails();
        ShipmentDetails shipment1 = new ShipmentDetails();
        shipment1.setContainsHazardous(false);
        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setContainsHazardous(true);
        consolDetails.setShipmentsList(Arrays.asList(shipment1, shipment2));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(consolDetails));
        var httpResponse = consolidationService.getDGShipment(requestModel);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testCheckContainerEditingRequiredForOceanDg() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        var response = consolidationService.checkContainerEditingRequiredForOceanDg(requestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testCheckContainerEditingRequiredForOceanDg2() throws RunnerException {
        var response = consolidationService.checkContainerEditingRequiredForOceanDg(CommonRequestModel.builder().build());
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testCheckContainerEditingRequiredForOceanDg3() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        var response = consolidationService.checkContainerEditingRequiredForOceanDg(requestModel);
        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    }

    @Test
    void testCheckContainerEditingRequiredForOceanDg4() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        var response = consolidationService.checkContainerEditingRequiredForOceanDg(requestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testCheckContainerEditingRequiredForOceanDg5() throws RunnerException {
        CommonGetRequest getRequest = CommonGetRequest.builder().id(1L).build();
        CommonRequestModel requestModel = CommonRequestModel.buildRequest(getRequest);
        ConsolidationDetails console = new ConsolidationDetails();
        Containers container = new Containers();
        container.setId(1L);
        Containers container2 = new Containers();
        container2.setId(2L);
        ShipmentDetails shipment1 = new ShipmentDetails();
        ShipmentDetails shipment2 = new ShipmentDetails();
        shipment2.setContainsHazardous(true);
        shipment2.setOceanDGStatus(OceanDGStatus.OCEAN_DG_REQUESTED);
        ShipmentDetails shipment3 = new ShipmentDetails();
        shipment3.setContainsHazardous(true);
        shipment3.setOceanDGStatus(OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED);
        ShipmentDetails shipment4 = new ShipmentDetails();
        shipment4.setContainsHazardous(true);
        shipment4.setOceanDGStatus(OceanDGStatus.OCEAN_DG_ACCEPTED);
        container2.setShipmentsList(Arrays.asList(shipment1, shipment2, shipment3, shipment4));
        console.setContainersList(Arrays.asList(container, container2));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(console));
        var response = consolidationService.checkContainerEditingRequiredForOceanDg(requestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    private Runnable mockRunnable() {
        return null;
    }

    @Test
    void testListRequestedConsolidationForShipment() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        var consoleShipMapping = ConsoleShipmentMapping.builder().isAttachmentDone(false).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).build();
        ConsolidationListResponse consolidationListResponse = ConsolidationListResponse.builder().id(2L).build();
        TenantSettingsDetailsContext.getCurrentTenantSettings().setIsMAWBColoadingEnabled(true);
        mockTenantSettings();
        var spyService = Mockito.spy(consolidationService);
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(Collections.singletonList(consoleShipMapping));
        when(spyService.list(any())).thenReturn(ResponseHelper.buildListSuccessResponse(
                List.of(consolidationListResponse),
                1, 1));

        var response = spyService.listRequestedConsolidationForShipment(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testListRequestedConsolidationForShipment_EmptyMapping() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        TenantSettingsDetailsContext.getCurrentTenantSettings().setIsMAWBColoadingEnabled(true);
        mockTenantSettings();
        var spyService = Mockito.spy(consolidationService);
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(List.of());
        var response = spyService.listRequestedConsolidationForShipment(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testListRequestedConsolidationForShipment_Error1() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        var consoleShipMapping = ConsoleShipmentMapping.builder().isAttachmentDone(false).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).build();
        TenantSettingsDetailsContext.getCurrentTenantSettings().setIsMAWBColoadingEnabled(true);
        mockTenantSettings();
        var spyService = Mockito.spy(consolidationService);
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(Collections.singletonList(consoleShipMapping));
        when(spyService.list(any())).thenReturn(ResponseEntity.of(Optional.empty()));

        var response = spyService.listRequestedConsolidationForShipment(commonRequestModel);
        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    }

    @Test
    void testListRequestedConsolidationForShipment_Error2() {
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(CommonGetRequest.builder().id(1L).build());
        var consoleShipMapping = ConsoleShipmentMapping.builder().isAttachmentDone(false).consolidationId(2L).requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED).build();
        TenantSettingsDetailsContext.getCurrentTenantSettings().setIsMAWBColoadingEnabled(true);
        mockTenantSettings();
        var spyService = Mockito.spy(consolidationService);
        when(consoleShipmentMappingDao.findByShipmentIdAll(1L)).thenReturn(Collections.singletonList(consoleShipMapping));
        when(spyService.list(any())).thenReturn(ResponseHelper.buildListSuccessResponse(
                List.of(),
                1, 1));

        var response = spyService.listRequestedConsolidationForShipment(commonRequestModel);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testGetAutoAttachConsolidationDetailsThrowsExceptionForExistingPushRequestedShipment() {
        AutoAttachConsolidationRequest request = getAutoAttachConsolidationRequest();
        request.setShipId(1L);

        ConsoleShipmentMapping consoleShipmentMapping1 = ConsoleShipmentMapping.builder()
                .shipmentId(1L)
                .consolidationId(1L)
                .requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED)
                .isAttachmentDone(false)
                .build();

        ConsoleShipmentMapping consoleShipmentMapping2 = ConsoleShipmentMapping.builder()
                .shipmentId(1L)
                .consolidationId(2L)
                .requestedType(ShipmentRequestedType.SHIPMENT_PUSH_REQUESTED)
                .isAttachmentDone(false)
                .build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        when(consoleShipmentMappingDao.findAll(any(), any() )).thenReturn(new PageImpl<>(List.of(consoleShipmentMapping1,consoleShipmentMapping2)));
        Exception e = assertThrows(RuntimeException.class, () -> {
           consolidationService.getAutoAttachConsolidationDetails(commonRequestModel);
        });
        assertEquals(ConsolidationConstants.PUSH_REQUESTED_SHIPMENT_VALIDATION_MESSAGE, e.getMessage());
    }

}