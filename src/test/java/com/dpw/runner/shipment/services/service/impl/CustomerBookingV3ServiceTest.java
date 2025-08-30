package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.OrderManagementAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IFusionServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.platformBooking.*;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingV3Response;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.MasterDataKeyUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.CustomerBookingV3Util;
import com.dpw.runner.shipment.services.utils.v3.NpmContractV3Util;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.slf4j.MDC;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
import static com.dpw.runner.shipment.services.commons.constants.DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CustomerBookingV3ServiceTest extends CommonMocks {

    @InjectMocks
    private CustomerBookingV3Service customerBookingService;
    @Mock
    private ICustomerBookingDao customerBookingDao;
    @Mock
    private IReferenceNumbersDao referenceNumbersDao;
    @Mock
    private BillingServiceUrlConfig billingServiceUrlConfig;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private BookingIntegrationsUtility bookingIntegrationsUtility;
    @Mock
    private IPackingDao packingDao;
    @Mock
    private IRoutingsDao routingsDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private IPartiesDao partiesDao;
    @Mock
    private IAuditLogService auditLogService;
    @Mock
    private INPMServiceAdapter npmService;
    @Mock
    private IV1Service v1Service;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private IEventDao eventDao;
    @Mock
    private IMDMServiceAdapter mdmServiceAdapter;
    @Mock
    private IBookingChargesDao bookingChargesDao;
    @Mock
    private IFusionServiceAdapter fusionServiceAdapter;
    @Mock
    private ConsolidationV3Service consolidationService;
    @Mock
    private NpmContractV3Util npmContractV3Util;
    @Mock
    private KafkaProducer producer;
    @Mock
    private OrderManagementAdapter orderManagementAdapter;
    @Mock
    private IQuoteContractsService quoteContractsService;
    @Mock
    private DependentServiceHelper dependentServiceHelper;
    @Mock
    private MasterDataKeyUtils masterDataKeyUtils;
    @Mock
    private CargoService cargoService;
    @Mock
    private CustomerBookingV3Util customerBookingV3Util;

    @Mock
    private INotesDao notesDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static CustomerBookingV3Request customerBookingRequest;
    private static CustomerBooking customerBooking;
    private static Parties testParties;


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
        MDC.setContextMap(new HashMap<>());
        customerBookingRequest = jsonTestUtility.getCustomerBookingV3Request();
        customerBooking = jsonTestUtility.getCustomerBooking();
        testParties = jsonTestUtility.getParties();
        customerBookingService.executorService = Executors.newFixedThreadPool(2);
        customerBookingService.executorServiceMasterData = Executors.newFixedThreadPool(2);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).FetchRatesMandate(Boolean.FALSE).ShipmentServiceV2Enabled(Boolean.TRUE).countryAirCargoSecurity(Boolean.TRUE).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().hasNoUtilization(true).isAlwaysUtilization(Boolean.TRUE).build());
    }

    @AfterEach
    void tearDown() {
        customerBookingService.executorService.shutdown();
        customerBookingService.executorServiceMasterData.shutdown();
    }

    @Test
    void testCreateV3WithPendingForCreditLimitStatus_with_kafka_events() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        mockCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        mockCustomerBooking.setBookingCharges(List.of(new BookingCharges()));
        CustomerBookingV3Response customerBookingV3Response = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        lenient().doNothing().when(bookingIntegrationsUtility).createBookingInPlatform(any());
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(mockCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingV3Response);
        doNothing().when(dependentServiceHelper).pushToKafkaForDownStream(any(), any());
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();
        mockTenantSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(customerBookingV3Response, actualResponse);
        verify(dependentServiceHelper).pushToKafkaForDownStream(any(), any());
    }

    @Test
    void testCreateV3() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        Packing packing = new Packing();
        packing.setWeight(BigDecimal.ONE);
        packing.setWeightUnit("KG");
        Containers containers = new Containers();
        containers.setContainerCode("20FR");
        containers.setContainerCount(1L);
        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .transportType("SEA")
                .build();
        mockCustomerBooking.setId(1L);
        mockCustomerBooking.setPackingList(List.of(packing));
        mockCustomerBooking.setContainersList(List.of(containers));
        CustomerBookingV3Response customerBookingV3Response = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(new CustomerBooking());
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingV3Response);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(customerBookingV3Response, actualResponse);
    }

    @Test
    void testCreateV3WithOrderManagementId() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        request.setOrderManagementId("1212");

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingV3Response = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(customerBookingDao.findByOrderManagementId(any())).thenReturn(Optional.of(new CustomerBooking()));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(new CustomerBooking());
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingV3Response);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(customerBookingV3Response, actualResponse);
    }

    @Test
    void testCreateUpdatesNpmContractV3() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .contractId("contract 1")
                .containersList(List.of(new Containers()))
                .build();

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .contractId("contract 1")
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        verify(npmService, times(1)).updateContracts(any());
        assertEquals(customerBookingResponse, actualResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberFCLCargoV3() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(false).setIsUtilizationForContainerQuoted(true).setHasNoUtilization(false);
        List<Containers> containers = List.of(Containers.builder().build());

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("FCL")
                .contractId("contract 2")
                .containersList(containers)
                .build();

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("FCL")
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBFC-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(quoteContractsService.getQuoteContractsByContractId(anyString())).thenReturn(QuoteContracts.builder().build());
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(customerBookingResponse, actualResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberFTLCargoV3() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(false).setIsUtilizationForContainerQuoted(true).setHasNoUtilization(false);
        List<Containers> containers = List.of(Containers.builder().build());

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("FTL")
                .contractId("contract 2")
                .containersList(containers)
                .build();

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("FTL")
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBFC-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(quoteContractsService.getQuoteContractsByContractId(anyString())).thenReturn(null);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(actualResponse, customerBookingResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberLCLCargoV3() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(false).setIsUtilizationForContainerQuoted(true).setHasNoUtilization(false);
        List<Containers> containers = List.of(Containers.builder().containerCode("20GP").build());
        QuoteContracts quoteContracts = QuoteContracts.builder().contractId("contract 2").containerTypes(List.of("20GP", "40GP")).build();

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("LCL")
                .contractId("contract 2")
                .containersList(containers)
                .build();
        Packing packing = new Packing();
        packing.setWeight(BigDecimal.ONE);
        packing.setWeightUnit("KG");
        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("LCL")
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBLC-random-string")
                .containersList(containers)
                .packingList(List.of(packing))
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(quoteContractsService.getQuoteContractsByContractId(anyString())).thenReturn(quoteContracts);
        when(npmService.updateContracts(any())).thenReturn(new ResponseEntity<>(HttpStatus.OK));
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(actualResponse, customerBookingResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberBBKCargoV3() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(false).setIsUtilizationForContainerQuoted(true).setHasNoUtilization(false);
        List<Containers> containers = List.of(Containers.builder().containerCode("20GP").build());
        QuoteContracts quoteContracts = QuoteContracts.builder().contractId("contract 2").containerTypes(List.of("40GP")).build();

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("BBK")
                .contractId("contract 2")
                .containersList(containers)
                .build();

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("BBK")
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBLC-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(quoteContractsService.getQuoteContractsByContractId(anyString())).thenReturn(quoteContracts);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(actualResponse, customerBookingResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberRORCargoV3() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(false).setIsUtilizationForContainerQuoted(false).setHasNoUtilization(true);

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("ROR")
                .build();

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("ROR")
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBLC-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(actualResponse, customerBookingResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberLTLCargoV3() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(null).setIsUtilizationForContainerQuoted(null).setHasNoUtilization(true);

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("LTL")
                .containersList(null)
                .build();

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("LTL")
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBLC-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(actualResponse, customerBookingResponse);
    }

    @Test
    void testPlatformCreateBookingV3() throws RunnerException {
        PlatformToRunnerCustomerBookingRequest request = new PlatformToRunnerCustomerBookingRequest();
        var container = BookingContainerRequest.builder().containerCode("20FR").containerCount(12L).commodityGroup("FAK").build();
        container.setGuid(UUID.randomUUID());
        var addressDataMap = customerBooking.getCustomer().getAddressData();
        var orgDataMap = customerBooking.getCustomer().getOrgData();
        addressDataMap.put(PartiesConstants.ADDRESS2, "delhi");
        addressDataMap.put(PartiesConstants.CITY, "New Delhi");
        addressDataMap.put(PartiesConstants.STATE, "New Delhi");
        addressDataMap.put(PartiesConstants.ZIP_POST_CODE, "110011");
        addressDataMap.put(PartiesConstants.MOBILE, "9871413293");
        addressDataMap.put(PartiesConstants.PHONE, "01198765432");
        var containersList = List.of(
                BookingContainerRequest.builder().containerCode("20GP").containerCount(1L).commodityGroup("FAK").runner_guid(UUID.randomUUID()).build(),
                BookingContainerRequest.builder().containerCode("20FR").containerCount(12L).commodityGroup("FAK").build(),
                container
        );
        var pack = BookingPackingRequest.builder().dimensionUnit("M").isDimension(true).commodityGroup("FAK").build();
        pack.setGuid(UUID.randomUUID());
        var packingList = List.of(
                BookingPackingRequest.builder().dimensionUnit("M").isDimension(true).commodityGroup("FAK").build(),
                BookingPackingRequest.builder().dimensionUnit("M").isDimension(false).commodityGroup("FAK").build(),
                pack
        );
        var route = BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("UIHJK").build();
        route.setGuid(UUID.randomUUID());
        var routingList = List.of(
                BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("APLU").build(),
                route
        );
        var chargesList = List.of(
                PlatformBookingChargesRequest.builder()
                        .creditor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build())
                        .debtor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build())
                        .build()
        );
        request.setContainersList(containersList);
        request.setPackingList(packingList);
        request.setRoutingList(routingList);
        request.setBookingCharges(chargesList);
        request.setVessel("Vessel");
        request.setIsSingleUsageContract(true);
        request.setIsConsignorFreeText(true);
        request.setIsConsigneeFreeText(true);
        request.setIsNotifyPartyFreeText(true);
        request.setCustomer(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").build());
        request.setConsignor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setConsignee(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setNotifyParty(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setShippingLine("code");
        request.setBookingNumber("DBAR-5586091-311749");

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBookingV3Request customerBookingRequest = new CustomerBookingV3Request();
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setId(1L);
        customerBooking.setBookingNumber("DBAR-5586091-311749");
        customerBooking.setIsConsigneeAddressFreeText(true);
        customerBooking.setIsConsignorAddressFreeText(true);
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(true);
        customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);

        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingV3Response.class);

        // Mock
        when(customerBookingDao.findByBookingNumber(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), eq(CustomerBookingV3Request.class))).thenReturn(customerBookingRequest);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().entities(List.of()).build());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(jsonHelper.convertValueToList(any(), eq(VesselsResponse.class))).thenReturn(List.of(new VesselsResponse()));
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(Arrays.asList(mdmContainerTypeResponse));

        // Test
        PlatformToRunnerCustomerBookingResponse response = customerBookingService.platformCreateBooking(request);

        // Assert
        verify(customerBookingDao, times(1)).save(customerBooking);
        assertNotNull(response);
        assertEquals("DBAR-5586091-311749", response.getBookingNumber());
        assertEquals("SINGLE_USAGE", request.getContractStatus());

        if (response.getCharges() != null && !response.getCharges().isEmpty()) {
            assertNotNull(response.getCharges().get(0).getGuid());
        }
    }

    @Test
    void testPlatformCreateBookingV3_with_B2B_Source() throws RunnerException {
        PlatformToRunnerCustomerBookingRequest request = new PlatformToRunnerCustomerBookingRequest();
        request.setSource(BookingSource.B2B);
        var container = BookingContainerRequest.builder().containerCode("20FR").containerCount(12L).commodityGroup("FAK").build();
        container.setGuid(UUID.randomUUID());
        var addressDataMap = customerBooking.getCustomer().getAddressData();
        var orgDataMap = customerBooking.getCustomer().getOrgData();
        addressDataMap.put(PartiesConstants.ADDRESS2, "delhi");
        addressDataMap.put(PartiesConstants.CITY, "New Delhi");
        addressDataMap.put(PartiesConstants.STATE, "New Delhi");
        addressDataMap.put(PartiesConstants.ZIP_POST_CODE, "110011");
        addressDataMap.put(PartiesConstants.MOBILE, "9871413293");
        addressDataMap.put(PartiesConstants.PHONE, "01198765432");
        var containersList = List.of(
                BookingContainerRequest.builder().containerCode("20GP").containerCount(1L).commodityGroup("FAK").runner_guid(UUID.randomUUID()).build(),
                BookingContainerRequest.builder().containerCode("20FR").containerCount(12L).commodityGroup("FAK").build(),
                container
        );
        var pack = BookingPackingRequest.builder().dimensionUnit("M").isDimension(true).commodityGroup("FAK").build();
        pack.setGuid(UUID.randomUUID());
        var packingList = List.of(
                BookingPackingRequest.builder().dimensionUnit("M").isDimension(true).commodityGroup("FAK").build(),
                BookingPackingRequest.builder().dimensionUnit("M").isDimension(false).commodityGroup("FAK").build(),
                pack
        );
        var route = BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("UIHJK").build();
        route.setGuid(UUID.randomUUID());
        var routingList = List.of(
                BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("APLU").build(),
                route
        );
        var chargesList = List.of(
                PlatformBookingChargesRequest.builder()
                        .creditor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build())
                        .debtor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build())
                        .build()
        );
        request.setContainersList(containersList);
        request.setPackingList(packingList);
        request.setRoutingList(routingList);
        request.setBookingCharges(chargesList);
        request.setVessel("Vessel");
        request.setIsSingleUsageContract(true);
        request.setIsConsignorFreeText(true);
        request.setIsConsigneeFreeText(true);
        request.setIsNotifyPartyFreeText(true);
        request.setCustomer(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").build());
        request.setConsignor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setConsignee(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setNotifyParty(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setShippingLine("code");
        request.setBookingNumber("DBAR-5586091-311749");

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBookingV3Request customerBookingRequest = new CustomerBookingV3Request();
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setId(1L);
        customerBooking.setBookingNumber("DBAR-5586091-311749");
        customerBooking.setIsConsigneeAddressFreeText(true);
        customerBooking.setIsConsignorAddressFreeText(true);
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(true);
        customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);

        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingV3Response.class);

        // Mock
        when(customerBookingDao.findByBookingNumber(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), eq(CustomerBookingV3Request.class))).thenReturn(customerBookingRequest);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().entities(List.of()).build());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(jsonHelper.convertValueToList(any(), eq(VesselsResponse.class))).thenReturn(List.of(new VesselsResponse()));
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(Arrays.asList(mdmContainerTypeResponse));

        // Test
        PlatformToRunnerCustomerBookingResponse response = customerBookingService.platformCreateBooking(request);

        // Assert
        verify(customerBookingDao, times(1)).save(customerBooking);
        assertNotNull(response);
        assertEquals("DBAR-5586091-311749", response.getBookingNumber());
        assertEquals("SINGLE_USAGE", request.getContractStatus());

        if (response.getCharges() != null && !response.getCharges().isEmpty()) {
            assertNotNull(response.getCharges().get(0).getGuid());
        }
    }

    @Test
    void testV3PlatformCreateBooking2() throws RunnerException {
        PlatformToRunnerCustomerBookingRequest request = new PlatformToRunnerCustomerBookingRequest();

        var route = BookingRoutingsRequest.builder()
                .reference_id(UUID.randomUUID().toString())
                .carrier("UIHJK")
                .build();
        route.setGuid(UUID.randomUUID());

        var routingList = List.of(
                BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("APLU").build(),
                route
        );

        request.setBookingNumber("DBAR-5586091-311749"); // Important: must match mocked booking
        request.setContainersList(List.of());
        request.setPackingList(List.of());
        request.setRoutingList(routingList);
        request.setBookingCharges(List.of());

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setId(1L);
        customerBooking.setBookingNumber("DBAR-5586091-311749");
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsConsigneeAddressFreeText(false);
        customerBooking.setIsConsignorAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(false);
        customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);
        customerBooking.setCarrierDetails(CarrierDetails.builder().build());

        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingV3Response.class);

        // Mocking
        when(customerBookingDao.findByBookingNumber(any())).thenReturn(Optional.of(customerBooking));
        when(modelMapper.map(any(), eq(CustomerBookingV3Request.class))).thenReturn(new CustomerBookingV3Request());
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));

        // Test
        PlatformToRunnerCustomerBookingResponse response = customerBookingService.platformCreateBooking(request);

        // Assert
        verify(customerBookingDao, times(1)).save(customerBooking);
        assertNotNull(response);
        assertEquals("DBAR-5586091-311749", response.getBookingNumber());
        assertTrue(response.getCharges().isEmpty());
    }

    @Test
    void testV3PlatformCreateBookingForTeslaIntegration() throws RunnerException {
        PlatformToRunnerCustomerBookingRequest request = new PlatformToRunnerCustomerBookingRequest();
        request.setBookingNumber("BookingNumber");
        var container = BookingContainerRequest.builder().containerCode("20FR").containerCount(12L).commodityGroup("FAK").build();
        container.setGuid(UUID.randomUUID());
        var addressDataMap = customerBooking.getCustomer().getAddressData();
        var orgDataMap = customerBooking.getCustomer().getOrgData();
        addressDataMap.put(PartiesConstants.ADDRESS2, "delhi");
        addressDataMap.put(PartiesConstants.CITY, "New Delhi");
        addressDataMap.put(PartiesConstants.STATE, "New Delhi");
        addressDataMap.put(PartiesConstants.ZIP_POST_CODE, "110011");
        addressDataMap.put(PartiesConstants.MOBILE, "9871413293");
        addressDataMap.put(PartiesConstants.PHONE, "01198765432");

        // Containers, packing, routing, and charges list setup
        var containersList = List.of(
                BookingContainerRequest.builder().containerCode("20GP").containerCount(1L).commodityGroup("FAK").runner_guid(UUID.randomUUID()).build(),
                BookingContainerRequest.builder().containerCode("20FR").containerCount(12L).commodityGroup("FAK").build(),
                container
        );

        var pack = BookingPackingRequest.builder().dimensionUnit("M").isDimension(true).commodityGroup("FAK").build();
        pack.setGuid(UUID.randomUUID());
        var packingList = List.of(
                BookingPackingRequest.builder().dimensionUnit("M").isDimension(true).commodityGroup("FAK").build(),
                BookingPackingRequest.builder().dimensionUnit("M").isDimension(false).commodityGroup("FAK").build(),
                pack
        );

        var route = BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("UIHJK").build();
        route.setGuid(UUID.randomUUID());
        var routingList = List.of(
                BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("APLU").build(),
                route
        );

        var chargesList = List.of(
                PlatformBookingChargesRequest.builder()
                        .creditor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build())
                        .debtor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build())
                        .build()
        );
        List<NotesRequest> notesList = List.of(NotesRequest.builder().label("ShipmentReference").text("SH01231-12313").build());

        request.setIntegrationSource(Constants.TESLA);
        request.setContainersList(containersList);
        request.setPackingList(packingList);
        request.setRoutingList(routingList);
        request.setBookingCharges(chargesList);
        request.setVessel("Vessel");
        request.setIsSingleUsageContract(true);
        request.setIsConsignorFreeText(true);
        request.setIsConsigneeFreeText(true);
        request.setIsNotifyPartyFreeText(true);
        request.setCustomer(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").build());
        request.setConsignor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setConsignee(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setNotifyParty(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setShippingLine("code");
        request.setNotesList(notesList);

        CustomerBookingV3Request customerBookingRequest = new CustomerBookingV3Request();
        customerBookingRequest.setIntegrationSource(Constants.TESLA);

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setId(1L);
        customerBooking.setBookingNumber("DBAR-5586091-311749");
        customerBooking.setIsConsigneeAddressFreeText(customerBooking.getIsConsigneeFreeText() != null && customerBooking.getIsConsigneeFreeText());
        customerBooking.setIsConsignorAddressFreeText(customerBooking.getIsConsignorFreeText() != null && customerBooking.getIsConsignorFreeText());
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(customerBooking.getIsNotifyPartyFreeText() != null && customerBooking.getIsNotifyPartyFreeText());
        customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);

        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingV3Response.class);

        // Mocking
        when(customerBookingDao.findByShipmentReferenceNumber(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), eq(CustomerBookingV3Request.class))).thenReturn(customerBookingRequest);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().entities(List.of()).build());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(jsonHelper.convertValueToList(any(), eq(VesselsResponse.class))).thenReturn(List.of(new VesselsResponse()));
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(Arrays.asList(mdmContainerTypeResponse));

        // Test the platformCreateBooking method
        PlatformToRunnerCustomerBookingResponse platformResponse = customerBookingService.platformCreateBooking(request);

        // Assertions
        verify(customerBookingDao, times(1)).save(customerBooking);
        assertNotNull(platformResponse.getBookingNumber());
    }

    @Test
    void testV3CreateGeneratesBookingNumberForNullRequest() throws RunnerException {
        // Test
        Throwable t = assertThrows(Throwable.class, () -> customerBookingService.create(null));
        // Assert
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testV3CreateGeneratesBookingNumberForCreateCompleteRequest() throws RunnerException {
        CustomerBookingV3Request request = customerBookingRequest;
        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(request, CustomerBookingV3Response.class);
        CustomerBooking customerBooking = objectMapper.convertValue(request, CustomerBooking.class);
        var bookingCharge = customerBooking.getBookingCharges().get(0);
        Packing packing = new Packing();
        packing.setBookingId(1L);
        packing.setVolume(BigDecimal.TEN);
        packing.setVolumeUnit("M3");
        packing.setWeight(BigDecimal.TEN);
        packing.setWeightUnit("KG");
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(jsonHelper.convertValue(any(), eq(BookingCharges.class))).thenReturn(bookingCharge);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        VolumeWeightChargeable volumeWeightChargeable = new VolumeWeightChargeable();
        volumeWeightChargeable.setChargeable(BigDecimal.ONE);
        mockShipmentSettings();
        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertEquals(actualResponse, customerBookingResponse);
    }


    @Test
    void testV3Delete() throws RunnerException {
        // Mock
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(customerBooking));
        doNothing().when(customerBookingDao).delete(any());
        CustomerBookingV3DeleteResponse customerBookingV3DeleteResponse = customerBookingService.delete(1L);
        // Assert
        assertNotNull(customerBookingV3DeleteResponse);
    }

    @Test
    void testDeleteBookingIdIsNull() {
        Long bookingId = null; // Null booking ID

        // Test the behavior when bookingId is null
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            customerBookingService.delete(bookingId); // Call the delete method with null bookingId
        });

        // Assert that the exception message is as expected
        assertEquals("Failed to delete Booking null !", exception.getMessage());
    }

    @Test
    void testV3Delete2() throws RunnerException {
        // Mock
        when(customerBookingDao.findById(anyLong())).thenThrow(new RuntimeException());

        // Call the delete method with bookingId = 2L
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            customerBookingService.delete(2L);
        });

        // Assert that the message is as expected
        assertEquals("Failed to delete Booking 2 !", exception.getMessage());
    }


    @Test
    void testDelete6() throws RunnerException {
        // Mock
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.empty());
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            customerBookingService.delete(2L);
        });

        // Assert that the message is as expected
        assertEquals("Failed to delete Booking 2 !", exception.getMessage());
    }

    @Test
    void testCreateWithException() throws RunnerException {
        when(customerBookingDao.save(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();
        // Test
        Throwable t = assertThrows(Throwable.class, () -> customerBookingService.create(customerBookingRequest));
        // Assert
        assertEquals("RuntimeException", t.getMessage());
        assertEquals(RunnerException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testCreateWithNullException() throws RunnerException {
        // Test
        Throwable t = assertThrows(Throwable.class, () -> customerBookingService.create(null));
        // Assert
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testListWithNullRequest_throwsDataRetrievalFailureException() {
        // Test & Assert
        Throwable t = assertThrows(RunnerException.class, () -> customerBookingService.list(null, Boolean.TRUE));
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testListWithEmptyException() throws RunnerException {
        // Test
        Throwable t = assertThrows(RunnerException.class, () -> customerBookingService.list(null, Boolean.TRUE));
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testListWithNoResultWhenFindAllThrowsException() throws RunnerException {
        when(customerBookingDao.findAll(any(), any())).thenThrow(new RuntimeException());
        // Test
        Throwable t = assertThrows(RunnerException.class, () -> customerBookingService.list(new ListCommonRequest(), Boolean.TRUE));
        assertEquals(DAO_GENERIC_LIST_EXCEPTION_MSG, t.getMessage());
    }

    @Test
    void testListWithNoResult() throws RunnerException {
        when(customerBookingDao.findAll(any(), any())).thenReturn(Page.empty());
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));
        doNothing().when(masterDataUtils).fetchCarriersForList(anyList());
        doNothing().when(masterDataUtils).setLocationData(anyList(), anyString());
        // Test
        CustomerBookingV3ListResponse response = customerBookingService.list(new ListCommonRequest(), Boolean.TRUE);
        // Assert
        assertNotNull(response);
        assertTrue(response.getCustomerBookingV3Responses().isEmpty());
    }

    @Test
    void testListWithSuccessResult() throws RunnerException {
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setId(2L);
        when(customerBookingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(customerBooking)));
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));
        doNothing().when(masterDataUtils).fetchCarriersForList(anyList());
        doNothing().when(masterDataUtils).setLocationData(anyList(), anyString());
        // Test
        CustomerBookingV3ListResponse response = customerBookingService.list(new ListCommonRequest(), Boolean.TRUE);
        // Assert
        assertNotNull(response);
        assertFalse(response.getCustomerBookingV3Responses().isEmpty());
    }

    @Test
    void testRetrieveByIdWithEmptyRequest() throws RunnerException {
        Throwable t = assertThrows(RunnerException.class, () -> customerBookingService.retrieveById(null));
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testRetrieveByIdWithEmptyIdAndGuidRequest() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().build();
        Throwable t = assertThrows(RunnerException.class, () -> customerBookingService.retrieveById(commonGetRequest));
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testRetrieveByIdWithBookingIdNotPresent() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().id(123L).build();
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.empty());
        Throwable t = assertThrows(RunnerException.class, () -> customerBookingService.retrieveById(commonGetRequest));
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, t.getMessage());
    }

    @Test
    void testRetrieveByIdWithExceptionInFindById() throws RunnerException {
        when(customerBookingDao.findById(anyLong())).thenThrow(new RuntimeException());
        Throwable t = assertThrows(RunnerException.class, () -> customerBookingService.retrieveById(CommonGetRequest.builder().id(123L).build()));
        assertEquals(DAO_GENERIC_RETRIEVE_EXCEPTION_MSG, t.getMessage());
    }

    @Test
    void testRetrieveByIdWithSuccessResponse() throws RunnerException {
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(customerBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(objectMapper.convertValue(customerBooking, CustomerBookingV3Response.class));
        CustomerBookingV3Response response = customerBookingService.retrieveById(CommonGetRequest.builder().id(123L).build());
        assertNotNull(response);
        assertEquals(202861, response.getId());
        assertEquals(BookingStatus.PENDING_FOR_KYC, response.getBookingStatus());
    }

    @Test
    void testRetrieveByIdSuccessWithEmptyIdRequest() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("471ca9f1-e08c-4ee9-a25e-7fcc0b90d68b").build();
        when(customerBookingDao.findByGuid(any())).thenReturn(Optional.of(customerBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(objectMapper.convertValue(customerBooking, CustomerBookingV3Response.class));
        CustomerBookingV3Response response =  customerBookingService.retrieveById(commonGetRequest);
        assertNotNull(response);
        assertEquals(202861,response.getId());
        assertEquals( BookingStatus.PENDING_FOR_KYC, response.getBookingStatus());
    }

    @Test
    void testRetrieveByOrderIdWithSuccessResponse() throws RunnerException {
        when(orderManagementAdapter.getOrderForBookingV3(any())).thenReturn(objectMapper.convertValue(customerBooking, CustomerBookingV3Response.class));
        CustomerBookingV3Response response = customerBookingService.retrieveByOrderId("12312");
        assertNotNull(response);
        assertEquals(202861, response.getId());
        assertEquals(BookingStatus.PENDING_FOR_KYC, response.getBookingStatus());
    }

    @Test
    void testRetrieveByOrderIdThrowsException() throws RunnerException {
        when(orderManagementAdapter.getOrderForBookingV3(any())).thenThrow(new RuntimeException("RunnerException"));
        Throwable t = assertThrows(RunnerException.class, () -> customerBookingService.retrieveByOrderId("12312"));
        assertEquals("RunnerException", t.getMessage());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2OnWithShipmentGuidNotNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(true).build());
        var inputBooking = customerBooking;
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentGuid(UUID.randomUUID().toString());
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingV3Response.class));
        mockTenantSettings();
        CustomerBookingV3Response response = customerBookingService.retrieveById(CommonGetRequest.builder().id(123L).build());
        assertNotNull(response);
        assertEquals(202861, response.getId());
        assertEquals(BookingStatus.READY_FOR_SHIPMENT, response.getBookingStatus());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2OnWithShipmentGuidNotNull2() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(true).build());
        var inputBooking = customerBooking;
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentGuid(UUID.randomUUID().toString());
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingV3Response.class));
        mockTenantSettings();
        CustomerBookingV3Response response = customerBookingService.retrieveById(CommonGetRequest.builder().id(123L).build());
        assertEquals(202861, response.getId());
        assertEquals( BookingStatus.READY_FOR_SHIPMENT, response.getBookingStatus());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2OnWithShipmentGuidNull() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(true).build());
        var inputBooking = customerBooking;
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentEntityId("123");
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());
        when(v1Service.getShipment(any())).thenReturn(V1RetrieveResponse.builder().entity(ShipmentRetrieveResponse.builder().guid(UUID.randomUUID()).build()).build());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingV3Response.class));
        mockTenantSettings();
        CustomerBookingV3Response response = customerBookingService.retrieveById(CommonGetRequest.builder().id(123L).build());
        assertEquals(202861, response.getId());
        assertEquals( BookingStatus.READY_FOR_SHIPMENT, response.getBookingStatus());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2OnWithShipmentGuidNull2() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(true).build());
        var inputBooking = customerBooking;
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentEntityId("123");
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(v1Service.getShipment(any())).thenReturn(V1RetrieveResponse.builder().entity(ShipmentRetrieveResponse.builder().guid(UUID.randomUUID()).build()).build());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingV3Response.class));
        mockTenantSettings();
        CustomerBookingV3Response response = customerBookingService.retrieveById(CommonGetRequest.builder().id(123L).build());
        assertEquals(202861,response.getId());
        assertEquals( BookingStatus.READY_FOR_SHIPMENT, response.getBookingStatus());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2Off() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(false).build());
        var inputBooking = customerBooking;
        var guid = UUID.randomUUID().toString();
        var mockV1Response = new ShipmentBillingListResponse();
        var dataMap = new HashMap<String, ShipmentBillingListResponse.BillingData>();
        dataMap.put(guid, new ShipmentBillingListResponse.BillingData());
        mockV1Response.setData(dataMap);
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentGuid(guid);
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(v1Service.fetchShipmentBillingData(any())).thenReturn(mockV1Response);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingV3Response.class));
        mockTenantSettings();
        CustomerBookingV3Response response = customerBookingService.retrieveById(CommonGetRequest.builder().id(123L).build());
        assertEquals(202861,response.getId());
        assertEquals(BookingStatus.READY_FOR_SHIPMENT, response.getBookingStatus());
    }

    @Test
    void testCreateWithCompletePayload() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputCustomerBooking = customerBooking;

        var container = Containers.builder().build();
        container.setGuid(UUID.randomUUID());
        inputCustomerBooking.setContainersList(
                Arrays.asList(container)
        );
        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.getBookingCharges().get(0).setContainersUUID(List.of(container.getGuid()));
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(BookingCharges.class))).thenReturn(new BookingCharges());
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getContainersList());
        when(partiesDao.saveEntityFromOtherEntity(anyList(), anyLong(), anyString())).thenReturn(inputCustomerBooking.getAdditionalParties());
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        // Test
        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        // Assert
        assertNotNull(actualResponse);
        assertEquals(customerBookingResponse, actualResponse);
    }

    @Test
    void testV3BookingUpdateNullRequest() {
        // test
        var t = assertThrows(Throwable.class, () -> customerBookingService.update(null));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testV3BookingUpdateNullRequestId() {
        // test
        var t = assertThrows(Throwable.class, () -> customerBookingService.update(new CustomerBookingV3Request()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testV3BookingUpdateWithIdDoesNotExists() {
        CustomerBookingV3Request customerBookingV3Request = new CustomerBookingV3Request();
        customerBookingV3Request.setId(1L);
        // test
        var t = assertThrows(Throwable.class, () -> customerBookingService.update(customerBookingV3Request));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, t.getMessage());
    }


    @Test
    void testV3BookingUpdateWithReadyForShipment() {
        var mockBookingEntity = customerBooking;
        mockBookingEntity.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        // mock
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(mockBookingEntity));
        // test
        var t = assertThrows(Throwable.class, () -> customerBookingService.update(customerBookingRequest));
        // assert
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3BookingUpdateWithSuccess() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("testUser");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setSource(BookingSource.Runner);

        var container = Containers.builder().build();
        inputCustomerBooking.setContainersList(List.of(container));

        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.PENDING_FOR_KYC);

        CustomerBookingV3Response expectedResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);

        // Mocks
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(inputCustomerBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(expectedResponse);
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(List.of(container));
        doThrow(new RuntimeException("Audit Log Exception")).when(auditLogService).addAuditLog(any());
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        // Act
        CustomerBookingV3Response actualResponse = customerBookingService.update(request);

        // Assert
        assertNotNull(actualResponse);
        assertEquals(expectedResponse.getBookingStatus(), actualResponse.getBookingStatus());
    }

    @Test
    void testV3BookingUpdateWithMdmThrowsException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("testUser");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setSource(BookingSource.Runner);

        var container = Containers.builder().build();
        inputCustomerBooking.setContainersList(List.of(container));

        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.PENDING_FOR_KYC);

        CustomerBookingV3Response expectedResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);

        // Mocks
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(inputCustomerBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(mdmServiceAdapter.getContainerTypes()).thenThrow(new RunnerException());
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        mockShipmentSettings();

        // Act
        assertThrows(CompletionException.class, () -> customerBookingService.update(request));
    }

    @Test
    void testV3BookingUpdateWithPendingWithKycStatusSuccess() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("testUser");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setSource(BookingSource.Runner);

        var container = Containers.builder().build();
        inputCustomerBooking.setContainersList(List.of(container));

        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.PENDING_FOR_KYC);

        CustomerBookingV3Response expectedResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);

        // Mocks
        lenient().doNothing().when(bookingIntegrationsUtility).createBookingInPlatform(any());
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(inputCustomerBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(expectedResponse);
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(List.of(container));
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        doThrow(new RuntimeException("Audit Log Exception")).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        mockTenantSettings();

        // Act
        CustomerBookingV3Response actualResponse = customerBookingService.update(request);

        // Assert
        assertNotNull(actualResponse);
        assertEquals(expectedResponse.getBookingStatus(), actualResponse.getBookingStatus());
    }

    @Test
    void testV3BookingUpdateWithReadyForShipmentStatusSuccess() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(1L);
        existingBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        existingBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        existingBooking.setCreatedBy("testUser");
        existingBooking.setTransportType("SEA");
        existingBooking.setIsPlatformBookingCreated(false);
        existingBooking.setSource(BookingSource.Runner);

        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("testUser");
        inputCustomerBooking.setTransportType("SEA");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setSource(BookingSource.Runner);

        var container = Containers.builder().build();
        inputCustomerBooking.setContainersList(List.of(container));

        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);

        CustomerBookingV3Response expectedResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);

        // Mocks
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Approved");
        lenient().doNothing().when(bookingIntegrationsUtility).createBookingInPlatform(any());
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(existingBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(expectedResponse);
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(List.of(container));
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        doThrow(new RuntimeException("Audit Log Exception")).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        mockTenantSettings();

        // Act
        CustomerBookingV3Response actualResponse = customerBookingService.update(request);

        // Assert
        assertNotNull(actualResponse);
        assertEquals(expectedResponse.getBookingStatus(), actualResponse.getBookingStatus());
    }


    @Test
    void testV3BookingUpdateWithReadyForShipmentStatusExceptionWithoutAirSecurityPermission() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(1L);
        existingBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        existingBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        existingBooking.setCreatedBy("testUser");
        existingBooking.setTransportType("AIR");
        existingBooking.setDirection("EXP");
        existingBooking.setIsPlatformBookingCreated(false);
        existingBooking.setSource(BookingSource.Runner);

        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("testUser");
        inputCustomerBooking.setTransportType("AIR");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setDirection("EXP");
        inputCustomerBooking.setSource(BookingSource.Runner);

        var container = Containers.builder().build();
        inputCustomerBooking.setContainersList(List.of(container));

        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);

        CustomerBookingV3Response expectedResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);

        // Mocks
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.AIR_SECURITY_PERMISSION, false));
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Approved");
        lenient().doNothing().when(bookingIntegrationsUtility).createBookingInPlatform(any());
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(existingBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();
        mockTenantSettings();

        // Act
        ValidationException exception = assertThrows(ValidationException.class, () -> customerBookingService.update(request));
        assertEquals("User does not have Air Security permission to create AIR EXP Shipment from Booking.", exception.getMessage());
    }

    @Test
    void testV3BookingUpdateWithReadyForShipmentStatusSuccessWithoutAirCargoSecurity() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(1L);
        existingBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        existingBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        existingBooking.setCreatedBy("testUser");
        existingBooking.setTransportType("SEA");
        existingBooking.setIsPlatformBookingCreated(false);
        existingBooking.setSource(BookingSource.Runner);

        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("testUser");
        inputCustomerBooking.setTransportType("SEA");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setSource(BookingSource.Runner);

        var container = Containers.builder().build();
        inputCustomerBooking.setContainersList(List.of(container));

        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);

        CustomerBookingV3Response expectedResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).FetchRatesMandate(Boolean.FALSE).ShipmentServiceV2Enabled(Boolean.TRUE).countryAirCargoSecurity(Boolean.FALSE).build());
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.AIR_DG, Boolean.TRUE));
        // Mocks
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Approved");
        lenient().doNothing().when(bookingIntegrationsUtility).createBookingInPlatform(any());
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(existingBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(expectedResponse);
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(List.of(container));
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        doThrow(new RuntimeException("Audit Log Exception")).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        mockTenantSettings();

        // Act
        CustomerBookingV3Response actualResponse = customerBookingService.update(request);

        // Assert
        assertNotNull(actualResponse);
        assertEquals(expectedResponse.getBookingStatus(), actualResponse.getBookingStatus());
    }

    @Test
    void testV3BookingUpdateWithReadyForShipmentStatusSuccess_Air_transport_WithoutAirCargoSecurity() throws RunnerException {
        // Arrange
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(1L);
        existingBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        existingBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        existingBooking.setCreatedBy("testUser");
        existingBooking.setTransportType("AIR");
        existingBooking.setIsPlatformBookingCreated(false);
        existingBooking.setSource(BookingSource.Runner);

        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("testUser");
        inputCustomerBooking.setTransportType("AIR");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setSource(BookingSource.Runner);

        var container = Containers.builder().build();
        inputCustomerBooking.setContainersList(List.of(container));

        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        request.setIsDg(Boolean.TRUE);


        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).FetchRatesMandate(Boolean.FALSE).ShipmentServiceV2Enabled(Boolean.TRUE).countryAirCargoSecurity(Boolean.FALSE).build());
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.AIR_DG, Boolean.FALSE));
        // Mocks
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Approved");
        lenient().doNothing().when(bookingIntegrationsUtility).createBookingInPlatform(any());
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(existingBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        mockShipmentSettings();
        mockTenantSettings();

        // Act
         assertDoesNotThrow(() -> customerBookingService.update(request));

    }

    @Test
    void testV3BookingUpdateWithReadyForShipmentStatusSuccessWithoutAirSecurityPermissions() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(1L);
        existingBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        existingBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        existingBooking.setCreatedBy("testUser");
        existingBooking.setTransportType("SEA");
        existingBooking.setIsPlatformBookingCreated(false);
        existingBooking.setSource(BookingSource.Runner);
        existingBooking.setBookingCharges(List.of());

        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("testUser");
        inputCustomerBooking.setTransportType("SEA");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setSource(BookingSource.Runner);
        inputCustomerBooking.setBookingCharges(List.of(new BookingCharges()));

        ShipmentDetailsV3Response shipmentDetailsV3Response = new ShipmentDetailsV3Response();
        shipmentDetailsV3Response.setShipmentId("SHIP12");
        shipmentDetailsV3Response.setId(1L);
        shipmentDetailsV3Response.setGuid(UUID.randomUUID());

        var container = Containers.builder().build();
        inputCustomerBooking.setContainersList(List.of(container));

        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);

        CustomerBookingV3Response expectedResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).FetchRatesMandate(Boolean.FALSE).ShipmentServiceV2Enabled(Boolean.TRUE).countryAirCargoSecurity(Boolean.FALSE).build());
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.AIR_DG, Boolean.TRUE));
        // Mocks
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Approved");
        lenient().doNothing().when(bookingIntegrationsUtility).createBookingInPlatform(any());
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(existingBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(expectedResponse);
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(List.of(container));
        when(bookingIntegrationsUtility.createShipmentInV3(any())).thenReturn(shipmentDetailsV3Response);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        doThrow(new RuntimeException("Audit Log Exception")).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        mockTenantSettings();

        // Act
        CustomerBookingV3Response actualResponse = customerBookingService.update(request);

        // Assert
        assertNotNull(actualResponse);
        assertEquals(expectedResponse.getBookingStatus(), actualResponse.getBookingStatus());
    }

    @Test
    void testV3BookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentEnabled() throws RunnerException {
        // Arrange
        UUID bookingGuid = UUID.randomUUID();
        UUID shipmentGuid = UUID.randomUUID();

        // Set up tenant settings with V2 shipment enabled
        V1TenantSettingsResponse tenantSettings = V1TenantSettingsResponse.builder()
                .ShipmentServiceV2Enabled(true)
                .build();
        TenantSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings()
                .setIsAlwaysUtilization(true)
                .setHasNoUtilization(false);

        // Mock booking data
        CustomerBooking inputCustomerBooking = new CustomerBooking();
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setGuid(bookingGuid);
        inputCustomerBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        inputCustomerBooking.setCreatedBy("tester");
        inputCustomerBooking.setIsPlatformBookingCreated(false);
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);

        // Convert to request and update status
        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.PENDING_FOR_REVIEW);

        CustomerBooking updatedBooking = objectMapper.convertValue(request, CustomerBooking.class);
        CustomerBookingV3Response expectedResponse = objectMapper.convertValue(updatedBooking, CustomerBookingV3Response.class);

        // Mocks
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(updatedBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(expectedResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        when(customerBookingDao.save(any())).thenReturn(updatedBooking);

        mockTenantSettings();
        mockShipmentSettings();

        // Act
        CustomerBookingV3Response actualResponse = customerBookingService.update(request);

        // Assert
        assertNotNull(actualResponse);
        assertEquals(BookingStatus.PENDING_FOR_REVIEW, actualResponse.getBookingStatus());
    }

    @Test
    void testV3UpdateBooking_SuccessfulUpdate_ReturnsUpdatedResponse() throws Exception {
        // Arrange
        UUID bookingGuid = UUID.randomUUID();
        long bookingId = 123L;
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .build()
        );
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(bookingId);
        existingBooking.setGuid(bookingGuid);
        existingBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        existingBooking.setCreatedBy("user1");
        existingBooking.setIsPlatformBookingCreated(false);
        existingBooking.setSource(BookingSource.Runner);
        existingBooking.setBookingStatus(BookingStatus.PENDING_FOR_REVIEW);

        CustomerBookingV3Request request = new CustomerBookingV3Request();
        request.setId(bookingId);
        request.setBookingStatus(BookingStatus.PENDING_FOR_KYC);

        CustomerBooking updatedBooking = new CustomerBooking();
        updatedBooking.setId(bookingId);
        updatedBooking.setGuid(bookingGuid);
        updatedBooking.setCreatedAt(existingBooking.getCreatedAt());
        updatedBooking.setCreatedBy(existingBooking.getCreatedBy());
        updatedBooking.setIsPlatformBookingCreated(false);
        updatedBooking.setSource(BookingSource.Runner);
        updatedBooking.setBookingStatus(BookingStatus.PENDING_FOR_KYC);

        CustomerBookingV3Response expectedResponse = new CustomerBookingV3Response();
        expectedResponse.setId(bookingId);
        expectedResponse.setGuid(bookingGuid);

        // Mocks
        when(customerBookingDao.findById(bookingId)).thenReturn(Optional.of(existingBooking));
        when(eventDao.findByEntityIdAndEntityType(bookingId, Constants.BOOKING)).thenReturn(Optional.of(new Events()));
        when(jsonHelper.convertToJson(existingBooking)).thenReturn("{}");
        when(jsonHelper.convertValue(updatedBooking, CustomerBookingV3Response.class)).thenReturn(expectedResponse);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(updatedBooking);
        when(customerBookingDao.save(any())).thenReturn(updatedBooking);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockTenantSettings();
        mockShipmentSettings();

        // Act
        CustomerBookingV3Response response = customerBookingService.update(request);

        // Assert
        assertNotNull(response);
        assertEquals(bookingId, response.getId());
        verify(customerBookingDao, times(1)).save(any());
        verify(eventDao, times(1)).findByEntityIdAndEntityType(bookingId, Constants.BOOKING);
    }

    @Test
    void testV3UpdateBooking_SuccessfulUpdate_ReturnsUpdatedResponse_Send_kafka_events() throws Exception {
        // Arrange
        UUID bookingGuid = UUID.randomUUID();
        long bookingId = 123L;
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .FetchRatesMandate(Boolean.FALSE)
                        .build()
        );
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(bookingId);
        existingBooking.setGuid(bookingGuid);
        existingBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        existingBooking.setCreatedBy("user1");
        existingBooking.setIsPlatformBookingCreated(false);
        existingBooking.setSource(BookingSource.Runner);
        existingBooking.setBookingStatus(BookingStatus.PENDING_FOR_REVIEW);

        CustomerBookingV3Request request = new CustomerBookingV3Request();
        request.setId(bookingId);
        request.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);

        CustomerBooking updatedBooking = new CustomerBooking();
        updatedBooking.setId(bookingId);
        updatedBooking.setGuid(bookingGuid);
        updatedBooking.setCreatedAt(existingBooking.getCreatedAt());
        updatedBooking.setCreatedBy(existingBooking.getCreatedBy());
        updatedBooking.setIsPlatformBookingCreated(false);
        updatedBooking.setSource(BookingSource.Runner);
        updatedBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);

        CustomerBookingV3Response expectedResponse = new CustomerBookingV3Response();
        expectedResponse.setId(bookingId);
        expectedResponse.setGuid(bookingGuid);

        // Mocks
        when(customerBookingDao.findById(bookingId)).thenReturn(Optional.of(existingBooking));
        when(eventDao.findByEntityIdAndEntityType(bookingId, Constants.BOOKING)).thenReturn(Optional.of(new Events()));
        when(jsonHelper.convertToJson(existingBooking)).thenReturn("{}");
        when(jsonHelper.convertValue(updatedBooking, CustomerBookingV3Response.class)).thenReturn(expectedResponse);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(updatedBooking);
        when(customerBookingDao.save(any())).thenReturn(updatedBooking);
        doNothing().when(dependentServiceHelper).pushToKafkaForDownStream(any(), any());
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockTenantSettings();
        mockShipmentSettings();

        // Act
        CustomerBookingV3Response response = customerBookingService.update(request);

        // Assert
        assertNotNull(response);
        assertEquals(bookingId, response.getId());
        verify(customerBookingDao, times(1)).save(any());
        verify(eventDao, times(1)).findByEntityIdAndEntityType(bookingId, Constants.BOOKING);
        verify(dependentServiceHelper, times(1)).pushToKafkaForDownStream(any(), any());
    }

    @Test
    void testV3UpdateBooking_SuccessfulUpdate_ReturnsUpdatedResponse_WithFalseEventPersisted() throws Exception {
        // Arrange
        UUID bookingGuid = UUID.randomUUID();
        long bookingId = 123L;
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .build()
        );
        Packing packing = new Packing();
        packing.setWeight(BigDecimal.ONE);
        packing.setWeightUnit("KG");
        Containers containers = new Containers();
        containers.setContainerCode("CNT122");
        containers.setContainerCount(1L);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        CustomerBooking existingBooking = new CustomerBooking();
        existingBooking.setId(bookingId);
        existingBooking.setGuid(bookingGuid);
        existingBooking.setCreatedAt(LocalDateTime.now().minusDays(1));
        existingBooking.setCreatedBy("user1");
        existingBooking.setIsPlatformBookingCreated(false);
        existingBooking.setSource(BookingSource.Runner);
        existingBooking.setBookingStatus(BookingStatus.PENDING_FOR_REVIEW);
        existingBooking.setPackingList(List.of(packing));
        existingBooking.setContainersList(List.of(containers));

        CustomerBookingV3Request request = new CustomerBookingV3Request();
        request.setId(bookingId);
        request.setBookingStatus(BookingStatus.PENDING_FOR_KYC);
        CustomerBooking updatedBooking = new CustomerBooking();
        updatedBooking.setId(bookingId);
        updatedBooking.setGuid(bookingGuid);
        updatedBooking.setCreatedAt(existingBooking.getCreatedAt());
        updatedBooking.setCreatedBy(existingBooking.getCreatedBy());
        updatedBooking.setIsPlatformBookingCreated(false);
        updatedBooking.setSource(BookingSource.Runner);
        updatedBooking.setBookingStatus(BookingStatus.PENDING_FOR_KYC);

        CustomerBookingV3Response expectedResponse = new CustomerBookingV3Response();
        expectedResponse.setId(bookingId);
        expectedResponse.setGuid(bookingGuid);

        // Mocks
        when(customerBookingDao.findById(bookingId)).thenReturn(Optional.of(updatedBooking));
        when(eventDao.findByEntityIdAndEntityType(bookingId, Constants.BOOKING)).thenReturn(Optional.empty());
        when(jsonHelper.convertToJson(any(CustomerBooking.class))).thenReturn("{}");
        when(jsonHelper.convertValue(updatedBooking, CustomerBookingV3Response.class)).thenReturn(expectedResponse);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(updatedBooking);
        when(customerBookingDao.save(any())).thenReturn(updatedBooking);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockTenantSettings();
        mockShipmentSettings();

        // Act
        CustomerBookingV3Response response = customerBookingService.update(request);

        // Assert
        assertNotNull(response);
        assertEquals(bookingId, response.getId());
        verify(customerBookingDao, times(1)).save(any());
        verify(eventDao, times(1)).findByEntityIdAndEntityType(bookingId, Constants.BOOKING);
    }

    @Test
    void testV3BookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentEnabled_AirShipment_AIRDGPermission_AirDgTrue_AirPermissionFalse_throwsException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .build()
        );
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setId(1L);
        inputCustomerBooking.setGuid(UUID.randomUUID());
        inputCustomerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setTransportType("AIR");
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.AIR_DG, false));
        request.setIsDg(true);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);
        // Mock
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        // Test
        assertThrows(ValidationException.class, () -> customerBookingService.update(customerBookingRequest));
    }

    @Test
    void testV3BookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentDisabled() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(false)
                        .build()
        );
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);
        var mockV1ShipmentCreationResponse = new V1ShipmentCreationResponse();
        mockV1ShipmentCreationResponse.setShipmentId("123");
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(jsonHelper.convertValue(any(), eq(V1ShipmentCreationResponse.class))).thenReturn(mockV1ShipmentCreationResponse);
        when(bookingIntegrationsUtility.createShipmentInV1(any(), anyBoolean(), anyBoolean(), any(), any())).thenReturn(ResponseEntity.ok(mockV1ShipmentCreationResponse));
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockTenantSettings();
        mockShipmentSettings();
        // Test
        CustomerBookingV3Response response = customerBookingService.update(request);
        // Assert
        assertNotNull(response);
        verify(customerBookingDao, times(2)).save(any());
    }

    @Test
    void testV3BookingUpdateWithSuccessCreateInPlatform() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(bookingChargesDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getBookingCharges());
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();
        // Test
        CustomerBookingV3Response response = customerBookingService.update(request);
        // Assert
        assertNotNull(response);
        verify(customerBookingDao, times(1)).save(any());
    }

    @Test
    void testBookingUpdateWithSuccessUpdateInPlatform() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setIsPlatformBookingCreated(true);
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingV3Request request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Request.class);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingV3Response.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        when(bookingChargesDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getBookingCharges());
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();
        // Test
        CustomerBookingV3Response response = customerBookingService.update(request);
        // Assert
        assertNotNull(response);
        verify(customerBookingDao, times(1)).save(any());
    }

    @Test
    void testV3BookingUpdateWithUtilization() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var oldCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        var newCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        ;
        oldCustomerBooking.setContractId("old");
        newCustomerBooking.setContractId("new");
        newCustomerBooking.setBookingStatus(BookingStatus.CANCELLED);
        CustomerBookingV3Request request = objectMapper.convertValue(newCustomerBooking, CustomerBookingV3Request.class);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(newCustomerBooking, CustomerBookingV3Response.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(newCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(oldCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(newCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();
        // Test
        CustomerBookingV3Response response = customerBookingService.update(request);
        // Assert
        assertNotNull(response);
        verify(customerBookingDao, times(1)).save(any());
    }

    @Test
    void testV3BookingUpdateWithUtilization2() throws RunnerException {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var oldCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        var newCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        ;
        oldCustomerBooking.setContractId(null);
        newCustomerBooking.setContractId("new");
        CustomerBookingV3Request request = objectMapper.convertValue(newCustomerBooking, CustomerBookingV3Request.class);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(newCustomerBooking, CustomerBookingV3Response.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(newCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(oldCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(newCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();
        // Test
        CustomerBookingV3Response response = customerBookingService.update(request);
        // Assert
        assertNotNull(response);
        verify(customerBookingDao, times(1)).save(any());
    }

    @Test
    void testV3BookingUpdateWithUtilization3() throws RunnerException {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var oldCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        var newCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        oldCustomerBooking.setContractId("old");
        newCustomerBooking.setContractId(null);
        CustomerBookingV3Request request = objectMapper.convertValue(newCustomerBooking, CustomerBookingV3Request.class);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(newCustomerBooking, CustomerBookingV3Response.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(newCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(oldCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(newCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();
        // Test
        CustomerBookingV3Response response = customerBookingService.update(request);
        // Assert
        assertNotNull(response);
        verify(customerBookingDao, times(1)).save(any());
    }

    @Test
    void testV3PlatformCreateBooking3() throws RunnerException {
        PlatformToRunnerCustomerBookingRequest request = new PlatformToRunnerCustomerBookingRequest();
        request.setBookingNumber("BookingNumber");
        var container = BookingContainerRequest.builder().containerCode("20FR").containerCount(12L).commodityGroup("FAK").build();
        container.setGuid(UUID.randomUUID());
        var addressDataMap = customerBooking.getCustomer().getAddressData();
        var orgDataMap = customerBooking.getCustomer().getOrgData();
        var containersList = List.of(
                BookingContainerRequest.builder().containerCode("20GP").containerCount(1L).commodityGroup("FAK").runner_guid(UUID.randomUUID()).build(),
                BookingContainerRequest.builder().containerCode("20FR").containerCount(12L).commodityGroup("FAK").build(),
                container
        );
        var pack = BookingPackingRequest.builder().dimensionUnit("M").isDimension(true).commodityGroup("FAK").build();
        pack.setGuid(UUID.randomUUID());
        var packingList = List.of(
                BookingPackingRequest.builder().dimensionUnit("M").isDimension(true).commodityGroup("FAK").build(),
                BookingPackingRequest.builder().dimensionUnit("M").isDimension(false).commodityGroup("FAK").build(),
                pack
        );
        var route = BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("UIHJK").build();
        route.setGuid(UUID.randomUUID());
        var routingList = List.of(
                BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("APLU").build(),
                route
        );
        var chargesList = List.of(
                PlatformBookingChargesRequest.builder()
                        .creditor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build())
                        .debtor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build())
                        .build()
        );
        request.setContainersList(containersList);
        request.setPackingList(packingList);
        request.setRoutingList(routingList);

        request.setBookingCharges(chargesList);
        request.setCustomer(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").build());
        request.setConsignee(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setConsignor(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());
        request.setNotifyParty(PartiesRequest.builder().orgCode("FRC0001").addressCode("FRC0002").orgData(orgDataMap).addressData(addressDataMap).build());

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        CustomerBookingV3Request customerBookingRequest = new CustomerBookingV3Request();
        customerBookingRequest.setContainersList(List.of(
                ContainerV3Request.builder().guid(UUID.randomUUID()).id(123L).containerCode("20GP").containerCount(2L).containerCode("20GP").commodityGroup("group").build(),
                ContainerV3Request.builder().guid(UUID.fromString("9f9c2d42-d479-44bf-8541-0029e498fc86")).containerCount(2L).containerCode("20GP").commodityGroup("group").build()
        ));
        customerBookingRequest.setPackingList(List.of(
                PackingV3Request.builder().guid(UUID.randomUUID()).packs("2").packsType("bundle").commodity("commodity").build(),
                PackingV3Request.builder().guid(UUID.fromString("9f9c2d42-d479-44bf-8541-0029e498fc88")).packs("1").packsType("bundle").commodity("commodity").build()
        ));
        customerBookingRequest.setRoutingList(List.of(
                RoutingsRequest.builder().guid(UUID.randomUUID()).build(),
                RoutingsRequest.builder().guid(UUID.fromString("2cc2e601-f931-4d75-8d16-178b1888ceb6")).build()
        ));
        customerBookingRequest.setBookingCharges(List.of(
                BookingChargesRequest.builder().guid(UUID.randomUUID()).build(),
                BookingChargesRequest.builder().guid(UUID.fromString("da0348c2-3cf4-46ef-a6d6-48db4dcef45b")).build()
        ));

        CustomerBooking customerBooking = objectMapper.convertValue(this.customerBooking, CustomerBooking.class);
        customerBooking.setId(1L);
        customerBooking.setGuid(UUID.randomUUID());
        customerBooking.setBookingNumber("DBAR-5586091-311749");
        customerBooking.setCarrierDetails(CarrierDetails.builder().build());
        customerBooking.setIsConsigneeAddressFreeText(customerBooking.getIsConsigneeFreeText() != null && customerBooking.getIsConsigneeFreeText());
        customerBooking.setIsConsignorAddressFreeText(customerBooking.getIsConsignorFreeText() != null && customerBooking.getIsConsignorFreeText());
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(customerBooking.getIsNotifyPartyFreeText() != null && customerBooking.getIsNotifyPartyFreeText());
        customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);


        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingV3Response.class);
        // Mock
        ArgumentCaptor captor = ArgumentCaptor.forClass(Map.class);
        doAnswer(invocation -> {
            Map<String, PartiesRequest> masterDataMap = (Map<String, PartiesRequest>) captor.getValue();
            masterDataMap.clear();
            masterDataMap.put("Customer", PartiesRequest.builder().build());
            masterDataMap.put("Consignor", PartiesRequest.builder().build());
            masterDataMap.put("Consignee", PartiesRequest.builder().build());
            masterDataMap.put("Notify Party", PartiesRequest.builder().build());
            return null;
        }).when(bookingIntegrationsUtility).transformOrgAndAddressPayloadToGivenParties((Map<String, PartiesRequest>) captor.capture());
        when(customerBookingDao.findByBookingNumber(any())).thenReturn(Optional.of(customerBooking));
        when(modelMapper.map(any(), eq(CustomerBookingV3Request.class))).thenReturn(customerBookingRequest);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        // Test
        PlatformToRunnerCustomerBookingResponse platformResponse = customerBookingService.platformCreateBooking(request);

        // Assert
        verify(customerBookingDao, times(1)).save(customerBooking);
        assertNotNull(platformResponse.getBookingNumber());
    }


    @Test
    void testV3Clone() throws RunnerException {
        CustomerBooking customerBooking1 = new CustomerBooking();
        customerBooking1.setTransportType(TRANSPORT_MODE_AIR);
        customerBooking1.setDirection(DIRECTION_EXP);

        CustomerBookingV3Response customerBookingResponse = new CustomerBookingV3Response();
        customerBookingResponse.setId(1L);
        customerBookingResponse.setGuid(UUID.randomUUID());
        customerBookingResponse.setBookingStatus(BookingStatus.CANCELLED);

        when(customerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking1));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        UserContext.getUser().setPermissions(new HashMap<>());
        UserContext.getUser().getPermissions().put(PermissionConstants.AIR_SECURITY_PERMISSION, true);

        mockShipmentSettings();

        CustomerBookingV3Response result = customerBookingService.cloneBooking(1L);

        assertNull(result.getId(), "Cloned booking should have null ID");
        assertNull(result.getGuid(), "Cloned booking should have null GUID");
        assertEquals(BookingStatus.PENDING_FOR_KYC, result.getBookingStatus(), "Booking status should be set to PENDING_FOR_KYC");
        assertNotNull(result.getBookingDate(), "Booking date should be set");
    }

    @Test
    void testV3Clone2() throws RunnerException {
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setTransportType(TRANSPORT_MODE_AIR);
        customerBooking.setDirection(DIRECTION_EXP);
        ContainerResponse containerResponse = new ContainerResponse();
        containerResponse.setContainerCode("20FR");
        containerResponse.setContainerCount(1L);
        containerResponse.setCommodityGroup("Group");
        containerResponse.setGrossWeight(new BigDecimal(1));
        containerResponse.setGrossWeightUnit("KG");

        PackingResponse packingResponse = new PackingResponse();
        packingResponse.setPacks("2");
        packingResponse.setPacksType("Packages");
        packingResponse.setWeight(new BigDecimal(4));
        packingResponse.setWeightUnit("KG");
        packingResponse.setVolume(new BigDecimal(10));
        packingResponse.setVolumeUnit("M3");
        packingResponse.setLength(new BigDecimal(2));
        packingResponse.setLengthUnit("M");
        packingResponse.setWidth(new BigDecimal(2));
        packingResponse.setWidthUnit("M");
        packingResponse.setHeight(new BigDecimal(2));
        packingResponse.setHeightUnit("M");
        packingResponse.setGoodsDescription("Description");
        packingResponse.setNetWeight(new BigDecimal(2));
        packingResponse.setNetWeightUnit("KG");
        packingResponse.setVolumeWeight(new BigDecimal(2));
        packingResponse.setVolumeWeightUnit("KgM3");
        packingResponse.setCommodityGroup("CommodityGroup");
        packingResponse.setChargeable(new BigDecimal(10));
        packingResponse.setChargeableUnit("KG");

        RoutingsResponse routingsResponse = new RoutingsResponse();
        routingsResponse.setPol("POL");
        routingsResponse.setLeg(1L);
        routingsResponse.setMode("AIR");
        routingsResponse.setPod("POD");

        ReferenceNumbersResponse referenceNumbersResponse = new ReferenceNumbersResponse();
        referenceNumbersResponse.setCountryOfIssue("IND");
        referenceNumbersResponse.setType("Type");
        referenceNumbersResponse.setReferenceNumber("2314");

        CustomerBookingV3Response customerBookingResponse = new CustomerBookingV3Response();
        customerBookingResponse.setId(1L);
        customerBookingResponse.setGuid(UUID.randomUUID());
        customerBookingResponse.setBookingStatus(BookingStatus.CANCELLED);
        customerBookingResponse.setCustomer(new PartiesResponse());
        customerBookingResponse.setConsignee(new PartiesResponse());
        customerBookingResponse.setConsignor(new PartiesResponse());
        customerBookingResponse.setNotifyParty(new PartiesResponse());
        customerBookingResponse.setCarrierDetails(new CarrierDetailResponse());
        customerBookingResponse.setContainersList(List.of(containerResponse));
        customerBookingResponse.setPackingList(List.of(packingResponse));
        customerBookingResponse.setRoutingList(List.of(routingsResponse));
        customerBookingResponse.setReferenceNumbersList(List.of(referenceNumbersResponse));

        when(customerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);

        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setCountryAirCargoSecurity(true);
        UserContext.getUser().setPermissions(new HashMap<>());
        UserContext.getUser().getPermissions().put(PermissionConstants.AIR_SECURITY_PERMISSION, true);

        mockShipmentSettings();

        CustomerBookingV3Response result = customerBookingService.cloneBooking(1L);

        assertNull(result.getId(), "Cloned booking should have null ID");
        assertNull(result.getGuid(), "Cloned booking should have null GUID");
        assertEquals(BookingStatus.PENDING_FOR_KYC, result.getBookingStatus(), "Booking status should be set to PENDING_FOR_KYC");
        assertNotNull(result.getBookingDate(), "Booking date should be set");
    }

    @Test
    void testV3Clone4() throws RunnerException {
        when(customerBookingDao.findById(any())).thenReturn(Optional.empty());
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            customerBookingService.cloneBooking(1L);
        });
        assertEquals("Failed to fetch data for given constraint.", exception.getMessage());
    }

    @Test
    void testV3Clone5() throws RunnerException {
        when(customerBookingDao.findById(any())).thenThrow(new RuntimeException());
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            customerBookingService.cloneBooking(1L);
        });
        assertEquals(DAO_GENERIC_RETRIEVE_EXCEPTION_MSG, exception.getMessage());
    }

    @Test
    void testV3Clone_with_bookingId_null() {
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            customerBookingService.cloneBooking(null);
        });
        assertEquals("Booking Id cannot be null", exception.getMessage());
    }

    @Test
    void testV3CheckForCrediLimitManagement_returnsTrue() throws JsonProcessingException, RunnerException {
        CustomerBooking customerBooking1 = customerBooking;
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Approved");
        var result = customerBookingService.checkForCreditLimitManagement(customerBooking1);
        assertTrue(result);
    }

    @Test
    void testV3CheckForCrediLimitManagement_finalStatusInit_returnsFalse() throws JsonProcessingException, RunnerException {
        CustomerBooking customerBooking1 = customerBooking;
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Init");
        var result = customerBookingService.checkForCreditLimitManagement(customerBooking1);
        assertFalse(result);
    }

    @Test
    void testV3CheckForCrediLimitManagement_nullString_returnsFalse() throws JsonProcessingException, RunnerException {
        CustomerBooking customerBooking1 = customerBooking;
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn(null);
        var result = customerBookingService.checkForCreditLimitManagement(customerBooking1);
        assertFalse(result);
    }

    @Test
    void testV3CreateKafkaEventGuidNull() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        request.setOrderManagementNumber("Odn1");
        request.setOrderManagementId("Od1");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(new CustomerBooking());
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        assertEquals(actualResponse, customerBookingResponse);
    }

    @Test
    void testV3CreateKafkaEvent() throws RunnerException {
        CustomerBookingV3Request request = new CustomerBookingV3Request();
        request.setOrderManagementNumber("Odn1");
        request.setOrderManagementId("Od1");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();

        mockCustomerBooking.setGuid(UUID.randomUUID());
        mockCustomerBooking.setId(1L);
        CustomerBookingV3Response customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingV3Response.class);

        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(new CustomerBooking());
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingV3Response.class))).thenReturn(customerBookingResponse);
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        mockShipmentSettings();

        CustomerBookingV3Response actualResponse = customerBookingService.create(request);

        assertEquals(actualResponse, customerBookingResponse);
    }

    @Test
    void testV3RetryForBillingFailsWhenCustomerBookingIsNotPresent() {
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking customerBooking = new CustomerBooking();
        String errorMessage = DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;

        // Mock
        when(customerBookingDao.findById(id)).thenReturn(Optional.empty());

        RunnerException exception = assertThrows(RunnerException.class, () -> {
            customerBookingService.retryForBilling(request);
        });

        // Assert that the message is as expected
        assertEquals(errorMessage, exception.getMessage());
    }

    @Test
    void testV3RetryForBillingFailsWhenStatusNotReadyForShipment() throws RunnerException {
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking customerBooking = new CustomerBooking();
        String errorMessage = String.format("Booking should be in: %s stage for this operation", BookingStatus.READY_FOR_SHIPMENT);

        // Mock
        when(customerBookingDao.findById(id)).thenReturn(Optional.of(customerBooking));
        RunnerException exception = assertThrows(RunnerException.class, () -> {
            customerBookingService.retryForBilling(request);
        });

        // Assert that the message is as expected
        assertEquals(errorMessage, exception.getMessage());
    }

    @Test
    void testV3RetryForBillingFailsWhenBillIsCreated() throws RunnerException {
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        customerBooking.setIsBillCreated(true);
        String errorMessage = String.format("Bill is already created for booking with id: %s", request.getId());

        // Mock
        when(customerBookingDao.findById(id)).thenReturn(Optional.of(customerBooking));

        RunnerException exception = assertThrows(RunnerException.class, () -> {
            customerBookingService.retryForBilling(request);
        });

        // Assert that the message is as expected
        assertEquals(errorMessage, exception.getMessage());
    }
    @Test
    void testV3RetryForBillingSuccess() throws RunnerException {
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        customerBooking.setShipmentGuid(String.valueOf(UUID.randomUUID()));

        V1ShipmentCreationResponse shipmentCreationResponse = new V1ShipmentCreationResponse();
        ResponseEntity<V1ShipmentCreationResponse> v1ShipmentCreationResponseResponseEntity = new ResponseEntity<>(shipmentCreationResponse, HttpStatus.OK);

        // Mock
        when(customerBookingDao.findById(id)).thenReturn(Optional.of(customerBooking));
        when(bookingIntegrationsUtility.createShipmentInV1(any(),anyBoolean(), anyBoolean(), any(), any())).thenReturn(v1ShipmentCreationResponseResponseEntity);
        when(jsonHelper.convertValue(any(), eq(V1ShipmentCreationResponse.class))).thenReturn(shipmentCreationResponse);

        // Test
        V1ShipmentCreationResponse v1ShipmentCreationResponse = customerBookingService.retryForBilling(request);

        // Assert
        assertNotNull(v1ShipmentCreationResponse);
    }

    @Test
    void testV3CreditCheckFailure() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(false)
                        .IsCreditLimitWithFusionEnabled(false)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3CreditCheckFailure2() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(false)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3CreditCheckFailure3() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of())
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3CreditCheckWithCreditLimitWith0Value() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of("CUS_BK"))
                        .CreditLimitOn(0)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
//        creditLimitRequest.setCustomerIdentifierId("12212112");
        creditLimitRequest.setClientOrgCode("FRC00003424");
        creditLimitRequest.setClientAddressCode("FRDO0005605");
        var org = jsonTestUtility.getOrganizationData();
        var address = jsonTestUtility.getAddressData();

        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().entities(List.of(org)).build());
        when(jsonHelper.convertValueToList(any(),eq(EntityTransferOrganizations.class))).thenReturn(List.of(org));

        when(v1Service.addressList(any())).thenReturn(V1DataResponse.builder().entities(List.of(address)).build());
        when(jsonHelper.convertValueToList(any(),eq(EntityTransferAddress.class))).thenReturn(List.of(address));
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3CreditCheckWithCreditLimitWith0Value2() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of("CUS_BK"))
                        .CreditLimitOn(0)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        creditLimitRequest.setCustomerIdentifierId("12212112");
        creditLimitRequest.setClientOrgCode("FRC00003424");
        creditLimitRequest.setClientAddressCode("FRDO0005605");
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3CreditCheckWithCreditLimitWith1Value() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of("CUS_BK"))
                        .CreditLimitOn(1)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        creditLimitRequest.setClientOrgCode("FRC00003424");
        creditLimitRequest.setClientAddressCode("FRDO0005605");
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3CreditCheckWithCreditLimitWith1ValueWithGlobalFusionOn() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of("CUS_BK"))
                        .IsGlobalFusionIntegrationEnabled(true)
                        .BusinessUnitName("ANY")
                        .CreditLimitOn(1)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        creditLimitRequest.setSiteIdentifierId("12212112");
        creditLimitRequest.setClientOrgCode("FRC00003424");
        creditLimitRequest.setClientAddressCode("FRDO0005605");
        var mockFusionResponse = ResponseHelper.buildDependentServiceResponse(DependentServiceResponse.builder().data(new CheckCreditBalanceFusionResponse()).build());
        var mockUpdateCreditLimitResponse = new UpdateOrgCreditLimitBookingResponse();
        var mockCheckCreditBalanceFusionResponse = new CheckCreditBalanceFusionResponse();
        mockCheckCreditBalanceFusionResponse.setData(
                CheckCreditBalanceFusionResponse.FusionData.builder()
                        .accountNumber("213123213")
                        .siteNumber("213213213321")
                        .creditDetails(List.of(CheckCreditBalanceFusionResponse.CreditDetails.builder().CreditLimit(111).CreditLimitCurrency("INR").TotalCreditLimit(111).build()))
                        .build());
        mockUpdateCreditLimitResponse.setSuccess(true);
        when(fusionServiceAdapter.checkCreditLimitP100(any())).thenReturn(mockFusionResponse);
        when(bookingIntegrationsUtility.updateOrgCreditLimitFromBooking(any())).thenReturn(ResponseEntity.ok(mockUpdateCreditLimitResponse));
        when(modelMapper.map(any(), eq(CheckCreditBalanceFusionResponse.class))).thenReturn(mockCheckCreditBalanceFusionResponse);
        when(jsonHelper.convertValue(any(), eq(UpdateOrgCreditLimitBookingResponse.class))).thenReturn(mockUpdateCreditLimitResponse);
        mockTenantSettings();
        CheckCreditLimitResponse checkCreditLimitResponse = customerBookingService.checkCreditLimitFromFusion(creditLimitRequest);
        assertNotNull(checkCreditLimitResponse);
    }

    @Test
    void testV3CreditCheckWithCreditLimitWith1ValueWithGlobalFusionOn2() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of("CUS_BK"))
                        .IsGlobalFusionIntegrationEnabled(true)
                        .BusinessUnitName("ANY")
                        .CreditLimitOn(1)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        creditLimitRequest.setSiteIdentifierId("12212112");
        creditLimitRequest.setClientOrgCode("FRC00003424");
        creditLimitRequest.setClientAddressCode("FRDO0005605");
        var mockFusionResponse = ResponseHelper.buildDependentServiceResponse(DependentServiceResponse.builder().data(new CheckCreditBalanceFusionResponse()).build());
        var mockUpdateCreditLimitResponse = new UpdateOrgCreditLimitBookingResponse();
        var mockCheckCreditBalanceFusionResponse = new CheckCreditBalanceFusionResponse();
        mockCheckCreditBalanceFusionResponse.setData(
                CheckCreditBalanceFusionResponse.FusionData.builder()
                        .accountNumber("78678").siteNumber("434343")
                        .creditDetails(List.of(CheckCreditBalanceFusionResponse.CreditDetails.builder().CreditLimit(111).CreditLimitCurrency("INR").TotalCreditLimit(111).build()))
                        .build());
        mockUpdateCreditLimitResponse.setSuccess(false);
        when(fusionServiceAdapter.checkCreditLimitP100(any())).thenReturn(mockFusionResponse);
        when(bookingIntegrationsUtility.updateOrgCreditLimitFromBooking(any())).thenReturn(ResponseEntity.ok(mockUpdateCreditLimitResponse));
        when(modelMapper.map(any(), eq(CheckCreditBalanceFusionResponse.class))).thenReturn(mockCheckCreditBalanceFusionResponse);
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(GenericException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3CreditCheckWithCreditLimitWith1ValueWithGlobalFusionOn3() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of("CUS_BK"))
                        .IsGlobalFusionIntegrationEnabled(true)
                        .BusinessUnitName("ANY")
                        .CreditLimitOn(1)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        creditLimitRequest.setSiteIdentifierId("12212112");
        creditLimitRequest.setClientOrgCode("FRC00003424");
        creditLimitRequest.setClientAddressCode("FRDO0005605");
        var mockFusionResponse = ResponseHelper.buildDependentServiceResponse(DependentServiceResponse.builder().data(new CheckCreditBalanceFusionResponse()).build());
        var mockUpdateCreditLimitResponse = new UpdateOrgCreditLimitBookingResponse();
        var mockCheckCreditBalanceFusionResponse = new CheckCreditBalanceFusionResponse();
        mockCheckCreditBalanceFusionResponse.setData(
                CheckCreditBalanceFusionResponse.FusionData.builder()
                        .accountNumber("78678").siteNumber("434343")
                        .creditDetails(List.of(CheckCreditBalanceFusionResponse.CreditDetails.builder().CreditLimit(111).CreditLimitCurrency("INR").TotalCreditLimit(111).build()))
                        .build());
        mockUpdateCreditLimitResponse.setSuccess(false);
        when(fusionServiceAdapter.checkCreditLimitP100(any())).thenReturn(null);
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testV3CreditCheckWithCreditLimitWith1ValueWithGlobalFusionOn4() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of("CUS_BK"))
                        .IsGlobalFusionIntegrationEnabled(true)
                        .BusinessUnitName("ANY")
                        .CreditLimitOn(1)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        creditLimitRequest.setSiteIdentifierId("12212112");
        creditLimitRequest.setClientOrgCode("FRC00003424");
        creditLimitRequest.setClientAddressCode("FRDO0005605");
        var mockFusionResponse = ResponseHelper.buildDependentServiceResponse(DependentServiceResponse.builder().data(new CheckCreditBalanceFusionResponse()).build());
        var mockUpdateCreditLimitResponse = new UpdateOrgCreditLimitBookingResponse();
        var mockCheckCreditBalanceFusionResponse = new CheckCreditBalanceFusionResponse();
        mockCheckCreditBalanceFusionResponse.setData(
                CheckCreditBalanceFusionResponse.FusionData.builder()
                        .accountNumber("213123213")
                        .siteNumber("213213213321")
                        .build());
        mockUpdateCreditLimitResponse.setSuccess(true);
        when(fusionServiceAdapter.checkCreditLimitP100(any())).thenReturn(mockFusionResponse);
        when(modelMapper.map(any(), eq(CheckCreditBalanceFusionResponse.class))).thenReturn(mockCheckCreditBalanceFusionResponse);
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(creditLimitRequest));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }


    @Test
    void testAddAllMasterDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setCarrierDetails(CarrierDetailResponse.builder().build());
        customerBookingV3Response.setReferenceNumbersList(List.of(new ReferenceNumbersResponse()));
        customerBookingV3Response.setContainersList(List.of(new ContainerResponse()));
        customerBookingV3Response.setPackingList(List.of(new PackingResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllMasterDataInSingleCallResult = customerBookingService
                .addAllMasterDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkMasterList(isA(MasterListRequestV2.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllMasterDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllMasterDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());

        CustomerBookingV3Response customerBookingV3Response = CustomerBookingV3Response.builder().build();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllMasterDataInSingleCallResult = customerBookingService
                .addAllMasterDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), isA(Map.class), Mockito.<String>any(), isA(Map.class));
        verify(masterDataUtils).fetchInBulkMasterList(isA(MasterListRequestV2.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllMasterDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllMasterDataInSingleCall3() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = CustomerBookingV3Response.builder().build();
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllMasterDataInSingleCallResult = customerBookingService
                .addAllMasterDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), isA(Map.class), Mockito.<String>any(), isA(Map.class));
        verify(masterDataUtils).fetchInBulkMasterList(isA(MasterListRequestV2.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllMasterDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllMasterDataInSingleCall4() throws InterruptedException, ExecutionException {
        // Arrange
        when(masterDataUtils.createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInBulkMasterList(Mockito.<MasterListRequestV2>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());

        CustomerBookingV3Response customerBookingV3Response = CustomerBookingV3Response.builder().build();
        customerBookingV3Response.setCarrierDetails(CarrierDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllMasterDataInSingleCallResult = customerBookingService
                .addAllMasterDataInSingleCall(customerBookingV3Response, null);

        // Assert
        verify(masterDataUtils, atLeast(1)).createInBulkMasterListRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), isA(Map.class), Mockito.<String>any(), isA(Map.class));
        verify(masterDataUtils).fetchInBulkMasterList(isA(MasterListRequestV2.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllMasterDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }


    @Test
    void testAddAllUnlocationDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkUnlocations(Mockito.<Set<String>>any(), Mockito.<String>any()))
                .thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllUnlocationDataInSingleCallResult = customerBookingService
                .addAllUnlocationDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        ResponseEntity<IRunnerResponse> getResult = actualAddAllUnlocationDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllUnlocationDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkUnlocations(Mockito.<Set<String>>any(), Mockito.<String>any()))
                .thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setCarrierDetails(CarrierDetailResponse.builder().build());
        customerBookingV3Response.setContainersList(List.of(new ContainerResponse()));
        customerBookingV3Response.setPackingList(List.of(new PackingResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllUnlocationDataInSingleCallResult = customerBookingService
                .addAllUnlocationDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        ResponseEntity<IRunnerResponse> getResult = actualAddAllUnlocationDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllUnlocationDataInSingleCall3() throws InterruptedException, ExecutionException {
        // Arrange
        when(masterDataUtils.fetchInBulkUnlocations(Mockito.<Set<String>>any(), Mockito.<String>any()))
                .thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setCarrierDetails(CarrierDetailResponse.builder().build());
        customerBookingV3Response.setContainersList(List.of(new ContainerResponse()));
        customerBookingV3Response.setPackingList(List.of(new PackingResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllUnlocationDataInSingleCallResult = customerBookingService
                .addAllUnlocationDataInSingleCall(customerBookingV3Response, null);

        // Assert
        ResponseEntity<IRunnerResponse> getResult = actualAddAllUnlocationDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCarrierDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkCarriers(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setCarrierDetails(CarrierDetailResponse.builder().build());

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCarrierDataInSingleCallResult = customerBookingService
                .addAllCarrierDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkCarriers(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCarrierDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCarrierDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        when(masterDataUtils.fetchInBulkCarriers(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setCarrierDetails(CarrierDetailResponse.builder().build());

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCarrierDataInSingleCallResult = customerBookingService
                .addAllCarrierDataInSingleCall(customerBookingV3Response, null);

        // Assert
        verify(masterDataUtils).fetchInBulkCarriers(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCarrierDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCurrencyDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkCurrencyRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInCurrencyList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCurrencyDataInSingleCallResult = customerBookingService
                .addAllCurrencyDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInCurrencyList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCurrencyDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllCurrencyDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkCurrencyRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInCurrencyList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllCurrencyDataInSingleCallResult = customerBookingService
                .addAllCurrencyDataInSingleCall(customerBookingV3Response, null);

        // Assert
        verify(masterDataUtils).fetchInCurrencyList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllCurrencyDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllTenantDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkTenantsRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInTenantsList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllTenantDataInSingleCallResult = customerBookingService
                .addAllTenantDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInTenantsList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllTenantDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllTenantDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkTenantsRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInTenantsList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = CustomerBookingV3Response.builder().build();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllTenantDataInSingleCallResult = customerBookingService
                .addAllTenantDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils, atLeast(1)).createInBulkTenantsRequest(Mockito.<IRunnerResponse>any(),
                Mockito.<Class<Object>>any(), isA(Map.class), Mockito.<String>any(), isA(Map.class));
        verify(masterDataUtils).fetchInTenantsList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllTenantDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllTenantDataInSingleCall3() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.createInBulkTenantsRequest(Mockito.<IRunnerResponse>any(), Mockito.<Class<Object>>any(),
                Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(), Mockito.<Map<String, Object>>any())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInTenantsList(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();

        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllTenantDataInSingleCallResult = customerBookingService
                .addAllTenantDataInSingleCall(customerBookingV3Response, null);

        // Assert
        verify(masterDataUtils).fetchInTenantsList(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllTenantDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllContainerTypesInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkContainerTypes(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setContainersList(List.of(new ContainerResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllContainerTypesInSingleCallResult = customerBookingService
                .addAllContainerTypesInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkContainerTypes(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllContainerTypesInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllChargeTypesInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkChargeTypes(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setBookingCharges(List.of(new BookingChargesResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllChargeTypesInSingleCallResult = customerBookingService
                .addAllChargeTypesInSingleMDMCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkChargeTypes(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllChargeTypesInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllContainerTypesInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkContainerTypes(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setContainersList(List.of(new ContainerResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllContainerTypesInSingleCallResult = customerBookingService
                .addAllContainerTypesInSingleCall(customerBookingV3Response, null);

        // Assert
        verify(masterDataUtils).fetchInBulkContainerTypes(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllContainerTypesInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllSalesAgentInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInSalesAgentList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllSalesAgentInSingleCallResult = customerBookingService
                .addAllSalesAgentInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInSalesAgentList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllSalesAgentInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllSalesAgentInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInSalesAgentList(Mockito.<List<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllSalesAgentInSingleCallResult = customerBookingService
                .addAllSalesAgentInSingleCall(customerBookingV3Response, null);

        // Assert
        verify(masterDataUtils).fetchInSalesAgentList(isA(List.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllSalesAgentInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(((Map<Object, Object>) ((RunnerResponse<Object>) getResult.getBody()).getData()).isEmpty());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllVesselDataInSingleCall() throws InterruptedException, ExecutionException {
        // Arrange
        doNothing().when(masterDataKeyUtils)
                .setMasterDataValue(Mockito.<Map<String, Map<String, String>>>any(), Mockito.<String>any(),
                        Mockito.<Map<String, Object>>any(), Mockito.<Map<String, Object>>any());
        when(masterDataUtils.fetchInBulkVessels(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setContainersList(List.of(new ContainerResponse()));
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllVesselDataInSingleCallResult = customerBookingService
                .addAllVesselDataInSingleCall(customerBookingV3Response, new HashMap<>());

        // Assert
        verify(masterDataUtils).fetchInBulkVessels(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllVesselDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllVesselDataInSingleCall2() throws InterruptedException, ExecutionException {
        // Arrange
        when(masterDataUtils.fetchInBulkVessels(Mockito.<Set<String>>any())).thenReturn(new HashMap<>());
        doNothing().when(masterDataUtils).pushToCache(Mockito.<Map<String, Object>>any(), Mockito.<String>any(), Mockito.any(), Mockito.any(), Mockito.any());
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        customerBookingV3Response.setContainersList(List.of(new ContainerResponse()));
        customerBookingV3Response.setCarrierDetails(CarrierDetailResponse.builder().build());
        // Act
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllVesselDataInSingleCallResult = customerBookingService
                .addAllVesselDataInSingleCall(customerBookingV3Response, null);

        // Assert
        verify(masterDataUtils).fetchInBulkVessels(isA(Set.class));
        ResponseEntity<IRunnerResponse> getResult = actualAddAllVesselDataInSingleCallResult.get();
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getError());
        assertNull(((RunnerResponse<Object>) getResult.getBody()).getRequestId());
        assertEquals(0, ((RunnerResponse<Object>) getResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) getResult.getBody()).getCount());
        assertEquals(HttpStatus.OK, getResult.getStatusCode());
        assertTrue(((RunnerResponse<Object>) getResult.getBody()).isSuccess());
        assertTrue(getResult.hasBody());
        assertTrue(getResult.getHeaders().isEmpty());
    }

    @Test
    void testAddAllOrganizationDataInSingleCall_Success () throws ExecutionException, InterruptedException {
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        when(masterDataUtils.createInBulkOrganizationRequest(any(), any(), anyMap(), anyString(), anyMap())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInOrganizations(anySet(), anyString())).thenReturn(new HashMap<>());
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllOrganizationDataInSingleCallResult =  customerBookingService.addAllOrganizationDataInSingleCall(customerBookingV3Response, null);
        ResponseEntity<IRunnerResponse> getResult = actualAddAllOrganizationDataInSingleCallResult.get();

        assertEquals(HttpStatus.OK, getResult.getStatusCode());
    }

    @Test
    void testAddAllOrganizationDataInSingleCall_Error () throws ExecutionException, InterruptedException {
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        when(masterDataUtils.createInBulkOrganizationRequest(any(), any(), anyMap(), anyString(), anyMap())).thenReturn(new ArrayList<>());
        when(masterDataUtils.fetchInOrganizations(anySet(), anyString())).thenThrow(new RuntimeException());
        CompletableFuture<ResponseEntity<IRunnerResponse>> actualAddAllOrganizationDataInSingleCallResult =  customerBookingService.addAllOrganizationDataInSingleCall(customerBookingV3Response, null);
        ResponseEntity<IRunnerResponse> getResult = actualAddAllOrganizationDataInSingleCallResult.get();

        assertNull(getResult);
    }

    @Test
    void testGetAllMasterData_whenShipmentExists_shouldReturnMasterDataMap() {
        // Given
        Long bookingId = 123L;

        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();
        Map<String, Object> dummyMasterData = Map.of("key1", "value1");

        when(customerBookingDao.findById(bookingId)).thenReturn(Optional.of(customerBooking));

        when(commonUtils.setIncludedFieldsToResponse(eq(customerBooking), anySet(), any(CustomerBookingV3Response.class)))
                .thenReturn(customerBookingV3Response);

        CustomerBookingV3Service spyService = Mockito.spy(customerBookingService);
        doReturn(dummyMasterData).when(spyService).fetchAllMasterDataByKey(customerBookingV3Response);

        Map<String, Object> result = spyService.getAllMasterData(bookingId);

        assertNotNull(result);
        assertEquals("value1", result.get("key1"));

        verify(customerBookingDao).findById(bookingId);
        verify(commonUtils).setIncludedFieldsToResponse(eq(customerBooking), anySet(), any(CustomerBookingV3Response.class));
        verify(spyService).fetchAllMasterDataByKey(customerBookingV3Response);
    }

    @Test
    void testFetchAllMasterDataByKey_shouldRunAllAsyncCallsAndReturnMap() {
        CustomerBookingV3Response customerBookingV3Response = new CustomerBookingV3Response();

        CustomerBookingV3Service spyService = Mockito.spy(customerBookingService);

        // Mock withMdc to return the same Runnable
        when(masterDataUtils.withMdc(any())).thenAnswer(invocation -> invocation.getArgument(0));

        // Mock all master data helper methods to just modify the map for visibility
        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("master", "ok");
            return null;
        }).when(spyService).addAllMasterDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("unlocation", "ok");
            return null;
        }).when(spyService).addAllUnlocationDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("carrier", "ok");
            return null;
        }).when(spyService).addAllCarrierDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("currency", "ok");
            return null;
        }).when(spyService).addAllCurrencyDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("tenant", "ok");
            return null;
        }).when(spyService).addAllTenantDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("containerType", "ok");
            return null;
        }).when(spyService).addAllContainerTypesInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("chargeType", "ok");
            return null;
        }).when(spyService).addAllChargeTypesInSingleMDMCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("salesAgent", "ok");
            return null;
        }).when(spyService).addAllSalesAgentInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("vessel", "ok");
            return null;
        }).when(spyService).addAllVesselDataInSingleCall(any(), any());

        doAnswer(invocation -> {
            Map<String, Object> map = invocation.getArgument(1);
            map.put("organization", "ok");
            return null;
        }).when(spyService).addAllOrganizationDataInSingleCall(any(), any());

        // Call method under test
        Map<String, Object> responseMap = spyService.fetchAllMasterDataByKey(customerBookingV3Response);

        // Validate map contains all expected keys
        assertEquals(10, responseMap.size());
        assertEquals("ok", responseMap.get("master"));
        assertEquals("ok", responseMap.get("unlocation"));
        assertEquals("ok", responseMap.get("carrier"));
        assertEquals("ok", responseMap.get("currency"));
        assertEquals("ok", responseMap.get("tenant"));
        assertEquals("ok", responseMap.get("containerType"));
        assertEquals("ok", responseMap.get("chargeType"));
        assertEquals("ok", responseMap.get("salesAgent"));
        assertEquals("ok", responseMap.get("vessel"));
        assertEquals("ok", responseMap.get("organization"));
    }

    @Test
    void testGetDefaultBooking() {
        ShipmentSettingsDetails tenantSettings = new ShipmentSettingsDetails();
        tenantSettings.setVolumeChargeableUnit("CBM");
        tenantSettings.setWeightChargeableUnit("KGS");
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(tenantSettings);

        UsersDto user = new UsersDto();
        user.setUsername("test_user");
        user.setTenantId(1);
        UserContext.setUser(user);

        mockShipmentSettings();
        CustomerBookingV3Response response = customerBookingService.getDefaultBooking();
        assertNotNull(response);
        assertEquals("CBM", response.getVolumeUnit());
        assertEquals("KGS", response.getGrossWeightUnit());
        assertEquals(1, response.getTenantId());
        assertEquals(BookingSource.Runner, response.getSource());
    }

    @Test
    void testUpdatePackingInfoInBooking() throws RunnerException {
        Packing packing = new Packing();
        packing.setBookingId(1L);
        packing.setVolume(BigDecimal.TEN);
        packing.setVolumeUnit("M3");
        packing.setWeight(BigDecimal.TEN);
        packing.setWeightUnit("KG");
        packing.setPacksType("BAG");
        Packing packing1 = new Packing();
        packing1.setBookingId(2L);
        packing1.setVolume(BigDecimal.ONE);
        packing1.setVolumeUnit("M3");
        packing1.setWeightUnit("KG");
        packing1.setPacksType("BAG");
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        when(packingDao.findByBookingIdIn(anyList())).thenReturn(List.of(packing, packing1));
        customerBookingService.updatePackingInfoInBooking(1L);
        verify(customerBookingDao, times(1)).save(any(CustomerBooking.class));
    }

    @Test
    void testUpdatePackingInfoInBookingWithNoPacking() throws RunnerException {
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        when(packingDao.findByBookingIdIn(anyList())).thenReturn(List.of());
        customerBookingService.updatePackingInfoInBooking(1L);
        verify(customerBookingDao, times(1)).save(any(CustomerBooking.class));
    }

    @Test
    void testUpdateContainerInfoInBooking() throws RunnerException {
        Containers containers = new Containers();
        containers.setContainerCode("20FR");
        containers.setContainerCount(1L);
        containers.setBookingId(2L);
        containers.setId(3L);
        containers.setPackagesPerContainer(1L);
        containers.setCargoWeightPerContainer(BigDecimal.TEN);
        containers.setContainerPackageType("BAG");
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20FR");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        when(containerDao.findByBookingIdIn(anyList())).thenReturn(List.of(containers));
        customerBookingService.updateContainerInfoInBooking(2L);
        verify(customerBookingDao, times(1)).save(any(CustomerBooking.class));
    }

    @Test
    void testUpdateContainerInfoInBookingWithNoContainers() throws RunnerException {
        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20FR");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking));
        when(containerDao.findByBookingIdIn(anyList())).thenReturn(List.of());
        customerBookingService.updateContainerInfoInBooking(2L);
        verify(customerBookingDao, times(1)).save(any(CustomerBooking.class));
    }

}