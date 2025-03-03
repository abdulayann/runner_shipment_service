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
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.platformBooking.*;
import com.dpw.runner.shipment.services.dto.response.CheckCreditBalanceFusionResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.anyLong;
import static org.mockito.Mockito.*;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CustomerBookingServiceTest extends CommonMocks {

    @InjectMocks
    private CustomerBookingService customerBookingService;
    @Mock
    private ICustomerBookingDao customerBookingDao;
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
    private IContainerDao containerDao;
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
    private KafkaProducer producer;
    @Mock
    private OrderManagementAdapter orderManagementAdapter;
    @Mock
    private IQuoteContractsService quoteContractsService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static CustomerBookingRequest customerBookingRequest;
    private static CustomerBooking customerBooking;


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
        customerBookingRequest = jsonTestUtility.getCustomerBookingRequest();
        customerBooking = jsonTestUtility.getCustomerBooking();
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().hasNoUtilization(true).build());
    }


    @Test
    void testCreate() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(new CustomerBooking());
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateUpdatesNpmContract() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
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
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        verify(npmService, times(1)).updateContracts(any());
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberFCLCargo() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
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
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(quoteContractsService.getQuoteContractsByContractId(anyString())).thenReturn(QuoteContracts.builder().build());
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberFTLCargo() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
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
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(quoteContractsService.getQuoteContractsByContractId(anyString())).thenReturn(null);
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberLCLCargo() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
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

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("LCL")
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBLC-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(quoteContractsService.getQuoteContractsByContractId(anyString())).thenReturn(quoteContracts);
        when(npmService.updateContracts(any())).thenReturn(new ResponseEntity<>(HttpStatus.OK));
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberBBKCargo() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
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
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(quoteContractsService.getQuoteContractsByContractId(anyString())).thenReturn(quoteContracts);
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberRORCargo() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
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
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateGeneratesBookingNumberLTLCargo() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
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
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testPlatformCreateBooking() throws RunnerException {
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

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBookingRequest customerBookingRequest = new CustomerBookingRequest();
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setId(1L);
        customerBooking.setBookingNumber("DBAR-5586091-311749");
        customerBooking.setIsConsigneeAddressFreeText(customerBooking.getIsConsigneeFreeText() != null && customerBooking.getIsConsigneeFreeText());
        customerBooking.setIsConsignorAddressFreeText(customerBooking.getIsConsignorFreeText() != null && customerBooking.getIsConsignorFreeText());
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(customerBooking.getIsNotifyPartyFreeText() != null && customerBooking.getIsNotifyPartyFreeText());
        customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);

        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingResponse.class);

        // Mock
        when(customerBookingDao.findByBookingNumber(any())).thenReturn(Optional.empty());
        when(modelMapper.map(any(), eq(CustomerBookingRequest.class))).thenReturn(customerBookingRequest);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(v1Service.fetchVesselData(any())).thenReturn(V1DataResponse.builder().entities(List.of()).build());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(jsonHelper.convertValueToList(any(), eq(VesselsResponse.class))).thenReturn(List.of(new VesselsResponse()));
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.platformCreateBooking(commonRequestModel);

        // Assert
        verify(customerBookingDao, times(1)).save(customerBooking);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());

    }

    @Test
    void testPlatformCreateBooking2() throws RunnerException {
        PlatformToRunnerCustomerBookingRequest request = new PlatformToRunnerCustomerBookingRequest();
        var route = BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("UIHJK").build();
        route.setGuid(UUID.randomUUID());
        var routingList = List.of(
                BookingRoutingsRequest.builder().reference_id(UUID.randomUUID().toString()).carrier("APLU").build(),
                route
        );
        request.setContainersList(List.of());
        request.setPackingList(List.of());
        request.setRoutingList(routingList);
        request.setBookingCharges(List.of());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBookingRequest customerBookingRequest = new CustomerBookingRequest();
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setId(1L);
        customerBooking.setBookingNumber("DBAR-5586091-311749");
        customerBooking.setIsConsigneeAddressFreeText(customerBooking.getIsConsigneeFreeText() != null && customerBooking.getIsConsigneeFreeText());
        customerBooking.setIsConsignorAddressFreeText(customerBooking.getIsConsignorFreeText() != null && customerBooking.getIsConsignorFreeText());
        customerBooking.setIsCustomerAddressFreeText(false);
        customerBooking.setIsNotifyPartyAddressFreeText(customerBooking.getIsNotifyPartyFreeText() != null && customerBooking.getIsNotifyPartyFreeText());
        customerBooking.setSource(BookingSource.Platform);
        customerBooking.setIsPlatformBookingCreated(Boolean.TRUE);
        customerBooking.setCarrierDetails(CarrierDetails.builder().build());

        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingResponse.class);

        // Mock
        when(customerBookingDao.findByBookingNumber(any())).thenReturn(Optional.of(customerBooking));
        when(modelMapper.map(any(), eq(CustomerBookingRequest.class))).thenReturn(customerBookingRequest);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.platformCreateBooking(commonRequestModel);

        // Assert
        verify(customerBookingDao, times(1)).save(customerBooking);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());

    }

    @Test
    void testRetryForBillingFailsWhenCustomerBookingIsNotPresent() {
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking customerBooking = new CustomerBooking();
        String errorMessage = DaoConstants.DAO_DATA_RETRIEVAL_FAILURE;

        // Mock
        when(customerBookingDao.findById(id)).thenReturn(Optional.empty());

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.retryForBilling(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());

    }

    @Test
    void testRetryForBillingFailsWhenStatusNotReadyForShipment() {
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking customerBooking = new CustomerBooking();
        String errorMessage = String.format("Booking should be in: %s stage for this operation", BookingStatus.READY_FOR_SHIPMENT);

        // Mock
        when(customerBookingDao.findById(id)).thenReturn(Optional.of(customerBooking));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.retryForBilling(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void testRetryForBillingFailsWhenBillIsCreated() {
        Long id = 1L;
        CommonGetRequest request = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        customerBooking.setIsBillCreated(true);
        String errorMessage = String.format("Bill is already created for booking with id: %s", request.getId());

        // Mock
        when(customerBookingDao.findById(id)).thenReturn(Optional.of(customerBooking));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.retryForBilling(commonRequestModel);

        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }
    @Test
    void testRetryForBillingSuccess() {
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
//        when(customerBookingDao.save(any())).thenReturn(customerBooking.setId(1L));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.retryForBilling(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(shipmentCreationResponse), httpResponse);
    }


    @Test
    void testCreateGeneratesBookingNumberForNullRequest() throws RunnerException {
        // Test
        Throwable t = assertThrows(Throwable.class, () -> customerBookingService.create(CommonRequestModel.buildDependentDataRequest(null)));
        // Assert
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testCreateGeneratesBookingNumberForCreateCompleteRequest() throws RunnerException {
        CustomerBookingRequest request = customerBookingRequest;

        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(request, CustomerBookingResponse.class);
        CustomerBooking customerBooking = objectMapper.convertValue(request, CustomerBooking.class);
        var bookingCharge = customerBooking.getBookingCharges().get(0);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(jsonHelper.convertValue(any(), eq(BookingCharges.class))).thenReturn(bookingCharge);
        mockShipmentSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(CommonRequestModel.buildRequest(request));

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }


    @Test
    void testDelete() {
        // Mock
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(customerBooking));
        doNothing().when(customerBookingDao).delete(any());
        var responseEntity = customerBookingService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDelete2() {
        var responseEntity = customerBookingService.delete(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete3() {
        var responseEntity = customerBookingService.delete(CommonRequestModel.buildDependentDataRequest(CommonGetRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete4() {
        // Mock
        var responseEntity = customerBookingService.delete(CommonRequestModel.buildDependentDataRequest(CommonGetRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete5() {
        // Mock
        when(customerBookingDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = customerBookingService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete6() {
        // Mock
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = customerBookingService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }


    @Test
    void testListAsync() {
        // Mock
        var responseEntity = customerBookingService.listAsync(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertNull(responseEntity);
    }

    @Test
    void testCreateWithException() throws RunnerException {
        when(customerBookingDao.save(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        mockShipmentSettings();
        // Test
        Throwable t = assertThrows(Throwable.class, () -> customerBookingService.create(CommonRequestModel.buildRequest(new CustomerBookingRequest())));
        // Assert
        assertEquals("RuntimeException", t.getMessage());
        assertEquals(RunnerException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testCreateWithNullException() throws RunnerException {
        // Test
        Throwable t = assertThrows(Throwable.class, () -> customerBookingService.create(CommonRequestModel.buildRequest()));
        // Assert
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testListWithEmptyRequest() {
        // Test
        var responseEntity = customerBookingService.list(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithEmptyException() {
        when(customerBookingDao.findAll(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = customerBookingService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithNoResult() {
        when(customerBookingDao.findAll(any(), any())).thenReturn(Page.empty());
        // Test
        var responseEntity = customerBookingService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertTrue(((RunnerListResponse)responseEntity.getBody()).getData().isEmpty());
    }

    @Test
    void testListWithSuccessResult() {
        when(customerBookingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(customerBooking)));
        when(modelMapper.map(any(), eq(CustomerBookingResponse.class))).thenReturn(objectMapper.convertValue(customerBooking, CustomerBookingResponse.class));
        // Test
        var responseEntity = customerBookingService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse)responseEntity.getBody()).getData().isEmpty());
    }

    @Test
    void testDeleteWithEmptyRequest() {
        when(customerBookingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(customerBooking)));
        when(modelMapper.map(any(), eq(CustomerBookingResponse.class))).thenReturn(objectMapper.convertValue(customerBooking, CustomerBookingResponse.class));
        // Test
        var responseEntity = customerBookingService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse)responseEntity.getBody()).getData().isEmpty());
    }

    @Test
    void testRetrieveByIdWithEmptyRequest() {
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithEmptyIdRequest() {
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithIdNotPresent() {
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithExceptionInFindById() {
        when(customerBookingDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithSuccessResponse() {
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(customerBooking));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(objectMapper.convertValue(customerBooking, CustomerBookingResponse.class));
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2OnWithShipmentGuidNotNull() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(true).build());
        var inputBooking = customerBooking;
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentGuid(UUID.randomUUID().toString());
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingResponse.class));
        mockTenantSettings();
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2OnWithShipmentGuidNotNull2() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(true).build());
        var inputBooking = customerBooking;
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentGuid(UUID.randomUUID().toString());
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingResponse.class));
        mockTenantSettings();
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2OnWithShipmentGuidNull() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(true).build());
        var inputBooking = customerBooking;
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentEntityId("123");
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());
        when(v1Service.getShipment(any())).thenReturn(V1RetrieveResponse.builder().entity(ShipmentRetrieveResponse.builder().guid(UUID.randomUUID()).build()).build());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingResponse.class));
        mockTenantSettings();
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2OnWithShipmentGuidNull2() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().ShipmentServiceV2Enabled(true).build());
        var inputBooking = customerBooking;
        inputBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        inputBooking.setShipmentEntityId("123");
        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(inputBooking));
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(v1Service.getShipment(any())).thenReturn(V1RetrieveResponse.builder().entity(ShipmentRetrieveResponse.builder().guid(UUID.randomUUID()).build()).build());
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingResponse.class));
        mockTenantSettings();
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithReadyForShipmentWithV2Off() {
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
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(objectMapper.convertValue(inputBooking, CustomerBookingResponse.class));
        mockTenantSettings();
        var responseEntity = customerBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateWithCompletePayload() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputCustomerBooking = customerBooking;

        var container = Containers.builder().build();
        container.setGuid(UUID.randomUUID());
        inputCustomerBooking.setContainersList(
                Arrays.asList(container)
        );
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        request.getBookingCharges().get(0).setContainersUUID(List.of(container.getGuid()));
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(BookingCharges.class))).thenReturn(new BookingCharges());
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getContainersList());
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(CommonRequestModel.buildRequest(request));

        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testCreditCheckFailure() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(false)
                        .IsCreditLimitWithFusionEnabled(false)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreditCheckFailure2() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(false)
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreditCheckFailure3() throws RunnerException {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .EnableCreditLimitManagement(true)
                        .IsCreditLimitWithFusionEnabled(true)
                        .RestrictedItemsForCreditLimit(List.of())
                        .build());
        CreditLimitRequest creditLimitRequest = new CreditLimitRequest();
        mockTenantSettings();
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreditCheckWithCreditLimitWith0Value() throws RunnerException {
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
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreditCheckWithCreditLimitWith0Value2() throws RunnerException {
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
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreditCheckWithCreditLimitWith1Value() throws RunnerException {
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
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreditCheckWithCreditLimitWith1ValueWithGlobalFusionOn() throws RunnerException {
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
        var responseEntity = customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreditCheckWithCreditLimitWith1ValueWithGlobalFusionOn2() throws RunnerException {
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
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(RuntimeException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreditCheckWithCreditLimitWith1ValueWithGlobalFusionOn3() throws RunnerException {
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
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testCreditCheckWithCreditLimitWith1ValueWithGlobalFusionOn4() throws RunnerException {
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
        var t = assertThrows(Throwable.class, () -> customerBookingService.checkCreditLimitFromFusion(CommonRequestModel.buildRequest(creditLimitRequest)));
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testBookingUpdateNullRequest() {
        // test
        var t = assertThrows(Throwable.class, () -> customerBookingService.update(CommonRequestModel.builder().build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testBookingUpdateNullRequestId() {
        // test
        var t = assertThrows(Throwable.class, () -> customerBookingService.update(CommonRequestModel.builder().data(new CustomerBookingRequest()).build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testBookingUpdateWithIdDoesNotExists() {
        // mock
        when(customerBookingDao.findById(any())).thenReturn(Optional.empty());
        // test
        var t = assertThrows(Throwable.class, () -> customerBookingService.update(CommonRequestModel.builder().data(customerBookingRequest).build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, t.getMessage());
    }

    @Test
    void testCancel() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        CustomerBookingRequest request = CustomerBookingRequest.builder().id(1l).bookingStatus(BookingStatus.CANCELLED).build();

        var mockBookingEntity = customerBooking;
        mockBookingEntity.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        // mock
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(mockBookingEntity));

        var t = assertThrows(Throwable.class, () -> customerBookingService.cancel(CommonRequestModel.builder().data(request).build()));
        // assert
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }


    @Test
    void testBookingUpdateWithReadyForShipment() {
        var mockBookingEntity = customerBooking;
        mockBookingEntity.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        // mock
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(mockBookingEntity));
        // test
        var t = assertThrows(Throwable.class, () -> customerBookingService.update(CommonRequestModel.builder().data(customerBookingRequest).build()));
        // assert
        assertEquals(ValidationException.class.getSimpleName(), t.getClass().getSimpleName());
    }

    @Test
    void testBookingUpdateWithSuccess() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);

        var container = Containers.builder().build();
        container.setGuid(UUID.randomUUID());
        inputCustomerBooking.setContainersList(
                Arrays.asList(container)
        );
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        request.getBookingCharges().get(0).setContainersUUID(List.of(container.getGuid()));
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(inputCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getContainersList());
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testBookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentEnabled() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .build()
        );
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setGuid(UUID.randomUUID());
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getContainersList());
        when(bookingIntegrationsUtility.createShipmentInV2(any())).thenReturn(ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().guid(UUID.randomUUID()).build()));
        mockTenantSettings();
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        //verify(mdmServiceAdapter,times(1)).createShipmentTaskFromBooking(any());
    }

    @Test
    void testBookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentEnabled_AirShipment_AIRDGPermission_AirDgTrue_AirPermissionTrue_CreatesBooking() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .build()
        );
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setGuid(UUID.randomUUID());
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        request.setTransportType("AIR");
        request.setIsDg(true);
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.airDG, true));
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getContainersList());
        when(bookingIntegrationsUtility.createShipmentInV2(any())).thenReturn(ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().guid(UUID.randomUUID()).build()));
        mockTenantSettings();
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        //verify(mdmServiceAdapter,times(1)).createShipmentTaskFromBooking(any());
    }

    @Test
    void testBookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentEnabled_AirShipment_AIRDGPermission_AirDgTrue_AirPermissionFalse_throwsException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .build()
        );
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setGuid(UUID.randomUUID());
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        request.setTransportType("AIR");
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.airDG, false));
        request.setIsDg(true);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
//        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getContainersList());
//        when(bookingIntegrationsUtility.createShipmentInV2(any())).thenReturn(ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().guid(UUID.randomUUID()).build()));
        mockTenantSettings();
        mockShipmentSettings();
        // Test
        var req = CommonRequestModel.builder().data(request).build();
        assertThrows(ValidationException.class, () -> customerBookingService.update(req));
    }

    @Test
    void testBookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentEnabled_AIRDGPermission_AirDgFalse_AirPermissionTrue_throwsException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .build()
        );
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setGuid(UUID.randomUUID());
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        request.setTransportType("AIR");
        request.setIsDg(false);
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.airDG, true));
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
//        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getContainersList());
//        when(bookingIntegrationsUtility.createShipmentInV2(any())).thenReturn(ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().guid(UUID.randomUUID()).build()));
        mockTenantSettings();
        mockShipmentSettings();
        // Test
        var req = CommonRequestModel.builder().data(request).build();
        assertThrows(NullPointerException.class, () -> customerBookingService.update(req));
    }

    @Test
    void testBookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentEnabled_AIRDGPermission_AirDgFalse_AirPermissionFalse_throwsException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(true)
                        .build()
        );
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setGuid(UUID.randomUUID());
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        request.setTransportType("AIR");
        request.setIsDg(false);
        UserContext.getUser().setPermissions(Map.of(PermissionConstants.airDG, false));
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
//        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(containerDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getContainersList());
//        when(bookingIntegrationsUtility.createShipmentInV2(any())).thenReturn(ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().guid(UUID.randomUUID()).build()));
        mockTenantSettings();
        mockShipmentSettings();
        // Test
        var req = CommonRequestModel.builder().data(request).build();
        assertThrows(NullPointerException.class, () -> customerBookingService.update(req));
    }

    @Test
    void testBookingUpdateWithSuccessWithReadyForShipmentWithV2ShipmentDisabled() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder()
                        .ShipmentServiceV2Enabled(false)
                        .build()
        );
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        request.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        var mockV1ShipmentCreationResponse = new V1ShipmentCreationResponse();
        mockV1ShipmentCreationResponse.setShipmentId("123");
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(jsonHelper.convertValue(any(), eq(V1ShipmentCreationResponse.class))).thenReturn(mockV1ShipmentCreationResponse);
        when(bookingIntegrationsUtility.createShipmentInV1(any(), anyBoolean(), anyBoolean(), any(), any())).thenReturn(ResponseEntity.ok(mockV1ShipmentCreationResponse));
        mockTenantSettings();
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void testBookingUpdateWithSuccessCreateInPlatform() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(bookingChargesDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getBookingCharges());
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testBookingUpdateWithSuccessUpdateInPlatform() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        var inputCustomerBooking = customerBooking;
        inputCustomerBooking.setIsPlatformBookingCreated(true);
        inputCustomerBooking.setBookingStatus(BookingStatus.PENDING_FOR_CREDIT_LIMIT);
        CustomerBookingRequest request = objectMapper.convertValue(inputCustomerBooking, CustomerBookingRequest.class);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(inputCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(inputCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(inputCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(objectMapper.convertValue(request, CustomerBooking.class));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        when(bookingChargesDao.updateEntityFromBooking(anyList(), anyLong())).thenReturn(inputCustomerBooking.getBookingCharges());
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testBookingUpdateWithUtilization() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var oldCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        var newCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);;
        oldCustomerBooking.setContractId("old");
        newCustomerBooking.setContractId("new");
        newCustomerBooking.setBookingStatus(BookingStatus.CANCELLED);
        CustomerBookingRequest request = objectMapper.convertValue(newCustomerBooking, CustomerBookingRequest.class);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(newCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(newCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(oldCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(newCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testBookingUpdateWithUtilization2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var oldCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        var newCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);;
        oldCustomerBooking.setContractId(null);
        newCustomerBooking.setContractId("new");
        CustomerBookingRequest request = objectMapper.convertValue(newCustomerBooking, CustomerBookingRequest.class);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(newCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(newCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(oldCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(newCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testBookingUpdateWithUtilization3() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Arrange
        ShipmentSettingsDetailsContext.getCurrentTenantSettings().setIsAlwaysUtilization(true).setHasNoUtilization(false);
        var oldCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        var newCustomerBooking = objectMapper.convertValue(customerBooking, CustomerBooking.class);
        oldCustomerBooking.setContractId("old");
        newCustomerBooking.setContractId(null);
        CustomerBookingRequest request = objectMapper.convertValue(newCustomerBooking, CustomerBookingRequest.class);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(newCustomerBooking, CustomerBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(newCustomerBooking);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(oldCustomerBooking));
        when(customerBookingDao.save(any())).thenReturn(newCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();
        // Test
        var responseEntity = customerBookingService.update(CommonRequestModel.builder().data(request).build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testPlatformCreateBooking3() throws RunnerException {
        PlatformToRunnerCustomerBookingRequest request = new PlatformToRunnerCustomerBookingRequest();
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
        CustomerBookingRequest customerBookingRequest = new CustomerBookingRequest();
        customerBookingRequest.setContainersList(List.of(
                ContainerRequest.builder().guid(UUID.randomUUID()).id(123L).containerCode("20GP").build(),
                ContainerRequest.builder().guid(UUID.fromString("9f9c2d42-d479-44bf-8541-0029e498fc86")).build()
        ));
        customerBookingRequest.setPackingList(List.of(
                PackingRequest.builder().guid(UUID.randomUUID()).build(),
                PackingRequest.builder().guid(UUID.fromString("9f9c2d42-d479-44bf-8541-0029e498fc88")).build()
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


        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingResponse.class);
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
        when(modelMapper.map(any(), eq(CustomerBookingRequest.class))).thenReturn(customerBookingRequest);
        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(customerBooking);
        when(customerBookingDao.save(any())).thenReturn(customerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.platformCreateBooking(commonRequestModel);

        // Assert
        verify(customerBookingDao, times(1)).save(customerBooking);
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testClone()
    {
        CustomerBooking customerBooking1 = customerBooking;
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingResponse.class);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking1));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(CommonGetRequest.builder().id(1L).build()).build();
        customerBookingService.cloneBooking(commonRequestModel);
        assertNotNull(commonRequestModel);
    }

    @Test
    void testClone2()
    {
        CustomerBooking customerBooking1 = new CustomerBooking();
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(customerBooking1, CustomerBookingResponse.class);
        when(customerBookingDao.findById(any())).thenReturn(Optional.of(customerBooking1));
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(CommonGetRequest.builder().id(1L).build()).build();
        customerBookingService.cloneBooking(commonRequestModel);
        assertNotNull(commonRequestModel);
    }

    @Test
    void testClone3()
    {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(CommonGetRequest.builder().id(null).build()).build();
        customerBookingService.cloneBooking(commonRequestModel);
        assertNotNull(commonRequestModel);
    }

    @Test
    void testClone4()
    {
        when(customerBookingDao.findById(any())).thenReturn(Optional.empty());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(CommonGetRequest.builder().id(1L).build()).build();
        customerBookingService.cloneBooking(commonRequestModel);
        assertNotNull(commonRequestModel);
    }

    @Test
    void testCheckForCrediLimitManagement_returnsTrue() throws JsonProcessingException, RunnerException {
        CustomerBooking customerBooking1 = customerBooking;
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Approved");
        var result = customerBookingService.checkForCreditLimitManagement(customerBooking1);
        assertTrue(result);
    }

    @Test
    void testCheckForCrediLimitManagement_finalStatusInit_returnsFalse() throws JsonProcessingException, RunnerException {
        CustomerBooking customerBooking1 = customerBooking;
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn("Init");
        var result = customerBookingService.checkForCreditLimitManagement(customerBooking1);
        assertFalse(result);
    }

    @Test
    void testCheckForCrediLimitManagement_nullString_returnsFalse() throws JsonProcessingException, RunnerException {
        CustomerBooking customerBooking1 = customerBooking;
        when(mdmServiceAdapter.getApprovalStausForParties(any())).thenReturn(null);
        var result = customerBookingService.checkForCreditLimitManagement(customerBooking1);
        assertFalse(result);
    }

    @Test
    void testCreateKafkaEventGuidNull() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
        request.setOrderManagementNumber("Odn1");
        request.setOrderManagementId("Od1");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking mockCustomerBooking = CustomerBooking.builder()
                .source(BookingSource.Runner)
                .isPlatformBookingCreated(false)
                .bookingNumber("DBAR-random-string")
                .build();
        mockCustomerBooking.setId(1L);
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(new CustomerBooking());
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();

        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateKafkaEvent() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
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
        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(mockCustomerBooking, CustomerBookingResponse.class);

        when(jsonHelper.convertValue(any(), eq(CustomerBooking.class))).thenReturn(new CustomerBooking());
        when(customerBookingDao.save(any())).thenReturn(mockCustomerBooking);
        when(jsonHelper.convertValue(any(), eq(CustomerBookingResponse.class))).thenReturn(customerBookingResponse);
        mockShipmentSettings();

        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }
}
