package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.BookingChargesRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.platformBooking.*;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.PlatformToRunnerCustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingSource;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
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
import org.modelmapper.ModelMapper;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;


@ExtendWith(MockitoExtension.class)
class CustomerBookingServiceTest {

    @InjectMocks
    private CustomerBookingService customerBookingService;
    @Mock
    private ICustomerBookingDao customerBookingDao;
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
    private IBookingChargesDao bookingChargesDao;

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

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }

    @Test
    void testCreateUpdatesNpmContract() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

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

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("FCL")
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

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = customerBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(customerBookingResponse), httpResponse);
    }
    @Test
    void testCreateGeneratesBookingNumberLCLCargo() throws RunnerException {
        CustomerBookingRequest request = new CustomerBookingRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CustomerBooking inputCustomerBooking = CustomerBooking.builder()
                .transportType(Constants.TRANSPORT_MODE_SEA)
                .cargoType("LCL")
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
        request.setContainersList(containersList);
        request.setPackingList(packingList);
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

        CustomerBookingResponse customerBookingResponse = objectMapper.convertValue(customerBooking, CustomerBookingResponse.class);

        // Mock
        when(customerBookingDao.findByBookingNumber(any())).thenReturn(Optional.empty());
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
        mockStatic(CommonUtils.class);
        when(CommonUtils.convertToEntityList(anyList(), eq(Packing.class))).thenReturn(customerBooking.getPackingList());
        when(CommonUtils.convertToEntityList(anyList(), eq(Routings.class))).thenReturn(customerBooking.getRoutingList());
        when(CommonUtils.convertToEntityList(anyList(), eq(Containers.class))).thenReturn(customerBooking.getContainersList());
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
        var responseEntity = customerBookingService.delete(CommonRequestModel.buildDependentDataRequest(null));
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
    void testListAsync() {
        // Mock
        var responseEntity = customerBookingService.listAsync(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertNull(responseEntity);
    }
}
