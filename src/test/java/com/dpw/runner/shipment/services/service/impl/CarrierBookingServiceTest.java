package com.dpw.runner.shipment.services.service.impl;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.CarrierBookingRequest;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.CarrierBookingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CarrierBookingServiceTest extends CommonMocks {

    @InjectMocks
    private CarrierBookingService carrierBookingService;
    @Mock
    private ICarrierBookingDao carrierBookingDao;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IPackingDao packingDao;
    @Mock
    private IReferenceNumbersDao referenceNumbersDao;
    @Mock
    private IBookingPaymentDao bookingPaymentDao;
    @Mock
    private IBookingCarriageDao bookingCarriageDao;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IContainerDao containerDao;
    @Mock
    private AuditLogService auditLogService;
    @Mock
    private IV1Service v1Service;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private IBookingChargesDao bookingChargesDao;
    @Mock
    private TenantSettingsService tenantSettingsService;
    @Mock
    private IPartiesDao partiesDao;
    @Mock
    private IEventDao eventDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static CarrierBookingRequest carrierBookingRequest;
    private static CarrierBooking carrierBooking;


    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().bookingPrefix("abcd").bookingNumberGeneration(1).P100Branch(false).build()
        );
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        carrierBookingRequest = jsonTestUtility.getCarrierBookingRequest();
        carrierBooking = jsonTestUtility.getCarrierBooking();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().bookingPrefix("abcd").bookingNumberGeneration(1).P100Branch(false).build());
    }


    @Test
    void testCreate() throws RunnerException {
        CarrierBookingRequest request = new CarrierBookingRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        CarrierBooking mockCarrierBooking = CarrierBooking.builder().bookingNumber("DBAR-random-string").build();
        mockCarrierBooking.setId(1L);
        CarrierBookingResponse carrierBookingResponse = objectMapper.convertValue(mockCarrierBooking, CarrierBookingResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(new CarrierBooking());
        when(carrierBookingDao.save(any())).thenReturn(mockCarrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = carrierBookingService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(carrierBookingResponse), httpResponse);
    }

    @Test
    void testDelete() {
        // Mock
        when(carrierBookingDao.findById(anyLong())).thenReturn(Optional.of(carrierBooking));
        doNothing().when(carrierBookingDao).delete(any());
        var responseEntity = carrierBookingService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDelete2() {
        var responseEntity = carrierBookingService.delete(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete3() {
        var responseEntity = carrierBookingService.delete(CommonRequestModel.buildDependentDataRequest(CommonGetRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete5() {
        // Mock
        when(carrierBookingDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = carrierBookingService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete6() {
        // Mock
        when(carrierBookingDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = carrierBookingService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateWithNullException() throws RunnerException {
        // Test
        Throwable t = assertThrows(Throwable.class, () -> carrierBookingService.create(CommonRequestModel.buildRequest()));
        // Assert
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testListWithEmptyRequest() {
        // Test
        var responseEntity = carrierBookingService.list(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithEmptyException() {
        when(carrierBookingDao.findAll(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = carrierBookingService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithNoResult() {
        when(carrierBookingDao.findAll(any(), any())).thenReturn(Page.empty());
        // Test
        var responseEntity = carrierBookingService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertTrue(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testListWithSuccessResult() {
        when(carrierBookingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(carrierBooking)));
        when(modelMapper.map(any(), eq(CarrierBookingResponse.class))).thenReturn(objectMapper.convertValue(carrierBooking, CarrierBookingResponse.class));
        // Test
        var responseEntity = carrierBookingService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testDeleteWithEmptyRequest() {
        when(carrierBookingDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(carrierBooking)));
        when(modelMapper.map(any(), eq(CarrierBookingResponse.class))).thenReturn(objectMapper.convertValue(carrierBooking, CarrierBookingResponse.class));
        // Test
        var responseEntity = carrierBookingService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testRetrieveByIdWithEmptyRequest() {
        var responseEntity = carrierBookingService.retrieveById(CommonRequestModel.buildRequest());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithEmptyIdRequest() {
        var responseEntity = carrierBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithIdNotPresent() {
        when(carrierBookingDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = carrierBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithExceptionInFindById() {
        when(carrierBookingDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = carrierBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithSuccessResponse() {
        when(carrierBookingDao.findById(anyLong())).thenReturn(Optional.of(carrierBooking));
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(objectMapper.convertValue(carrierBooking, CarrierBookingResponse.class));
        var responseEntity = carrierBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByGUIdWithSuccessResponse() {
        when(carrierBookingDao.findByGuid(any())).thenReturn(Optional.of(carrierBooking));
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(objectMapper.convertValue(carrierBooking, CarrierBookingResponse.class));
        var responseEntity = carrierBookingService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid("69281519-f079-4dfc-99fd-970c40bd14a9").build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateWithCompletePayload() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputCarrierBooking = carrierBooking;

        var container = Containers.builder().build();
        container.setGuid(UUID.randomUUID());
        inputCarrierBooking.setContainersList(Collections.singletonList(container));

        var consolidationAddress = Parties.builder().build();
        inputCarrierBooking.setConsolidationAddresses(Collections.singletonList(consolidationAddress));

        CarrierBookingRequest request = objectMapper.convertValue(inputCarrierBooking, CarrierBookingRequest.class);
        CarrierBookingResponse carrierBookingResponse = objectMapper.convertValue(inputCarrierBooking, CarrierBookingResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(inputCarrierBooking);
        when(carrierBookingDao.save(any())).thenReturn(inputCarrierBooking);
        when(jsonHelper.convertValue(any(), eq(CarrierBookingResponse.class))).thenReturn(carrierBookingResponse);
        Optional<Long> consolidationId = Optional.of(123L);
        when(consolidationDetailsDao.findIdByGuid(any())).thenReturn(consolidationId);
        mockTenantSettings();
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = carrierBookingService.create(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testUpdateWithCompletePayload() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputCarrierBooking = carrierBooking;

        var container = Containers.builder().build();
        container.setGuid(UUID.randomUUID());
        inputCarrierBooking.setContainersList(Collections.singletonList(container));

        var consolidationAddress = Parties.builder().build();
        inputCarrierBooking.setConsolidationAddresses(Collections.singletonList(consolidationAddress));

        CarrierBookingRequest request = objectMapper.convertValue(inputCarrierBooking, CarrierBookingRequest.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(CarrierBooking.class))).thenReturn(inputCarrierBooking);
        when(carrierBookingDao.save(any())).thenReturn(inputCarrierBooking);
        when(carrierBookingDao.findById(any())).thenReturn(Optional.of(inputCarrierBooking));
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = carrierBookingService.update(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testBookingUpdateNullRequest() {
        // test
        var t = assertThrows(Throwable.class, () -> carrierBookingService.update(CommonRequestModel.builder().build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testBookingUpdateNullRequestId() {
        // test
        var t = assertThrows(Throwable.class, () -> carrierBookingService.update(CommonRequestModel.builder().data(new CarrierBookingRequest()).build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testBookingUpdateWithIdDoesNotExists() {
        // mock
        when(carrierBookingDao.findById(any())).thenReturn(Optional.empty());
        // test
        var t = assertThrows(Throwable.class, () -> carrierBookingService.update(CommonRequestModel.builder().data(carrierBookingRequest).build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, t.getMessage());
    }

}
