package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.request.CreditLimitRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerStatusUpdateRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingService;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.lang.reflect.InvocationTargetException;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CustomerBookingControllerTest {

    @Mock
    private ICustomerBookingService customerBookingService;
    @Mock
    private ICRPServiceAdapter crpService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private CustomerBookingController customerBookingController;

    @Test
    void create() throws RunnerException {
        // Mock
        when(customerBookingService.create(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = customerBookingController.create(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void create2() throws RunnerException {
        // Mock
        when(customerBookingService.create(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = customerBookingController.create(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void create3() throws RunnerException {
        // Mock
        when(customerBookingService.create(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = customerBookingController.create(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void platformCreateBooking() throws RunnerException {
        // Mock
        when(customerBookingService.platformCreateBooking(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(any(), eq(PlatformToRunnerCustomerBookingRequest.class))).thenReturn(new PlatformToRunnerCustomerBookingRequest());
        // Test
        var responseEntity = customerBookingController.platformCreateBooking(PlatformToRunnerCustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void platformCreateBooking2() throws RunnerException {
        // Mock
        when(customerBookingService.platformCreateBooking(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(PlatformToRunnerCustomerBookingRequest.class))).thenReturn(new PlatformToRunnerCustomerBookingRequest());
        // Test
        var responseEntity = customerBookingController.platformCreateBooking(PlatformToRunnerCustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void platformCreateBooking3() throws RunnerException {
        // Mock
        when(customerBookingService.platformCreateBooking(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(PlatformToRunnerCustomerBookingRequest.class))).thenReturn(new PlatformToRunnerCustomerBookingRequest());
        // Test
        var responseEntity = customerBookingController.platformCreateBooking(PlatformToRunnerCustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCRPService() throws RunnerException {
        // Mock
        when(crpService.listCRPService(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(any(), eq(CRPListRequest.class))).thenReturn(new CRPListRequest());
        // Test
        var responseEntity = customerBookingController.listCRPService(CRPListRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCRPService2() throws RunnerException {
        // Mock
        when(crpService.listCRPService(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(CRPListRequest.class))).thenReturn(new CRPListRequest());
        // Test
        var responseEntity = customerBookingController.listCRPService(CRPListRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void listCRPService3() throws RunnerException {
        // Mock
        when(crpService.listCRPService(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(CRPListRequest.class))).thenReturn(new CRPListRequest());
        // Test
        var responseEntity = customerBookingController.listCRPService(CRPListRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveCRPService() throws RunnerException {
        // Mock
        when(crpService.retrieveCRPService(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertValue(any(), eq(CRPRetrieveRequest.class))).thenReturn(new CRPRetrieveRequest());
        // Test
        var responseEntity = customerBookingController.retrieveCRPService(CRPRetrieveRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveCRPService2() throws RunnerException {
        // Mock
        when(crpService.retrieveCRPService(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(CRPRetrieveRequest.class))).thenReturn(new CRPRetrieveRequest());
        // Test
        var responseEntity = customerBookingController.retrieveCRPService(CRPRetrieveRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveCRPService3() throws RunnerException {
        // Mock
        when(crpService.retrieveCRPService(any())).thenThrow(new RuntimeException("RuntimeException"));
        when(jsonHelper.convertValue(any(), eq(CRPRetrieveRequest.class))).thenReturn(new CRPRetrieveRequest());
        // Test
        var responseEntity = customerBookingController.retrieveCRPService(CRPRetrieveRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        when(customerBookingService.update(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = customerBookingController.update(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void cancel() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        when(customerBookingService.cancel(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = customerBookingController.cancel(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void cancel2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        when(customerBookingService.cancel(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = customerBookingController.cancel(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void cancel3() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        when(customerBookingService.cancel(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = customerBookingController.cancel(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update2() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        when(customerBookingService.update(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = customerBookingController.update(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void update3() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        // Mock
        when(customerBookingService.update(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = customerBookingController.update(CustomerBookingRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkCreditLimitFromFusion() throws RunnerException {
        // Mock
        when(customerBookingService.checkCreditLimitFromFusion(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = customerBookingController.checkCreditLimitFromFusion(CreditLimitRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void checkCreditLimitFromFusion2() throws RunnerException {
        // Mock
        when(customerBookingService.checkCreditLimitFromFusion(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = customerBookingController.checkCreditLimitFromFusion(CreditLimitRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void checkCreditLimitFromFusion3() throws RunnerException {
        // Mock
        when(customerBookingService.checkCreditLimitFromFusion(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = customerBookingController.checkCreditLimitFromFusion(CreditLimitRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retryForBilling() throws RunnerException {
        // Mock
        when(customerBookingService.retryForBilling(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = customerBookingController.retryForBilling(123L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retryForBilling2() throws RunnerException {
        // Mock
        when(customerBookingService.retryForBilling(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = customerBookingController.retryForBilling(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retryForBilling3() throws RunnerException {
        // Mock
        when(customerBookingService.retryForBilling(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = customerBookingController.retryForBilling(123L);
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        // Mock
        when(customerBookingService.retrieveById(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = customerBookingController.retrieveById(Optional.of(111L), Optional.of(UUID.randomUUID().toString()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void list() {
        // Mock
        when(customerBookingService.list(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = customerBookingController.list(ListCommonRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void delete() {
        // Mock
        when(customerBookingService.delete(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = customerBookingController.delete(111L);
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void cloneBooking() {
        when(customerBookingService.cloneBooking(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = customerBookingController.cloneById(1L);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


}
