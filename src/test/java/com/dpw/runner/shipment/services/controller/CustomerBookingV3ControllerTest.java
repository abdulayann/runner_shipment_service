package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.ICRPServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1ShipmentCreationResponse;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;

import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {CustomerBookingV3Controller.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CustomerBookingV3ControllerTest {

    @Mock
    private IContainerV3Service containerV3Service;
    @Mock
    private IPackingV3Service packingV3Service;
    @Mock
    private IReferenceNumbersV3Service referenceNumbersV3Service;
    @Mock
    private IPartiesV3Service partiesV3Service;
    @Mock
    private ICustomerBookingV3Service customerBookingV3Service;
    @Mock
    private ICRPServiceAdapter crpService;

    @InjectMocks
    private CustomerBookingV3Controller customerBookingV3Controller;

    @Test
    void createBooking_Success() throws RunnerException {
        when(customerBookingV3Service.create(any())).thenReturn(new CustomerBookingV3Response());
        var response = customerBookingV3Controller.createBooking(new CustomerBookingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateBooking_Success() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        when(customerBookingV3Service.update(any())).thenReturn(new CustomerBookingV3Response());
        var response = customerBookingV3Controller.updateBooking(new CustomerBookingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteBooking_Success() throws RunnerException {
        when(customerBookingV3Service.delete(any())).thenReturn(new CustomerBookingV3DeleteResponse());
        var response = customerBookingV3Controller.deleteBooking(1L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void retrieveById_Success() throws RunnerException {
        when(customerBookingV3Service.retrieveById(any())).thenReturn(new CustomerBookingV3Response());
        var response = customerBookingV3Controller.retrieveById(Optional.of(1L), Optional.empty());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void list_Success() throws RunnerException {
        CustomerBookingV3ListResponse customerBookingV3ListResponse = new CustomerBookingV3ListResponse();
        customerBookingV3ListResponse.setTotalPages(10);
        customerBookingV3ListResponse.setTotalCount(247L);
        when(customerBookingV3Service.list(any(), any())).thenReturn(customerBookingV3ListResponse);
        var response = customerBookingV3Controller.list(new ListCommonRequest(), Boolean.TRUE);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void cancelBooking_Success() throws Exception {
        when(customerBookingV3Service.update(any())).thenReturn(new CustomerBookingV3Response());
        var response = customerBookingV3Controller.cancel(new CustomerBookingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void cloneById_Success() throws RunnerException {
        when(customerBookingV3Service.cloneBooking(anyLong())).thenReturn(new CustomerBookingV3Response());
        var response = customerBookingV3Controller.cloneById(123L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void retrieveByOrderId_Success() throws RunnerException {
        when(customerBookingV3Service.retrieveByOrderId(anyString())).thenReturn(new CustomerBookingV3Response());
        var response = customerBookingV3Controller.retrieveByOrderId("ORD123");
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void platformCreateBooking_Success() throws RunnerException {
        when(customerBookingV3Service.platformCreateBooking(any())).thenReturn(new PlatformToRunnerCustomerBookingResponse());
        var response = customerBookingV3Controller.platformCreateBooking(new PlatformToRunnerCustomerBookingRequest());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void listCRPService_Success() throws RunnerException {
        var mockResponse = new ResponseEntity<IRunnerResponse>(new RunnerResponse<>(), HttpStatus.OK);
        when(crpService.listCRPService(any())).thenReturn(mockResponse);
        var response = customerBookingV3Controller.listCRPService(new CRPListRequest());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void retrieveCRPService_Success() throws RunnerException {
        var mockResponse = new ResponseEntity<IRunnerResponse>(new RunnerResponse<>(), HttpStatus.OK);
        when(crpService.retrieveCRPService(any())).thenReturn(mockResponse);
        var response = customerBookingV3Controller.retrieveCRPService(new CRPRetrieveRequest());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createBookingContainers() throws RunnerException {
        when(containerV3Service.create(any(), any())).thenReturn(new ContainerResponse());
        var responseEntity = customerBookingV3Controller.createBookingContainers(new ContainerV3Request());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateBookingContainers() throws RunnerException {
        when(containerV3Service.updateBulk(any(), any())).thenReturn(new BulkContainerResponse());
        var responseEntity = customerBookingV3Controller.updateBookingContainers(List.of(new ContainerV3Request()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getBookingContainers() throws RunnerException {
        when(containerV3Service.list(any(), anyBoolean(), any())).thenReturn(new ContainerListResponse());
        var responseEntity = customerBookingV3Controller.listBookingContainers(new ListCommonRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void deleteBookingContainers() {
        when(containerV3Service.deleteBulk(any(), any())).thenReturn(new BulkContainerResponse());
        var responseEntity = customerBookingV3Controller.deleteBookingContainers(List.of(new ContainerV3Request()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createBookingPackages() throws RunnerException {
        when(packingV3Service.create(any(), any())).thenReturn(new PackingResponse());
        var responseEntity = customerBookingV3Controller.createBookingPackages(new PackingV3Request());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateBookingPackages() throws RunnerException {
        when(packingV3Service.updateBulk(any(), any())).thenReturn(new BulkPackingResponse());
        var responseEntity = customerBookingV3Controller.updateBookingPackages(List.of(new PackingV3Request()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getBookingPackages() {
        when(packingV3Service.list(any(), anyBoolean(), any())).thenReturn(new PackingListResponse());
        var responseEntity = customerBookingV3Controller.listBookingPackages(new ListCommonRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void deleteBookingPackages() throws RunnerException {
        when(packingV3Service.deleteBulk(any(),any())).thenReturn(new BulkPackingResponse());
        var responseEntity = customerBookingV3Controller.deleteBookingPackages(List.of(new PackingV3Request()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createBookingReferences() {
        when(referenceNumbersV3Service.create(any())).thenReturn(new ReferenceNumbersResponse());
        var responseEntity = customerBookingV3Controller.createBookingReferences(new ReferenceNumbersRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateBookingReferences() {
        when(referenceNumbersV3Service.update(any())).thenReturn(new ReferenceNumbersResponse());
        var responseEntity = customerBookingV3Controller.updateBookingReferences(new ReferenceNumbersRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void deleteBookingReferences() {
        when(referenceNumbersV3Service.delete(any())).thenReturn(new ReferenceNumbersResponse());
        var responseEntity = customerBookingV3Controller.deleteBookingReferences(new ReferenceNumbersRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listBookingReferences() {
        when(referenceNumbersV3Service.list(any(), any())).thenReturn(new ArrayList<>());
        var responseEntity = customerBookingV3Controller.listBookingReferences(new ListCommonRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void createBookingParties() {
        when(partiesV3Service.create(any())).thenReturn(new PartiesResponse());
        var responseEntity = customerBookingV3Controller.createBookingParties(new PartiesRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateBookingParties() {
        when(partiesV3Service.update(any())).thenReturn(new PartiesResponse());
        var responseEntity = customerBookingV3Controller.updateBookingParties(new PartiesRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void deleteBookingParties() {
        when(partiesV3Service.delete(any())).thenReturn(new PartiesResponse());
        var responseEntity = customerBookingV3Controller.deleteBookingParties(new PartiesRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listBookingParties() {
        when(partiesV3Service.list(any())).thenReturn(new ArrayList<>());
        var responseEntity = customerBookingV3Controller.listBookingParties(new ListCommonRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retryForBilling() throws RunnerException {
        when(customerBookingV3Service.retryForBilling(any())).thenReturn(new V1ShipmentCreationResponse());
        var responseEntity = customerBookingV3Controller.retryForBilling(1L);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fusionCheckCreditLimit() throws RunnerException {
        when(customerBookingV3Service.checkCreditLimitFromFusion(any())).thenReturn(new CheckCreditLimitResponse());
        var responseEntity = customerBookingV3Controller.checkCreditLimitFromFusion(new CreditLimitRequest());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAllMasterDataTest() {
        Map<String, Object> masterData = new HashMap<>();
        when(customerBookingV3Service.getAllMasterData(anyLong())).thenReturn(masterData);
        var response = customerBookingV3Controller.getAllMasterData(1L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(customerBookingV3Service).getAllMasterData(1L);
    }
}
