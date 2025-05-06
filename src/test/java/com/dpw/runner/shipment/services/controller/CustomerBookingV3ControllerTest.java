package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {CustomerBookingV3Controller.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class CustomerBookingV3ControllerTest {

    @Mock
    private IContainerV3Service containerV3Service;

    @Mock
    private IPackingV3Service packingV3Service;

    @InjectMocks
    private CustomerBookingV3Controller customerBookingV3Controller;

    @Test
    void createBookingContainer() {
        when(containerV3Service.create(any(), any())).thenReturn(new ContainerResponse());
        var responseEntity = customerBookingV3Controller.createBookingContainers(new ContainerV3Request());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void updateBookingContainers() {
        when(containerV3Service.updateBulk(any(), any())).thenReturn(new BulkContainerResponse());
        var responseEntity = customerBookingV3Controller.updateBookingContainers(List.of(new ContainerV3Request()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getBookingContainers() throws RunnerException {
        when(containerV3Service.list(any(), anyBoolean())).thenReturn(new ContainerListResponse());
        var responseEntity = customerBookingV3Controller.listBookingContainers(CommonRequestModel.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void deleteBookingContainers() {
        when(containerV3Service.deleteBulk(any(), any())).thenReturn(new BulkContainerResponse());
        var responseEntity = customerBookingV3Controller.deleteBookingContainers(List.of(new ContainerV3Request()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void addBookingPackages() throws RunnerException {
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
        when(packingV3Service.list(any(), anyBoolean())).thenReturn(new PackingListResponse());
        var responseEntity = customerBookingV3Controller.listBookingPackages(CommonRequestModel.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void deleteBookingPackages() {
        when(packingV3Service.deleteBulk(any(),any())).thenReturn(new BulkPackingResponse());
        var responseEntity = customerBookingV3Controller.deleteBookingPackages(List.of(new PackingV3Request()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }
}
