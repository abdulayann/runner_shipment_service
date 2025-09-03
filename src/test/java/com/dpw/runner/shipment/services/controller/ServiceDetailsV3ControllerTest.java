package com.dpw.runner.shipment.services.controller;


import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsListResponse;
import com.dpw.runner.shipment.services.dto.response.ServiceDetailsResponse;
import com.dpw.runner.shipment.services.dto.request.ServiceDetailsRequest;
import com.dpw.runner.shipment.services.dto.v3.response.BulkServiceDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IServiceDetailsV3Service;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;

import java.util.Collections;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;


@ContextConfiguration(classes = {ServiceDetailsV3Controller.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ServiceDetailsV3ControllerTest {

    @Mock
    private IServiceDetailsV3Service serviceDetailsV3Service;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private ServiceDetailsV3Controller serviceDetailsV3Controller;


    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void createFromShipment() throws RunnerException {
        when(serviceDetailsV3Service.create(any(), eq("SHIPMENT"))).thenReturn(new ServiceDetailsResponse());
        var response = serviceDetailsV3Controller.createFromShipment(new ServiceDetailsRequest());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteFromShipment() throws RunnerException {
        when(serviceDetailsV3Service.delete(1L, "SHIPMENT")).thenReturn("Deleted Successfully.");
        var response = serviceDetailsV3Controller.deleteFromShipment(1L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateBulkFromShipment() throws RunnerException {
        when(serviceDetailsV3Service.updateBulk(any(), eq("SHIPMENT"))).thenReturn(new BulkServiceDetailsResponse());
        var response = serviceDetailsV3Controller.updateBulkFromShipment(Collections.singletonList(new ServiceDetailsRequest()));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteBulkFromShipment() throws RunnerException {
        when(serviceDetailsV3Service.deleteBulk(any(), eq("SHIPMENT"))).thenReturn(new BulkServiceDetailsResponse());
        var response = serviceDetailsV3Controller.deleteBulkFromShipment(Collections.singletonList(new ServiceDetailsRequest()));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void retrieveById() {
        when(serviceDetailsV3Service.retrieveById(any(), any(), eq(null))).thenReturn(new ServiceDetailsResponse());
        var response = serviceDetailsV3Controller.retrieveById(1L, null, null);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void getAllMasterData() {
        when(serviceDetailsV3Service.getAllMasterData(anyLong(), eq(null))).thenReturn(new HashMap<>());
        var response = serviceDetailsV3Controller.getAllMasterData(1L, null);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void fetchShipmentServices() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        when(serviceDetailsV3Service.fetchShipmentServices(any(), eq(null))).thenReturn(new ServiceDetailsListResponse());
        var response = serviceDetailsV3Controller.fetchShipmentServices(listCommonRequest, null);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }
}
