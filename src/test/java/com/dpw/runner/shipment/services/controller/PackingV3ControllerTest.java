package com.dpw.runner.shipment.services.controller;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.CalculatePackSummaryRequest;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignPackageContainerRequest;
import com.dpw.runner.shipment.services.dto.v3.request.OrderLineCreateUpdateDeleteRequest;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.BulkPackingResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import org.apache.http.auth.AuthenticationException;
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

import javax.servlet.http.HttpServletResponse;
import java.util.Collections;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;


@ContextConfiguration(classes = {PackingV3Controller.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PackingV3ControllerTest {

    @Mock
    private IPackingV3Service packingV3Service;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private PackingV3Controller packingV3Controller;


    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void createFromShipment() throws RunnerException {
        when(packingV3Service.create(any(), eq("SHIPMENT"))).thenReturn(new PackingResponse());
        var response = packingV3Controller.createFromShipment(new PackingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createFromConsolidation() throws RunnerException {
        when(packingV3Service.create(any(), eq("CONSOLIDATION"))).thenReturn(new PackingResponse());
        var response = packingV3Controller.createFromConsolidation(new PackingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void createFromCustomerBooking() throws RunnerException {
        when(packingV3Service.create(any(), eq("BOOKING"))).thenReturn(new PackingResponse());
        var response = packingV3Controller.createFromCustomerBooking(new PackingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateFromShipment() throws RunnerException {
        when(packingV3Service.update(any(), eq("SHIPMENT"))).thenReturn(new PackingResponse());
        var response = packingV3Controller.updateFromShipment(new PackingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateFromConsolidation() throws RunnerException {
        when(packingV3Service.update(any(), eq("CONSOLIDATION"))).thenReturn(new PackingResponse());
        var response = packingV3Controller.updateFromConsolidation(new PackingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateFromCustomerBooking() throws RunnerException {
        when(packingV3Service.update(any(), eq("BOOKING"))).thenReturn(new PackingResponse());
        var response = packingV3Controller.updateFromCustomerBooking(new PackingV3Request());
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteFromShipment() throws RunnerException {
        when(packingV3Service.delete(1L, "SHIPMENT")).thenReturn("Deleted Successfully.");
        var response = packingV3Controller.deleteFromShipment(1L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteFromConsolidation() throws RunnerException {
        when(packingV3Service.delete(1L, "CONSOLIDATION")).thenReturn("Deleted Successfully.");
        var response = packingV3Controller.deleteFromConsolidation(1L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteFromCustomerBooking() throws RunnerException {
        when(packingV3Service.delete(1L, "BOOKING")).thenReturn("Deleted Successfully.");
        var response = packingV3Controller.deleteFromCustomerBooking(1L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateBulkFromShipment() throws RunnerException {
        when(packingV3Service.updateBulk(any(), eq("SHIPMENT"), anyBoolean())).thenReturn(new BulkPackingResponse());
        var response = packingV3Controller.updateBulkFromShipment(Collections.singletonList(new PackingV3Request()));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateBulkFromConsolidation() throws RunnerException {
        when(packingV3Service.updateBulk(any(), eq("CONSOLIDATION"), anyBoolean())).thenReturn(new BulkPackingResponse());
        var response = packingV3Controller.updateBulkFromConsolidation(Collections.singletonList(new PackingV3Request()));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void updateBulkFromCustomerBooking() throws RunnerException {
        when(packingV3Service.updateBulk(any(), eq("BOOKING"), anyBoolean())).thenReturn(new BulkPackingResponse());
        var response = packingV3Controller.updateBulkFromCustomerBooking(Collections.singletonList(new PackingV3Request()));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteBulkFromShipment() throws RunnerException {
        when(packingV3Service.deleteBulk(any(), eq("SHIPMENT"))).thenReturn(new BulkPackingResponse());
        var response = packingV3Controller.deleteBulkFromShipment(Collections.singletonList(new PackingV3Request()));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteBulkFromConsolidation() throws RunnerException {
        when(packingV3Service.deleteBulk(any(), eq("CONSOLIDATION"))).thenReturn(new BulkPackingResponse());
        var response = packingV3Controller.deleteBulkFromConsolidation(Collections.singletonList(new PackingV3Request()));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void deleteBulkFromCustomerBooking() throws RunnerException {
        when(packingV3Service.deleteBulk(any(), eq("BOOKING"))).thenReturn(new BulkPackingResponse());
        var response = packingV3Controller.deleteBulkFromCustomerBooking(Collections.singletonList(new PackingV3Request()));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void retrieveById() {
        when(packingV3Service.retrieveById(any(), any(), eq(null))).thenReturn(new PackingResponse());
        var response = packingV3Controller.retrieveById(1L, null, null);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void downloadCSV() throws Exception {
        doNothing().when(packingV3Service).downloadPacking(any(HttpServletResponse.class), any(BulkDownloadRequest.class));
        packingV3Controller.downloadCSV(mock(HttpServletResponse.class), new BulkDownloadRequest());
        verify(packingV3Service, times(1)).downloadPacking(any(), any());
    }

    @Test
    void getAllMasterData() {
        when(packingV3Service.getAllMasterData(anyLong(), eq(null))).thenReturn(new HashMap<>());
        var response = packingV3Controller.getAllMasterData(1L, null);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void calculatePackSummary() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest calculatePackSummaryRequest = new CalculatePackSummaryRequest();
        var response = packingV3Controller.calculatePackSummary(calculatePackSummaryRequest, null);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void calculatePackSummaryForNte() throws AuthenticationException, RunnerException {
        CalculatePackSummaryRequest calculatePackSummaryRequest = new CalculatePackSummaryRequest();
        var response = packingV3Controller.calculatePackSummary(calculatePackSummaryRequest, "network_transfer");
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void assignContainersAtShipmentLevel() throws RunnerException {
        AssignContainerRequest request = new AssignContainerRequest();
        when(packingV3Service.assignShipmentPackagesContainers(request)).thenReturn(new ContainerResponse());
        var response = packingV3Controller.assignContainersAtShipmentLevel(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void unAssignContainersAtShipmentLevel() throws RunnerException {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        doNothing().when(packingV3Service).unAssignPackageContainers(request, Constants.SHIPMENT_PACKING);
        var response = packingV3Controller.unAssignContainersAtShipmentLevel(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void unAssignContainers() throws RunnerException {
        UnAssignPackageContainerRequest request = new UnAssignPackageContainerRequest();
        doNothing().when(packingV3Service).unAssignPackageContainers(request, Constants.CONSOLIDATION_PACKING);
        var response = packingV3Controller.unAssignContainers(request);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void calculateCargoSummary() throws RunnerException {
        var response = packingV3Controller.calculateCargoSummary(12L);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void orderLineBulkCreate() throws RunnerException {
        OrderLineCreateUpdateDeleteRequest fakeRequest = OrderLineCreateUpdateDeleteRequest.builder().build();
        var response = packingV3Controller.orderLineCreateUpdateDeleteBulk(fakeRequest);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }
}
