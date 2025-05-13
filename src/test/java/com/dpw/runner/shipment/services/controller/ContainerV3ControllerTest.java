package com.dpw.runner.shipment.services.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;

import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.utils.ContainerV3Util;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletResponse;

@ExtendWith(MockitoExtension.class)
class ContainerV3ControllerTest {

  @InjectMocks
  private ContainerV3Controller containerV3Controller;

  @Mock
  private IContainerV3Service containerV3Service;

  @Mock
  private ContainerV3Util containerV3Util;

  @Mock
  private JsonHelper jsonHelper;

  @Test
  void testCreateFromShipment() {
    ContainerV3Request request = new ContainerV3Request();
    ContainerResponse response = new ContainerResponse();

    Mockito.when(containerV3Service.create(request, "SHIPMENT")).thenReturn(response);
    Mockito.when(jsonHelper.convertToJson(Mockito.any())).thenReturn("{}");

    ResponseEntity<IRunnerResponse> result = containerV3Controller.createFromShipment(request);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testCreateFromConsolidation() {
    ContainerV3Request request = new ContainerV3Request();
    ContainerResponse response = new ContainerResponse();

    Mockito.when(containerV3Service.create(request, "CONSOLIDATION")).thenReturn(response);
    Mockito.when(jsonHelper.convertToJson(Mockito.any())).thenReturn("{}");

    ResponseEntity<IRunnerResponse> result = containerV3Controller.createFromConsolidation(request);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testUpdateBulk() {
    List<ContainerV3Request> requestList = List.of(new ContainerV3Request());
    BulkContainerResponse response = new BulkContainerResponse();

    Mockito.when(containerV3Service.updateBulk(requestList, "SHIPMENT")).thenReturn(response);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.updateBulk(requestList);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testDeleteBulk() {
    List<ContainerV3Request> requestList = List.of(new ContainerV3Request());
    BulkContainerResponse response = new BulkContainerResponse();

    Mockito.when(containerV3Service.deleteBulk(requestList, "SHIPMENT")).thenReturn(response);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.deleteBulk(requestList);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testValidateContainerNumber() {
    String containerNumber = "CONT1234567";
    ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();

    Mockito.when(containerV3Service.validateContainerNumber(containerNumber)).thenReturn(response);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.validateContainerNumber(containerNumber);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testCalculateContainerSummary() throws RunnerException {
    Long shipmentId = 1L;
    Long consolidationId = null;
    ContainerSummaryResponse mockResponse = new ContainerSummaryResponse();

    Mockito.when(containerV3Service.calculateContainerSummary(shipmentId, consolidationId, null)).thenReturn(mockResponse);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.calculateContainerSummary(shipmentId, consolidationId, null);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testAssignContainers() throws RunnerException {
    AssignContainerRequest request = new AssignContainerRequest();
    ContainerResponse response = new ContainerResponse();

    Mockito.when(containerV3Service.assignContainers(request)).thenReturn(response);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.assignContainers(request);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testFetchShipmentContainers() throws RunnerException {
    ListCommonRequest listCommonRequest = new ListCommonRequest();
    ContainerListResponse containerListResponse = new ContainerListResponse();
    containerListResponse.setContainers(List.of(new ContainerBaseResponse()));
    containerListResponse.setTotalPages(1);
    containerListResponse.setNumberOfRecords(1L);

    Mockito.when(containerV3Service.fetchShipmentContainers(Mockito.any(), eq(null)))
        .thenReturn(containerListResponse);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.fetchShipmentContainers(listCommonRequest, null);

    assertEquals(HttpStatus.OK, result.getStatusCode());

    IRunnerResponse body = result.getBody();
    assertNotNull(body);
  }

  @Test
  void testList() throws RunnerException {
    // Arrange
    ListCommonRequest listCommonRequest = new ListCommonRequest();

    ContainerBaseResponse container = new ContainerBaseResponse();
    ContainerListResponse mockResponse = new ContainerListResponse();
    mockResponse.setContainers(List.of(container));
    mockResponse.setTotalPages(2);
    mockResponse.setNumberOfRecords(50L);

    Mockito.when(containerV3Service.list(eq(listCommonRequest), eq(true), eq(null)))
        .thenReturn(mockResponse);

    // Act
    ResponseEntity<IRunnerResponse> response = containerV3Controller.list(listCommonRequest, false, null);

    // Assert
    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  void downloadCSV() throws RunnerException {
    boolean isSuccess = true;
    containerV3Controller.downloadCSV(new MockHttpServletResponse(), new BulkDownloadRequest());
    assertTrue(isSuccess);
  }
}

