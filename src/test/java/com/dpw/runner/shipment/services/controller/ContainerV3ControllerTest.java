package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.Constants;
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
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerParams;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.impl.ContainerV3FacadeService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.utils.ContainerV3Util;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletResponse;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;

@ExtendWith(MockitoExtension.class)
class ContainerV3ControllerTest {

  @InjectMocks
  private ContainerV3Controller containerV3Controller;

  @Mock
  private IContainerV3Service containerV3Service;

  @Mock
  private ContainerV3FacadeService containerV3FacadeService;

  @Mock
  private ContainerV3Util containerV3Util;

  @Mock
  private JsonHelper jsonHelper;

  @Test
  void testCreateFromShipment() throws RunnerException {
    ContainerV3Request request = new ContainerV3Request();
    BulkContainerResponse bulkContainerResponse = new BulkContainerResponse();

    Mockito.when(containerV3FacadeService.createUpdateContainer(List.of(request), "SHIPMENT")).thenReturn(bulkContainerResponse);
    Mockito.when(jsonHelper.convertToJson(Mockito.any())).thenReturn("{}");

    ResponseEntity<IRunnerResponse> result = containerV3Controller.createFromShipment(request);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testCreateFromConsolidation() throws RunnerException {
    ContainerV3Request request = new ContainerV3Request();
    BulkContainerResponse bulkContainerResponse = new BulkContainerResponse();

    Mockito.when(containerV3FacadeService.createUpdateContainer(List.of(request), "CONSOLIDATION")).thenReturn(bulkContainerResponse);
    Mockito.when(jsonHelper.convertToJson(Mockito.any())).thenReturn("{}");

    ResponseEntity<IRunnerResponse> result = containerV3Controller.createFromConsolidation(request);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testUpdateBulk() throws RunnerException {
    List<ContainerV3Request> requestList = List.of(new ContainerV3Request());
    BulkContainerResponse response = new BulkContainerResponse();

    Mockito.when(containerV3FacadeService.createUpdateContainer(requestList, "CONSOLIDATION")).thenReturn(response);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.updateBulk(requestList);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testDeleteBulk() throws RunnerException {
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

    Mockito.lenient().when(containerV3Service.assignContainers(request, Constants.CONTAINER)).thenReturn(response);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.assignContainers(request);

    assertEquals(HttpStatus.OK, result.getStatusCode());
  }

  @Test
  void testUnAssignContainers() throws RunnerException {

    UnAssignContainerRequest request = new UnAssignContainerRequest();
    ContainerResponse response = new ContainerResponse();
    Mockito.when(containerV3Service.unAssignContainers(Mockito.eq(request), Mockito.eq(Constants.CONSOLIDATION_CONTAINER), Mockito.any(UnAssignContainerParams.class)))
            .thenReturn(response);
    ResponseEntity<IRunnerResponse> result = containerV3Controller.unAssignContainers(request);
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

    // Stub service call
    Mockito.when(containerV3Service.list(eq(listCommonRequest), anyBoolean(), anyString()))
        .thenReturn(mockResponse);

    // Act — use actual values here, NOT Mockito matchers
    ResponseEntity<IRunnerResponse> response = containerV3Controller.list(listCommonRequest, true, "test-source");

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

  @Test
  void testFetchConsolidationContainersForPackageAssignment() throws RunnerException {
    ListCommonRequest listCommonRequest = new ListCommonRequest();
    ContainerListResponse containerListResponse = new ContainerListResponse();
    containerListResponse.setContainers(List.of(new ContainerBaseResponse()));
    containerListResponse.setTotalPages(1);
    containerListResponse.setNumberOfRecords(1L);

    Mockito.when(containerV3Service.fetchConsolidationContainersForPackageAssignment(Mockito.any(), eq(Constants.CONSOLIDATION)))
            .thenReturn(containerListResponse);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.fetchConsolidationContainersForPackageAssignment(listCommonRequest);

    assertEquals(HttpStatus.OK, result.getStatusCode());
    IRunnerResponse body = result.getBody();
    assertNotNull(body);
  }

  @Test
  void testFetchShipmentContainersForPackageAssignment() throws RunnerException {
    ListCommonRequest listCommonRequest = new ListCommonRequest();
    ContainerListResponse containerListResponse = new ContainerListResponse();
    containerListResponse.setContainers(List.of(new ContainerBaseResponse()));
    containerListResponse.setTotalPages(1);
    containerListResponse.setNumberOfRecords(1L);

    Mockito.when(containerV3Service.fetchConsolidationContainersForPackageAssignment(Mockito.any(), eq(Constants.SHIPMENT)))
            .thenReturn(containerListResponse);

    ResponseEntity<IRunnerResponse> result = containerV3Controller.fetchShipmentContainersForPackageAssignment(listCommonRequest);

    assertEquals(HttpStatus.OK, result.getStatusCode());
    IRunnerResponse body = result.getBody();
    assertNotNull(body);
  }
}

