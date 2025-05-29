package com.dpw.runner.shipment.services.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentGridChangeV3Response;
import com.dpw.runner.shipment.services.dto.request.AutoAttachConsolidationV3Request;
import com.dpw.runner.shipment.services.dto.request.CalculateAchievedValueRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentConsoleAttachDetachV3Request;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListV3Response;
import com.dpw.runner.shipment.services.dto.response.ConsolidationPendingNotificationResponse;
import com.dpw.runner.shipment.services.dto.v3.request.ConsolidationDetailsV3Request;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.apache.http.auth.AuthenticationException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsolidationV3ControllerTest {

  @InjectMocks
  private ConsolidationV3Controller controller;

  @Mock
  private IConsolidationV3Service consolidationV3Service;

  @Mock
  private JsonHelper jsonHelper;

  private ConsolidationDetailsV3Request consolidationDetailsRequest;

  @BeforeEach
  void setup() {
    consolidationDetailsRequest = new ConsolidationDetailsV3Request();
    // set minimal required fields here if needed
  }

  @Test
  void testCreate_shouldReturnSuccessResponse() {
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();
    when(consolidationV3Service.create(any())).thenReturn(mockResponse);
    when(jsonHelper.convertToJson(any())).thenReturn("{}");

    ResponseEntity<IRunnerResponse> response = controller.create(consolidationDetailsRequest);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(consolidationV3Service).create(consolidationDetailsRequest);
  }

  @Test
  void testCompleteUpdate_shouldReturnSuccessResponse() throws RunnerException {
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();
    when(consolidationV3Service.completeUpdate(any())).thenReturn(mockResponse);
    when(jsonHelper.convertToJson(any())).thenReturn("{}");

    ResponseEntity<IRunnerResponse> response = controller.completeUpdate(consolidationDetailsRequest);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(consolidationV3Service).completeUpdate(consolidationDetailsRequest);
  }

  @Test
  void testRetrieveById_shouldReturnSuccessResponse() throws Exception {
    Long id = 123L;
    String guid = "guid-123";
    String xSource = "test-source";
    ConsolidationDetailsV3Response mockResponse = new ConsolidationDetailsV3Response();

    when(consolidationV3Service.retrieveById(any(), eq(xSource))).thenReturn(mockResponse);
    when(jsonHelper.convertToJson(any())).thenReturn("{}");

    ResponseEntity<IRunnerResponse> response = controller.retrieveById(id, guid, xSource);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(consolidationV3Service).retrieveById(any(), eq(xSource));
  }

  @Test
  void testAttachShipments_shouldReturnSuccessResponseWithWarning() throws Exception {
    ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
    when(consolidationV3Service.attachShipments(any())).thenReturn("Warning message");

    ResponseEntity<IRunnerResponse> response = controller.attachShipments(request);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(consolidationV3Service).attachShipments(request);
  }

  @Test
  void testCalculateAchievedValues_shouldReturnSuccess() throws Exception {
    Long consolidationId = 1L;
    ShipmentGridChangeV3Response mockResponse = new ShipmentGridChangeV3Response();
    CalculateAchievedValueRequest request = CalculateAchievedValueRequest.builder()
            .consolidationId(consolidationId).build();

    when(consolidationV3Service.calculateAchievedValues(request)).thenReturn(mockResponse);

    ResponseEntity<IRunnerResponse> response = controller.calculateAchievedValues(request);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(consolidationV3Service).calculateAchievedValues(request);
  }

  @Test
  void testPendingNotificationsData_shouldReturnSuccess() {
    Long id = 123L;
    ConsolidationPendingNotificationResponse mockResponse = new ConsolidationPendingNotificationResponse();

    when(consolidationV3Service.getPendingNotificationData(any())).thenReturn(mockResponse);

    ResponseEntity<IRunnerResponse> response = controller.pendingNotificationsData(id);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(consolidationV3Service).getPendingNotificationData(any());
  }

  @Test
  void testList_shouldReturnListResponse() {
    ListCommonRequest listRequest = new ListCommonRequest();
    ConsolidationListV3Response mockResponse = new ConsolidationListV3Response();
    mockResponse.setConsolidationListResponses(new ArrayList<>());
    mockResponse.setTotalPages(1);
    mockResponse.setNumberOfRecords(1L);

    when(consolidationV3Service.list(any(), anyBoolean())).thenReturn(mockResponse);
    when(jsonHelper.convertToJson(any())).thenReturn("{}");

    ResponseEntity<IRunnerResponse> response = controller.list(listRequest, true);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(consolidationV3Service).list(listRequest, true);
  }

  @Test
  void testGetAllMasterData_shouldReturnMasterData()
      throws AuthenticationException, RunnerException {
    Long consolidationId = 100L;
    Map<String, Object> mockResponse = new HashMap<>();

    when(consolidationV3Service.getAllMasterData(any(), eq(null))).thenReturn(mockResponse);
    ResponseEntity<IRunnerResponse> response = controller.getAllMasterData(consolidationId, null);

    assertNotNull(response);
  }

  @Test
  void testGetAutoAttachConsolidationDetails_shouldReturnAutoAttachDetails() {
    AutoAttachConsolidationV3Request request = new AutoAttachConsolidationV3Request();
    ConsolidationListV3Response mockResponse = new ConsolidationListV3Response();
    mockResponse.setConsolidationListResponses(new ArrayList<>());
    mockResponse.setTotalPages(1);
    mockResponse.setNumberOfRecords(1L);

    when(consolidationV3Service.getAutoAttachConsolidationDetails(any())).thenReturn(mockResponse);

    ResponseEntity<IRunnerResponse> response = controller.getAutoAttachConsolidationDetails(request);

    assertNotNull(response);
    assertEquals(HttpStatus.OK, response.getStatusCode());
  }

  @Test
  void testDetachShipments() throws RunnerException {
    ShipmentConsoleAttachDetachV3Request request = new ShipmentConsoleAttachDetachV3Request();
    ResponseEntity<IRunnerResponse> result = new ResponseEntity<>(HttpStatus.OK);

    when(consolidationV3Service.detachShipments(request)).thenReturn(result);

    ResponseEntity<IRunnerResponse> response = controller.detachShipments(request);
    assertNotNull(response);
  }
}

