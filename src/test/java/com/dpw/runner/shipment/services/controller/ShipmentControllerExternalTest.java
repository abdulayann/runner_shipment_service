package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
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
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Optional;

import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {ShipmentControllerExternal.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentControllerExternalTest {
    @Mock
    IShipmentServiceV3 shipmentService;
    @Mock
    JsonHelper jsonHelper;
    @InjectMocks
    ShipmentControllerExternal shipmentControllerExternal;

    private MockMvc mockMvc;
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(shipmentControllerExternal).build();
    }

    @Test
    void list(){
        when(shipmentService.listShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        var responseEntity = shipmentControllerExternal.list(ListCommonRequest.builder().build());
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById() {
        when(shipmentService.retrieveShipmentDataByIdExternal(any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn("json");
        var responseEntity = shipmentControllerExternal.retrieveById(Optional.of(1L),Optional.empty(), NETWORK_TRANSFER);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveById_NoId_ReturnError() {
        when(shipmentService.retrieveShipmentDataByIdExternal(any(), any())).thenReturn(ResponseHelper.buildFailedResponse(ShipmentConstants.ID_GUID_NULL_ERROR));
        when(jsonHelper.convertToJson(any())).thenReturn("json");
        var responseEntity = shipmentControllerExternal.retrieveById(Optional.of(1L),Optional.empty(), NETWORK_TRANSFER);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByIdWithIncludeColumns() {
        when(shipmentService.retrieveShipmentDataByIdUsingIncludeColumns(any(), any())).thenReturn(ResponseHelper.buildSuccessResponse());
        when(jsonHelper.convertToJson(any())).thenReturn("json");
        var responseEntity = shipmentControllerExternal.retrieveById(CommonGetRequest.builder().build(), NETWORK_TRANSFER);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveByIdWithIncludeColumns_NoId_ReturnError() {
        when(shipmentService.retrieveShipmentDataByIdUsingIncludeColumns(any(), any())).thenReturn(ResponseHelper.buildFailedResponse(ShipmentConstants.ID_GUID_NULL_ERROR));
        when(jsonHelper.convertToJson(any())).thenReturn("json");
        var responseEntity = shipmentControllerExternal.retrieveById(CommonGetRequest.builder().build(), NETWORK_TRANSFER);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveShipmentDetails_Success() throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        when(shipmentService.getShipmentDetails(request)).thenReturn(ResponseEntity.ok(mockResponse));

        ResponseEntity<IRunnerResponse> response = shipmentControllerExternal.retrieveShipmentDetails(request);

        assertEquals(200, response.getStatusCodeValue());
        assertEquals(mockResponse, response.getBody());
    }

    @Test
    void testRetrieveShipmentDetailsWithValidGuid_Success() throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().guid("01544fd2-16d1-4c17-b369-f431bd85d2f0").build();
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        when(shipmentService.getShipmentDetails(request)).thenReturn(ResponseEntity.ok(mockResponse));

        ResponseEntity<IRunnerResponse> response = shipmentControllerExternal.retrieveShipmentDetails(request);

        assertEquals(200, response.getStatusCodeValue());
        assertEquals(mockResponse, response.getBody());
    }
    @Test
    void testRetrieveShipmentDetailsWithNullId_Success() throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().build();
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        Exception ex = assertThrows(ValidationException.class, () -> shipmentControllerExternal.retrieveShipmentDetails(request));

        assertEquals("Id or Guid is mandatory", ex.getMessage());
    }

    @Test
    void testRetrieveShipmentDetails_ValidationException() {
        CommonGetRequest request = CommonGetRequest.builder().build();
        assertThrows(ValidationException.class, () -> shipmentControllerExternal.retrieveShipmentDetails(request));
    }

    @Test
    void testGetShipmentList() throws RunnerException {
        ListCommonRequest request = new ListCommonRequest();

        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        when(shipmentService.fetchShipments(request)).thenReturn(ResponseEntity.ok(mockResponse));

        ResponseEntity<IRunnerResponse> response = shipmentControllerExternal.getShipmentList(request);

        assertEquals(200, response.getStatusCodeValue());
        assertEquals(mockResponse, response.getBody());
    }
}
