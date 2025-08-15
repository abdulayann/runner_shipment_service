package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Optional;

import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
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
}
