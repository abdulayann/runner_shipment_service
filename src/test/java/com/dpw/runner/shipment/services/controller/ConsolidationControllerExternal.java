package com.dpw.runner.shipment.services.controller;

// Add these imports if not present
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ConsolidationListV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3ExternalResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.ArrayList;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsolidationControllerExternalTest {

    @Mock
    private IConsolidationV3Service consolidationV3Service;

    @InjectMocks
    private ConsolidationControllerExternal controller;

    @Mock
    private JsonHelper jsonHelper;

    @Test
    void testGetConsolidationsList() throws RunnerException {
        ListCommonRequest request = new ListCommonRequest();

        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        when(consolidationV3Service.fetchConsolidation(request)).thenReturn(ResponseEntity.ok(mockResponse));

        ResponseEntity<IRunnerResponse> response = controller.getConsolidationsList(request);

        assertEquals(200, response.getStatusCodeValue());
        assertEquals(mockResponse, response.getBody());
    }

    @Test
    void testRetrieveConsolidationDetails_Success() throws RunnerException {
        CommonGetRequest request = CommonGetRequest.builder().id(1L).build();
        IRunnerResponse mockResponse = mock(IRunnerResponse.class);
        when(consolidationV3Service.getConsolidationDetails(request)).thenReturn(ResponseEntity.ok(mockResponse));

        ResponseEntity<IRunnerResponse> response = controller.retrieveConsolidationDetails(request);

        assertEquals(200, response.getStatusCodeValue());
        assertEquals(mockResponse, response.getBody());
    }

    @Test
    void testRetrieveConsolidationDetails_ValidationException() {
        CommonGetRequest request = CommonGetRequest.builder().build();
        assertThrows(ValidationException.class, () -> controller.retrieveConsolidationDetails(request));
    }

    @Test
    void retrieveByIdExternal_ShouldReturnSuccessResp() throws Exception{
        Long id = 1L;
        String guid = "guid-1";
        String xSource = "test-source";
        ConsolidationDetailsV3ExternalResponse mockResponse = new ConsolidationDetailsV3ExternalResponse();

        when(consolidationV3Service.retrieveByIdExternal(any())).thenReturn(mockResponse);
        when(jsonHelper.convertToJson(any())).thenReturn("{}");

        ResponseEntity<IRunnerResponse> response = controller.retrieveByIdExternal(id, guid, xSource);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(consolidationV3Service).retrieveByIdExternal(any());
    }

    @Test
    void retrieveByIdExternalPartial_ShouldReturnSuccessResp() throws Exception{
        Long id = 1L;
        String guid = "guid-123";
        String xSource = "test-source";
        CommonGetRequest request = CommonGetRequest.builder().guid(guid).build();
        ConsolidationDetailsV3ExternalResponse mockResponse = new ConsolidationDetailsV3ExternalResponse();

        when(consolidationV3Service.retrieveByIdExternalPartial(any())).thenReturn(mockResponse);
        when(jsonHelper.convertToJson(any())).thenReturn("{}");

        ResponseEntity<IRunnerResponse> response = controller.retrieveByIdExternalPartial(request, xSource);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(consolidationV3Service).retrieveByIdExternalPartial(any());
    }

    @Test
    void testListExternal_shouldReturnListExternalResponse(){
        ListCommonRequest listRequest = new ListCommonRequest();
        ConsolidationListV3Response mockResponse = new ConsolidationListV3Response();
        mockResponse.setConsolidationListResponses(new ArrayList<>());
        mockResponse.setTotalPages(1);
        mockResponse.setNumberOfRecords(1L);

        when(consolidationV3Service.listExternal(any())).thenReturn(mockResponse);
        when(jsonHelper.convertToJson(any())).thenReturn("{}");

        ResponseEntity<IRunnerResponse> response = controller.listExternal(listRequest);

        assertNotNull(response);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        verify(consolidationV3Service).listExternal(listRequest);
    }
}
