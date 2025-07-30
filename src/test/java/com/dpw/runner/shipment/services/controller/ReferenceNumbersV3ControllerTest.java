package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.BulkReferenceNumbersResponse;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IReferenceNumbersV3Service;
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
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ReferenceNumbersV3ControllerTest {

    @Mock
    private IReferenceNumbersV3Service referenceNumbersV3Service;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private ReferenceNumbersV3Controller controller;

    @Test
    void testCreate_success() {
        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        ReferenceNumbersResponse expectedResponse = new ReferenceNumbersResponse();

        when(referenceNumbersV3Service.create(any())).thenReturn(expectedResponse);

        ResponseEntity<IRunnerResponse> result = controller.create(request);

        assertEquals(HttpStatus.OK, result.getStatusCode());

        RunnerResponse<?> actualResponse = (RunnerResponse<?>) result.getBody();
        assertEquals(expectedResponse, actualResponse.getData());
    }

    @Test
    void testUpdate_success() {
        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        ReferenceNumbersResponse expectedResponse = new ReferenceNumbersResponse();

        when(referenceNumbersV3Service.update(any())).thenReturn(expectedResponse);

        ResponseEntity<IRunnerResponse> result = controller.update(request);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        RunnerResponse<?> actual = (RunnerResponse<?>) result.getBody();
        assertEquals(expectedResponse, actual.getData());
    }

    @Test
    void testUpdateBulk_success() {
        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        BulkReferenceNumbersResponse expectedResponse = new BulkReferenceNumbersResponse();
        ReferenceNumbersResponse ref1 = new ReferenceNumbersResponse();
        ReferenceNumbersResponse ref2 = new ReferenceNumbersResponse();
        List<ReferenceNumbersResponse> referenceNumbersResponseList = List.of(ref1, ref2);
        expectedResponse.setReferenceNumbersResponseList(referenceNumbersResponseList);

        when(referenceNumbersV3Service.updateBulk(any())).thenReturn(expectedResponse);

        ResponseEntity<IRunnerResponse> result = controller.updateBulk(List.of(request));

        assertEquals(HttpStatus.OK, result.getStatusCode());
        RunnerListResponse<?> actual = (RunnerListResponse<?>) result.getBody();
        assertEquals(2, actual.getData().size());
        assertTrue(actual.getData().contains(ref1));
        assertTrue(actual.getData().contains(ref2));
    }

    @Test
    void testDelete_success() {
        ReferenceNumbersRequest request = new ReferenceNumbersRequest();
        ReferenceNumbersResponse expectedResponse = new ReferenceNumbersResponse();

        when(referenceNumbersV3Service.delete(any())).thenReturn(expectedResponse);

        ResponseEntity<IRunnerResponse> result = controller.delete(request);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        RunnerResponse<?> actual = (RunnerResponse<?>) result.getBody();
        assertEquals(expectedResponse, actual.getData());
    }

    @Test
    void testList_success() {
        ListCommonRequest request = new ListCommonRequest();
        ReferenceNumbersResponse ref1 = new ReferenceNumbersResponse();
        ReferenceNumbersResponse ref2 = new ReferenceNumbersResponse();

        List<ReferenceNumbersResponse> serviceList = List.of(ref1, ref2);
        when(referenceNumbersV3Service.list(any(), eq(null))).thenReturn(serviceList);

        ResponseEntity<IRunnerResponse> result = controller.list(request, null);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        RunnerListResponse<?> actual = (RunnerListResponse<?>) result.getBody();

        assertNotNull(actual);
        assertEquals(2, actual.getData().size());
        assertTrue(actual.getData().contains(ref1));
        assertTrue(actual.getData().contains(ref2));
    }

    @Test
    void testListV3_success() {
        ListCommonRequest request = new ListCommonRequest();
        request.setPageSize(2);
        request.setPageNo(1);
        ReferenceNumbersResponse ref1 = new ReferenceNumbersResponse();
        ref1.setType("SRN");
        ref1.setReferenceNumber("srn123");
        ReferenceNumbersResponse ref2 = new ReferenceNumbersResponse();
        ref1.setType("SRN");
        ref1.setReferenceNumber("srn1234");
        // Fill with some data
        List<ReferenceNumbersResponse> serviceList = List.of(ref1, ref2);

        // Create a mock response from the service
        List<IRunnerResponse> responses = new ArrayList<>();
        responses.addAll(serviceList);

        when(referenceNumbersV3Service.listReferenceNumbers(request, null))
                .thenReturn(ResponseHelper.buildListSuccessResponse(
                        responses,
                        1,
                        1));

        ResponseEntity<IRunnerResponse> result = controller.listV3(request, null);

        assertEquals(HttpStatus.OK, result.getStatusCode());
        RunnerListResponse<?> actual = (RunnerListResponse<?>) result.getBody();

        assertNotNull(actual);
        assertEquals(2, actual.getData().size());
        assertTrue(actual.getData().contains(ref1));
        assertTrue(actual.getData().contains(ref2));
    }

}
