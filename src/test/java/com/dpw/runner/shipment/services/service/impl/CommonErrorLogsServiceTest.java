package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonErrorLogsDao;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.CommonErrorType;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CommonErrorLogsServiceTest {

    @Mock
    private ModelMapper modelMapper;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ICommonErrorLogsDao commonErrorLogsDao;

    @InjectMocks
    private CommonErrorLogsService commonErrorLogsService;

    private static ObjectMapper objectMapper;

    private CommonErrorLogs commonErrorLogs;

    @BeforeAll
    static void init() {
        objectMapper = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        commonErrorLogs = CommonErrorLogs.builder().entityId(1L).entityType(Constants.SHIPMENT)
                .errorType(CommonErrorType.AUTOMATIC_TRANSFER)
                .errorMessage("AutomaticTransferError").build();
        commonErrorLogs.setId(11L);
        commonErrorLogs.setGuid(UUID.fromString("893cc8fa-7315-4d23-a635-3ce8705a5140"));
    }

    @Test
    void testListWithEmptyRequest() {
        var responseEntity = commonErrorLogsService.list(CommonRequestModel.buildRequest());

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithEmptyException() {
        when(commonErrorLogsDao.findAll(any(), any())).thenThrow(new RuntimeException());

        var responseEntity = commonErrorLogsService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithNoResult() {
        when(commonErrorLogsDao.findAll(any(), any())).thenReturn(Page.empty());

        var responseEntity = commonErrorLogsService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertTrue(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testListWithSuccessResult() {
        when(commonErrorLogsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(commonErrorLogs)));
        when(modelMapper.map(any(), eq(CommonErrorLogsResponse.class))).thenReturn(objectMapper.convertValue(commonErrorLogs, CommonErrorLogsResponse.class));

        var responseEntity = commonErrorLogsService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testRetrieveByIdWithEmptyRequest() {
        var responseEntity = commonErrorLogsService.retrieveById(CommonRequestModel.buildRequest());

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithEmptyIdRequest() {
        var responseEntity = commonErrorLogsService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithIdNotPresent() {
        when(commonErrorLogsDao.findById(anyLong())).thenReturn(Optional.empty());

        var responseEntity = commonErrorLogsService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(11L).build()));

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithExceptionInFindById() {
        when(commonErrorLogsDao.findById(anyLong())).thenThrow(new RuntimeException());

        var responseEntity = commonErrorLogsService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(11L).build()));

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithSuccessResponse() {
        when(commonErrorLogsDao.findById(anyLong())).thenReturn(Optional.of(commonErrorLogs));
        when(jsonHelper.convertValue(any(), eq(CommonErrorLogsResponse.class))).thenReturn(objectMapper.convertValue(commonErrorLogs, CommonErrorLogsResponse.class));

        var responseEntity = commonErrorLogsService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(11L).build()));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithGuidWithSuccessResponse() {
        when(commonErrorLogsDao.findByGuid(any())).thenReturn(Optional.of(commonErrorLogs));
        when(jsonHelper.convertValue(any(), eq(CommonErrorLogsResponse.class))).thenReturn(objectMapper.convertValue(commonErrorLogs, CommonErrorLogsResponse.class));

        var responseEntity = commonErrorLogsService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid("893cc8fa-7315-4d23-a635-3ce8705a5140").build()));

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}