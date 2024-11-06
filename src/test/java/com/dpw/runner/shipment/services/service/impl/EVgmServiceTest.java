package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.EVgmRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.EVgmResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.EVgm;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
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
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EVgmServiceTest extends CommonMocks {

    @InjectMocks
    private EVgmService eVgmService;
    @Mock
    private IEVgmDao eVgmDao;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private AuditLogService auditLogService;
    @Mock
    private MasterDataUtils masterDataUtils;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static EVgmRequest eVgmRequest;
    private static EVgm eVgm;


    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build()
        );
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        eVgmRequest = jsonTestUtility.getEVgmRequest();
        eVgm = jsonTestUtility.getEVgm();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
    }


    @Test
    void testCreate() throws RunnerException {
        EVgmRequest request = new EVgmRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        EVgm mockEVgm = EVgm.builder().build();
        mockEVgm.setId(1L);
        EVgmResponse eVgmResponse = objectMapper.convertValue(mockEVgm, EVgmResponse.class);

        // Mock
        when(jsonHelper.convertValue(any(), eq(EVgm.class))).thenReturn(new EVgm());
        when(eVgmDao.save(any())).thenReturn(mockEVgm);
        when(jsonHelper.convertValue(any(), eq(EVgmResponse.class))).thenReturn(eVgmResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = eVgmService.create(commonRequestModel);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(eVgmResponse), httpResponse);
    }

    @Test
    void testDelete() {
        // Mock
        when(eVgmDao.findById(anyLong())).thenReturn(Optional.of(eVgm));
        doNothing().when(eVgmDao).delete(any());
        var responseEntity = eVgmService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDelete2() {
        var responseEntity = eVgmService.delete(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete3() {
        var responseEntity = eVgmService.delete(CommonRequestModel.buildDependentDataRequest(CommonGetRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete5() {
        // Mock
        when(eVgmDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = eVgmService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete6() {
        // Mock
        when(eVgmDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = eVgmService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(12L).build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testCreateWithNullException() throws RunnerException {
        // Test
        Throwable t = assertThrows(Throwable.class, () -> eVgmService.create(CommonRequestModel.buildRequest()));
        // Assert
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());

    }

    @Test
    void testListWithEmptyRequest() {
        // Test
        var responseEntity = eVgmService.list(CommonRequestModel.buildRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithEmptyException() {
        when(eVgmDao.findAll(any(), any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = eVgmService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListWithNoResult() {
        when(eVgmDao.findAll(any(), any())).thenReturn(Page.empty());
        // Test
        var responseEntity = eVgmService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertTrue(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testListWithSuccessResult() {
        when(eVgmDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(eVgm)));
        when(modelMapper.map(any(), eq(EVgmResponse.class))).thenReturn(objectMapper.convertValue(eVgm, EVgmResponse.class));
        // Test
        var responseEntity = eVgmService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testDeleteWithEmptyRequest() {
        when(eVgmDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(eVgm)));
        when(modelMapper.map(any(), eq(EVgmResponse.class))).thenReturn(objectMapper.convertValue(eVgm, EVgmResponse.class));
        // Test
        var responseEntity = eVgmService.list(CommonRequestModel.buildRequest(ListCommonRequest.builder().build()));
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertFalse(((RunnerListResponse) Objects.requireNonNull(responseEntity.getBody())).getData().isEmpty());
    }

    @Test
    void testRetrieveByIdWithEmptyRequest() {
        var responseEntity = eVgmService.retrieveById(CommonRequestModel.buildRequest());
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithEmptyIdRequest() {
        var responseEntity = eVgmService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithIdNotPresent() {
        when(eVgmDao.findById(anyLong())).thenReturn(Optional.empty());
        var responseEntity = eVgmService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithExceptionInFindById() {
        when(eVgmDao.findById(anyLong())).thenThrow(new RuntimeException());
        var responseEntity = eVgmService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByIdWithSuccessResponse() {
        when(eVgmDao.findById(anyLong())).thenReturn(Optional.of(eVgm));
        when(jsonHelper.convertValue(any(), eq(EVgmResponse.class))).thenReturn(objectMapper.convertValue(eVgm, EVgmResponse.class));
        var responseEntity = eVgmService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(123L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveByGUIdWithSuccessResponse() {
        when(eVgmDao.findByGuid(any())).thenReturn(Optional.of(eVgm));
        when(jsonHelper.convertValue(any(), eq(EVgmResponse.class))).thenReturn(objectMapper.convertValue(eVgm, EVgmResponse.class));
        var responseEntity = eVgmService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().guid("69281519-f079-4dfc-99fd-970c40bd14a9").build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testCreateWithCompletePayload() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputEVgm = eVgm;
        EVgmRequest request = objectMapper.convertValue(inputEVgm, EVgmRequest.class);
        EVgmResponse eVgmResponse = objectMapper.convertValue(inputEVgm, EVgmResponse.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(EVgm.class))).thenReturn(inputEVgm);
        when(eVgmDao.save(any())).thenReturn(inputEVgm);
        when(jsonHelper.convertValue(any(), eq(EVgmResponse.class))).thenReturn(eVgmResponse);
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = eVgmService.create(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testUpdateWithCompletePayload() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputEVgm = eVgm;

        EVgmRequest request = objectMapper.convertValue(inputEVgm, EVgmRequest.class);
        // Mock
        when(jsonHelper.convertValue(any(), eq(EVgm.class))).thenReturn(inputEVgm);
        when(eVgmDao.save(any())).thenReturn(inputEVgm);
        when(eVgmDao.findById(any())).thenReturn(Optional.of(inputEVgm));
        doThrow(new RuntimeException()).when(auditLogService).addAuditLog(any());
        // Test
        ResponseEntity<IRunnerResponse> httpResponse = eVgmService.update(CommonRequestModel.buildRequest(request));
        // Assert
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }

    @Test
    void testEVgmUpdateNullRequest() {
        // test
        var t = assertThrows(Throwable.class, () -> eVgmService.update(CommonRequestModel.builder().build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testEVgmUpdateNullRequestId() {
        // test
        var t = assertThrows(Throwable.class, () -> eVgmService.update(CommonRequestModel.builder().data(new EVgmRequest()).build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_INVALID_REQUEST_MSG, t.getMessage());
    }

    @Test
    void testEVgmUpdateWithIdDoesNotExists() {
        // mock
        when(eVgmDao.findById(any())).thenReturn(Optional.empty());
        // test
        var t = assertThrows(Throwable.class, () -> eVgmService.update(CommonRequestModel.builder().data(eVgmRequest).build()));
        // assert
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), t.getClass().getSimpleName());
        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, t.getMessage());
    }
}
