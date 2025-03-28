package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IAirMessagingLogsDao;
import com.dpw.runner.shipment.services.dto.request.AirMessagingLogsRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.AirMessagingLogsResponse;
import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AirMessagingLogsServiceTest {

    @Mock
    private IAirMessagingLogsDao airMessagingLogsDao;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private AirMessagingLogsService airMessagingLogsService;

    private static JsonTestUtility jsonTestUtility;
    private AirMessagingLogs testAirMessagingLogs;
    private static final ModelMapper modelMapperTest = new ModelMapper();

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build());

        testAirMessagingLogs = jsonTestUtility.getTestAirMessagingLogs();
    }

    @Test
    void testCreate_Success() throws RunnerException {
        AirMessagingLogsRequest airMessagingLogsRequest = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsRequest.class);
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        AirMessagingLogsResponse airMessagingLogsResponse = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsResponse.class);

        when(jsonHelper.convertValue(airMessagingLogsRequest, AirMessagingLogs.class)).thenReturn(airMessagingLogs);
        when(airMessagingLogsDao.save(any())).thenReturn(airMessagingLogs);
        when(jsonHelper.convertValue(airMessagingLogs, AirMessagingLogsResponse.class)).thenReturn(airMessagingLogsResponse);
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.create(CommonRequestModel.buildRequest(airMessagingLogsRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(airMessagingLogsResponse), responseEntity);
    }

    @Test
    void testCreate_Failure() throws RunnerException {
        AirMessagingLogsRequest airMessagingLogsRequest = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsRequest.class);
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;

        when(jsonHelper.convertValue(airMessagingLogsRequest, AirMessagingLogs.class)).thenReturn(airMessagingLogs);
        when(airMessagingLogsDao.save(any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.create(CommonRequestModel.buildRequest(airMessagingLogsRequest));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testUpdate_Success() throws RunnerException {
        AirMessagingLogsRequest airMessagingLogsRequest = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsRequest.class);
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        AirMessagingLogsResponse airMessagingLogsResponse = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsResponse.class);

        when(jsonHelper.convertValue(airMessagingLogsRequest, AirMessagingLogs.class)).thenReturn(airMessagingLogs);
        when(airMessagingLogsDao.save(any())).thenReturn(airMessagingLogs);
        when(airMessagingLogsDao.findById(any())).thenReturn(Optional.of(airMessagingLogs));
        when(jsonHelper.convertValue(airMessagingLogs, AirMessagingLogsResponse.class)).thenReturn(airMessagingLogsResponse);
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.update(CommonRequestModel.buildRequest(airMessagingLogsRequest));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(airMessagingLogsResponse), responseEntity);
    }

    @Test
    void testUpdate_Failure_NullRequest() {
        AirMessagingLogsRequest airMessagingLogsRequest = null;
        assertThrows(RunnerException.class, () -> airMessagingLogsService.update(CommonRequestModel.buildRequest(airMessagingLogsRequest)));
    }

    @Test
    void testUpdate_Failure_DataRetrievalFailure() {
        AirMessagingLogsRequest airMessagingLogsRequest = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsRequest.class);

        when(airMessagingLogsDao.findById(any())).thenReturn(Optional.empty());
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(airMessagingLogsRequest);
        assertThrows(DataRetrievalFailureException.class, () -> airMessagingLogsService.update(commonRequestModel));
    }

    @Test
    void testUpdate_Failure_SaveError() throws RunnerException {
        AirMessagingLogsRequest airMessagingLogsRequest = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsRequest.class);
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;

        when(jsonHelper.convertValue(airMessagingLogsRequest, AirMessagingLogs.class)).thenReturn(airMessagingLogs);
        when(airMessagingLogsDao.save(any())).thenThrow(new RuntimeException());
        when(airMessagingLogsDao.findById(any())).thenReturn(Optional.of(airMessagingLogs));
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.update(CommonRequestModel.buildRequest(airMessagingLogsRequest));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testUpdate_Failure_GuidNotMatch() {
        AirMessagingLogsRequest airMessagingLogsRequest = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsRequest.class);
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        AirMessagingLogs oldEntity = jsonTestUtility.getTestAirMessagingLogs();
        oldEntity.setGuid(UUID.randomUUID());

        when(jsonHelper.convertValue(airMessagingLogsRequest, AirMessagingLogs.class)).thenReturn(airMessagingLogs);
        when(airMessagingLogsDao.findById(any())).thenReturn(Optional.of(oldEntity));
        assertThrows(RunnerException.class, () -> airMessagingLogsService.update(CommonRequestModel.buildRequest(airMessagingLogsRequest)));
    }

    @Test
    void testList_Success() {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", 8, "=");
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        AirMessagingLogsResponse airMessagingLogsResponse = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsResponse.class);
        when(airMessagingLogsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(airMessagingLogs)));
        when(jsonHelper.convertValue(airMessagingLogs, AirMessagingLogsResponse.class)).thenReturn(airMessagingLogsResponse);
        airMessagingLogsService.list(CommonRequestModel.buildRequest(listCommonRequest));
        verify(airMessagingLogsDao, times(1)).findAll(any(), any());
    }

    @Test
    void testList_Failure() {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", 8, "=");
        when(airMessagingLogsDao.findAll(any(), any())).thenThrow(new RuntimeException());
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.list(CommonRequestModel.buildRequest(listCommonRequest));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testListAsync_Success() throws ExecutionException, InterruptedException {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", 8, "=");
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        AirMessagingLogsResponse airMessagingLogsResponse = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsResponse.class);
        when(airMessagingLogsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(airMessagingLogs)));
        when(jsonHelper.convertValue(airMessagingLogs, AirMessagingLogsResponse.class)).thenReturn(airMessagingLogsResponse);
        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = airMessagingLogsService.listAsync(CommonRequestModel.buildRequest(listCommonRequest));
        verify(airMessagingLogsDao, times(1)).findAll(any(), any());
        assertEquals(HttpStatus.OK, responseEntity.get().getStatusCode());
    }

    @Test
    void testListAsync_Failure() throws ExecutionException, InterruptedException {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", 8, "=");
        when(airMessagingLogsDao.findAll(any(), any())).thenThrow(new RuntimeException());
        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntity = airMessagingLogsService.listAsync(CommonRequestModel.buildRequest(listCommonRequest));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.get().getStatusCode());
    }

    @Test
    void testDelete_Success() {
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        when(airMessagingLogsDao.findById(any())).thenReturn(Optional.of(airMessagingLogs));
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(8L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testDelete_Failure() {
        when(airMessagingLogsDao.findById(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(8L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testDelete_Failure_NullRequest() {
        CommonGetRequest request = null;
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.delete(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_Success() {
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        AirMessagingLogsResponse airMessagingLogsResponse = modelMapperTest.map(testAirMessagingLogs, AirMessagingLogsResponse.class);
        when(airMessagingLogsDao.findById(any())).thenReturn(Optional.of(airMessagingLogs));
        when(jsonHelper.convertValue(airMessagingLogs, AirMessagingLogsResponse.class)).thenReturn(airMessagingLogsResponse);
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(8L).build()));
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_Failure() {
        when(airMessagingLogsDao.findById(any())).thenReturn(Optional.empty());
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().id(8L).build()));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testRetrieveById_Failure_NullRequest() {
        CommonGetRequest request = null;
        ResponseEntity<IRunnerResponse> responseEntity = airMessagingLogsService.retrieveById(CommonRequestModel.buildRequest(request));
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetRecentLogForEntityGuid_Success() {
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        when(airMessagingLogsDao.findByEntityGuidByQuery(any())).thenReturn(List.of(airMessagingLogs));
        AirMessagingLogs response = airMessagingLogsService.getRecentLogForEntityGuid(airMessagingLogs.getGuid());
        assertEquals(airMessagingLogs, response);
    }

    @Test
    void testGetRecentLogForEntityGuid_Failure_NullGuid() {
        AirMessagingLogs response = airMessagingLogsService.getRecentLogForEntityGuid(null);
        assertNull(response);
    }

    @Test
    void testGetRecentLogForEntityGuid_Failure_GuidNotExist() {
        AirMessagingLogs airMessagingLogs = testAirMessagingLogs;
        when(airMessagingLogsDao.findByEntityGuidByQuery(any())).thenReturn(List.of());
        AirMessagingLogs response = airMessagingLogsService.getRecentLogForEntityGuid(airMessagingLogs.getGuid());
        assertNull(response);
    }
}
