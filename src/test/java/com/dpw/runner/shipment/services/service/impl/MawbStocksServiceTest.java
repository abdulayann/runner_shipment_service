package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.MawbStocksConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dto.request.MawbStocksRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.MawbStocksResponse;
import com.dpw.runner.shipment.services.dto.response.NextMawbCarrierResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.syncing.Entity.MawbStocksV2;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IMawbStockSync;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class MawbStocksServiceTest {

    @InjectMocks
    private MawbStocksService mawbStocksService;

    @Mock
    private IMawbStocksDao mawbStocksDao;
    @Mock
    private IMawbStocksLinkDao mawbStocksLinkDao;
    @Mock
    private IMawbStockSync mawbStockSync;
    @Mock
    SyncEntityConversionService syncEntityConversionService;
    @Mock
    private JsonHelper jsonHelper;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).build());
    }

    @Test
    void testCreateSuccess() {
        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksRequest);


        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mawbStocks.setId(1L);

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder()
                .seqNumber("9131362")
                .parentId(1L)
                .status("Unused")
                .build();
        mawbStocks.setMawbStocksLinkRows(List.of(mawbStocksLink));

        MawbStocksResponse mockResponse = convertEntityToDto(mawbStocks);

        // Mock
        when(jsonHelper.convertValue(any(), eq(MawbStocks.class))).thenReturn(mawbStocks);
        when(mawbStocksDao.save(any())).thenReturn(mawbStocks);
        when(mawbStocksLinkDao.validateDuplicateMawbNumber(anyList())).thenReturn(0L);
        when(mawbStocksLinkDao.save(any())).thenReturn(mawbStocksLink);
        when(jsonHelper.convertValue(any(), eq(MawbStocksResponse.class))).thenReturn(mockResponse);


        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.create(commonRequestModel);


        // Assert
        verify(mawbStocksDao, times(1)).save(any());
        verify(mawbStockSync, times(1)).sync(any());
        verify(mawbStocksLinkDao, atLeastOnce()).save(any());
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);

    }

    @Test
    void testCreateDaoOperationThrowsError() {
        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksRequest);


        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mawbStocks.setId(1L);

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder()
                .seqNumber("9131362")
                .parentId(1L)
                .status("Unused")
                .build();
        mawbStocks.setMawbStocksLinkRows(List.of(mawbStocksLink));

        MawbStocksResponse mockResponse = convertEntityToDto(mawbStocks);
        String errorMessage = "mock exception";

        // Mock
        when(jsonHelper.convertValue(any(), eq(MawbStocks.class))).thenReturn(mawbStocks);
        when(mawbStocksDao.save(any())).thenThrow(new RuntimeException(errorMessage));

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.create(commonRequestModel);


        // Assert
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());

    }

    @Test
    void testCreateFailsOnDuplicateMawbNumbers() {
        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksRequest);


        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mawbStocks.setId(1L);

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder()
                .seqNumber("9131362")
                .parentId(1L)
                .status("Unused")
                .build();
        mawbStocks.setMawbStocksLinkRows(List.of(mawbStocksLink));

        String errorMessage = MawbStocksConstants.DUPLICATE_MAWB_STOCK_VALIDATION;

        // Mock
        when(jsonHelper.convertValue(any(), eq(MawbStocks.class))).thenReturn(mawbStocks);
        when(mawbStocksDao.save(any())).thenReturn(mawbStocks);
        when(mawbStocksLinkDao.validateDuplicateMawbNumber(anyList())).thenReturn(2L);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.create(commonRequestModel);


        // Assert
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(errorMessage, runnerResponse.getError().getMessage());

    }

    @Test
    void testCreateSyncThrowsError() {
        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksRequest);


        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mawbStocks.setId(1L);

        MawbStocksLink mawbStocksLink = MawbStocksLink.builder()
                .seqNumber("9131362")
                .parentId(1L)
                .status("Unused")
                .build();
        mawbStocks.setMawbStocksLinkRows(List.of(mawbStocksLink));

        MawbStocksResponse mockResponse = convertEntityToDto(mawbStocks);

        // Mock
        when(jsonHelper.convertValue(any(), eq(MawbStocks.class))).thenReturn(mawbStocks);
        when(mawbStocksDao.save(any())).thenReturn(mawbStocks);
        when(mawbStocksLinkDao.validateDuplicateMawbNumber(anyList())).thenReturn(0L);
        when(mawbStocksLinkDao.save(any())).thenReturn(mawbStocksLink);
        when(jsonHelper.convertValue(any(), eq(MawbStocksResponse.class))).thenReturn(mockResponse);
        doThrow(new RuntimeException("Error while syncing")).when(mawbStockSync).sync(any());



        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.create(commonRequestModel);


        // Assert
        verify(mawbStocksDao, times(1)).save(any());
        verify(mawbStockSync, times(1)).sync(any());
        verify(mawbStocksLinkDao, atLeastOnce()).save(any());
        assertEquals(ResponseHelper.buildSuccessResponse(mockResponse), httpResponse);

    }

    @Test
    void testUpdateSuccess() {
        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setId(1L);
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksRequest);

        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        MawbStocksResponse mawbStocksResponse = convertEntityToDto(mawbStocks);

        // Mock
        when(mawbStocksDao.findById(anyLong())).thenReturn(Optional.of(mawbStocks));
        when(mawbStocksDao.save(any())).thenReturn(mawbStocks);
        when(jsonHelper.convertValue(any(), eq(MawbStocks.class))).thenReturn(mawbStocks);
        when(jsonHelper.convertValue(any(), eq(MawbStocksResponse.class))).thenReturn(mawbStocksResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.update(commonRequestModel);

        // Verify
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
        assertEquals(ResponseHelper.buildSuccessResponse(mawbStocksResponse), httpResponse);
    }

    @Test
    void testUpdateDaoThrowsError() {
        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setId(1L);
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksRequest);

        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        MawbStocksResponse mawbStocksResponse = convertEntityToDto(mawbStocks);

        // Mock
        when(mawbStocksDao.findById(anyLong())).thenReturn(Optional.of(mawbStocks));
        when(mawbStocksDao.save(any())).thenThrow(new RuntimeException());
//        when(jsonHelper.convertValue(any(), eq(MawbStocks.class))).thenReturn(mawbStocks);
//        when(jsonHelper.convertValue(any(), eq(MawbStocksResponse.class))).thenReturn(mawbStocksResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.update(commonRequestModel);

        // Verify
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void testUpdateEntityNotPresent() {
        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setId(1L);
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksRequest);

        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        MawbStocksResponse mawbStocksResponse = convertEntityToDto(mawbStocks);

        // Mock
        when(mawbStocksDao.findById(anyLong())).thenReturn(Optional.of(mawbStocks));
        when(mawbStocksDao.save(any())).thenThrow(new RuntimeException());
//        when(jsonHelper.convertValue(any(), eq(MawbStocks.class))).thenReturn(mawbStocks);
//        when(jsonHelper.convertValue(any(), eq(MawbStocksResponse.class))).thenReturn(mawbStocksResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.update(commonRequestModel);

        // Verify
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }


    @Test
    void testListSuccess() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setFilterCriteria(new ArrayList<>());
        listCommonRequest.setPageNo(1);
        listCommonRequest.setPageSize(10);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(listCommonRequest);

        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");


        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mawbStocks.setId(1L);
        MawbStocksResponse mawbStocksResponse = objectMapper.convertValue(mawbStocks, MawbStocksResponse.class);

        PageImpl mockPage = new PageImpl(List.of(mawbStocks));
        List runnerResponses = convertEntityListToDtoList(mockPage.getContent());
        ResponseEntity<IRunnerResponse> mockResponse = ResponseHelper.buildListSuccessResponse(
                runnerResponses, mockPage.getTotalPages(), mockPage.getTotalElements()
        );

        // Mock
        when(mawbStocksDao.findAll(any(), any())).thenReturn(mockPage);
        when(jsonHelper.convertValue(any(), eq(MawbStocksResponse.class))).thenReturn(mawbStocksResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.list(commonRequestModel);

        // Assert
        assertEquals(mockResponse, httpResponse);
    }

    @Test
    void testListFails() {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setFilterCriteria(new ArrayList<>());
        listCommonRequest.setPageNo(1);
        listCommonRequest.setPageSize(10);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(listCommonRequest);

        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");


        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mawbStocks.setId(1L);
        MawbStocksResponse mawbStocksResponse = objectMapper.convertValue(mawbStocks, MawbStocksResponse.class);

        String errorMessage = DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;

        // Mock
        when(mawbStocksDao.findAll(any(), any())).thenThrow(new RuntimeException());

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.list(commonRequestModel);

        // Assert
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void testListAsyncSuccess() throws ExecutionException, InterruptedException {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setFilterCriteria(new ArrayList<>());
        listCommonRequest.setPageNo(1);
        listCommonRequest.setPageSize(10);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(listCommonRequest);

        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");


        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mawbStocks.setId(1L);
        MawbStocksResponse mawbStocksResponse = objectMapper.convertValue(mawbStocks, MawbStocksResponse.class);

        PageImpl mockPage = new PageImpl(List.of(mawbStocks));
        List runnerResponses = convertEntityListToDtoList(mockPage.getContent());
        ResponseEntity<IRunnerResponse> mockResponse = ResponseHelper.buildListSuccessResponse(
                runnerResponses, mockPage.getTotalPages(), mockPage.getTotalElements()
        );

        // Mock
        when(mawbStocksDao.findAll(any(), any())).thenReturn(mockPage);
        when(jsonHelper.convertValue(any(), eq(MawbStocksResponse.class))).thenReturn(mawbStocksResponse);

        // Test
        CompletableFuture<ResponseEntity<IRunnerResponse>> httpResponse = mawbStocksService.listAsync(commonRequestModel);

        // Assert
        assertEquals(mockResponse, httpResponse.get());
    }

    @Test
    void testListAsyncFails() throws ExecutionException, InterruptedException {
        ListCommonRequest listCommonRequest = new ListCommonRequest();
        listCommonRequest.setFilterCriteria(new ArrayList<>());
        listCommonRequest.setPageNo(1);
        listCommonRequest.setPageSize(10);

        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(listCommonRequest);

        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");


        MawbStocks mawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mawbStocks.setId(1L);
        MawbStocksResponse mawbStocksResponse = objectMapper.convertValue(mawbStocks, MawbStocksResponse.class);

        String errorMessage = DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;

        // Mock
        when(mawbStocksDao.findAll(any(), any())).thenThrow(new RuntimeException());

        // Test
        CompletableFuture<ResponseEntity<IRunnerResponse>> responseEntityCompletableFuture = mawbStocksService.listAsync(commonRequestModel);

        var httpResponse = responseEntityCompletableFuture.get();
        // Assert
        RunnerResponse runnerResponse = objectMapper.convertValue(httpResponse.getBody(), RunnerResponse.class);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
        assertEquals(errorMessage, runnerResponse.getError().getMessage());
    }

    @Test
    void delete() {
        Long id = 1L;
        CommonGetRequest getRequest = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);

        MawbStocks mockMawbStocks = new MawbStocks();

        when(mawbStocksDao.findById(id)).thenReturn(Optional.of(mockMawbStocks));

        ResponseEntity<IRunnerResponse> responseEntity = mawbStocksService.delete(commonRequestModel);

        verify(mawbStocksDao, times(1)).delete(any(MawbStocks.class));
        Assertions.assertEquals(responseEntity.getStatusCodeValue() , HttpStatus.OK.value());
    }

    @Test
    void deleteEntityNotPresent() {
        Long id = 1L;
        CommonGetRequest getRequest = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);

        MawbStocks mockMawbStocks = new MawbStocks();

        when(mawbStocksDao.findById(id)).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = mawbStocksService.delete(commonRequestModel);

        verify(mawbStocksDao, times(0)).delete(any(MawbStocks.class));
        Assertions.assertEquals(responseEntity.getStatusCodeValue() , HttpStatus.BAD_REQUEST.value());
    }

    @Test
    void deleteDaoThrowsException() {
        Long id = 1L;
        CommonGetRequest getRequest = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);

        MawbStocks mockMawbStocks = new MawbStocks();

        when(mawbStocksDao.findById(id)).thenReturn(Optional.of(mockMawbStocks));
        doThrow(new RuntimeException()).when(mawbStocksDao).delete(any());

        ResponseEntity<IRunnerResponse> responseEntity = mawbStocksService.delete(commonRequestModel);

        Assertions.assertEquals(responseEntity.getStatusCodeValue() , HttpStatus.BAD_REQUEST.value());
    }

    @Test
    void retrieveById() {
        Long id = 1L;
        CommonGetRequest getRequest = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);


        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");

        MawbStocks mockMawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mockMawbStocks.setId(id);
        MawbStocksResponse mockMawbStocksResponse = objectMapper.convertValue(mockMawbStocks, MawbStocksResponse.class);


        when(mawbStocksDao.findById(anyLong())).thenReturn(Optional.of(mockMawbStocks));
        when(jsonHelper.convertValue(any(MawbStocks.class), eq(MawbStocksResponse.class))).thenReturn(mockMawbStocksResponse);

        ResponseEntity<IRunnerResponse> responseEntity = mawbStocksService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(responseEntity, ResponseHelper.buildSuccessResponse(mockMawbStocksResponse));
    }

    @Test
    void retrieveByIdDaoThrowsException() {
        Long id = 1L;
        CommonGetRequest getRequest = CommonGetRequest.builder().id(id).build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);


        MawbStocksRequest mawbStocksRequest = new MawbStocksRequest();
        mawbStocksRequest.setAirLinePrefix("Aegean Airlines");
        mawbStocksRequest.setCount("1");
        mawbStocksRequest.setMawbNumber("9131362");
        mawbStocksRequest.setFrom("390-91313621");
        mawbStocksRequest.setTo("390-91313631");

        MawbStocks mockMawbStocks = objectMapper.convertValue(mawbStocksRequest, MawbStocks.class);
        mockMawbStocks.setId(id);
        MawbStocksResponse mockMawbStocksResponse = objectMapper.convertValue(mockMawbStocks, MawbStocksResponse.class);

        when(mawbStocksDao.findById(anyLong())).thenThrow(new RuntimeException());
//        when(jsonHelper.convertValue(any(MawbStocks.class), eq(MawbStocksResponse.class))).thenReturn(mockMawbStocksResponse);

        ResponseEntity<IRunnerResponse> responseEntity = mawbStocksService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void testGetNextMawbNumberByCarrierSuccess() {
        MawbStocks mawbStocks = new MawbStocks();
        PageImpl<MawbStocks> mawbStocksPage = new PageImpl<>(List.of(mawbStocks));
        String airLinePrefix = "aegan";
        String borrowedFrom =  null;

        // Mock
        when(mawbStocksDao.findAll(any(), any())).thenReturn(mawbStocksPage);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.getNextMawbNumberByCarrier(airLinePrefix, borrowedFrom,
            false);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(NextMawbCarrierResponse.builder().nextMawbNumber(mawbStocksPage.getContent().get(0).getNextMawbNumber()).build()), httpResponse);
    }

    @Test
    void testGetNextMawbNumberByCarrierReturnsNull() {
        MawbStocks mawbStocks = new MawbStocks();
        PageImpl<MawbStocks> mawbStocksPage = new PageImpl<>(List.of());
        String airLinePrefix = "aegan";
        String borrowedFrom =  null;

        // Mock
        when(mawbStocksDao.findAll(any(), any())).thenReturn(mawbStocksPage);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.getNextMawbNumberByCarrier(airLinePrefix, borrowedFrom,
            false);

        // Assert
        assertEquals(ResponseHelper.buildSuccessResponse(NextMawbCarrierResponse.builder().nextMawbNumber(null).build()), httpResponse);
    }

    @Test
    void testCreateV1MawbStocks() throws RunnerException {
        UUID guid = UUID.randomUUID();
        MawbStocksV2 mawbStocksV2 = new MawbStocksV2();
        mawbStocksV2.setGuid(guid);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksV2);

        MawbStocks mawbStocks = new MawbStocks();
        mawbStocks.setId(1L);
        mawbStocks.setGuid(guid);

        MawbStocksResponse mawbStocksResponse = convertEntityToDto(mawbStocks);

        // Mock
        when(mawbStocksDao.findByGuid(any())).thenReturn(Optional.of(mawbStocks));
        when(syncEntityConversionService.mawbStocksV1ToV2(any())).thenReturn(mawbStocks);
        when(jsonHelper.convertValue(any(), eq(MawbStocksResponse.class))).thenReturn(mawbStocksResponse);

        // Test
        ResponseEntity<IRunnerResponse> httpResponse = mawbStocksService.createV1MawbStocks(commonRequestModel, false);

        // Assert
        verify(syncEntityConversionService, times(1)).mawbStocksV1ToV2(any());
        verify(mawbStocksLinkDao, times(1)).deleteByParentId(anyLong());
        verify(mawbStocksDao, times(1)).save(any());
        assertEquals(ResponseHelper.buildSuccessResponse(mawbStocksResponse), httpResponse);
    }

    @Test
    void testCreateV1MawbStocksThrowsException() throws RunnerException {
        UUID guid = UUID.randomUUID();
        MawbStocksV2 mawbStocksV2 = new MawbStocksV2();
        mawbStocksV2.setGuid(guid);
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(mawbStocksV2);

        MawbStocks mawbStocks = new MawbStocks();
        mawbStocks.setId(1L);
        mawbStocks.setGuid(guid);

        MawbStocksResponse mawbStocksResponse = convertEntityToDto(mawbStocks);
        String errorMessage = "Mock error";

    // Mock
        when(mawbStocksDao.findByGuid(any())).thenThrow(new RuntimeException(errorMessage));

        // Test
        Exception e = assertThrows(RunnerException.class, () ->
                mawbStocksService.createV1MawbStocks(commonRequestModel, false));

        // Assert
        assertEquals(errorMessage, e.getMessage());
    }


    private List<IRunnerResponse> convertEntityListToDtoList(List<MawbStocks> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(mawbStocks -> responseList.add(convertEntityToDto(mawbStocks)));
        return responseList;
    }

    private MawbStocksResponse convertEntityToDto(MawbStocks mawbStocks) {
        return objectMapper.convertValue(mawbStocks, MawbStocksResponse.class);
    }

    private MawbStocks convertRequestToEntity(MawbStocksRequest request) {
        return objectMapper.convertValue(request, MawbStocks.class);
    }

}
