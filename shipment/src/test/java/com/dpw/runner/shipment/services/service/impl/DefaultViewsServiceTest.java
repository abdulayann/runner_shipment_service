package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IDefaultViewsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.DefaultViewsRequest;
import com.dpw.runner.shipment.services.dto.response.DefaultViewsResponse;
import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DefaultViewsServiceTest {

    @Mock
    private IDefaultViewsDao defaultViewsDao;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private DefaultViewsService defaultViewsService;

    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
    }

    @Test
    void testCreate() {
           
        DefaultViewsRequest request = new DefaultViewsRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        DefaultViewsResponse viewsResponse = new DefaultViewsResponse();
        when(defaultViewsDao.save(views)).thenReturn(views);
        when(jsonHelper.convertValue(any(DefaultViewsRequest.class), eq(DefaultViews.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(DefaultViews.class), eq(DefaultViewsResponse.class))).thenReturn(viewsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testCreateException() {

        DefaultViewsRequest request = new DefaultViewsRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        DefaultViewsResponse viewsResponse = new DefaultViewsResponse();
        when(defaultViewsDao.save(views)).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(DefaultViewsRequest.class), eq(DefaultViews.class))).thenReturn(views);

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testUpdate() {
           
        DefaultViewsRequest request = new DefaultViewsRequest(); // Provide necessary data for request
        request.setId(10L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        DefaultViewsResponse viewsResponse = new DefaultViewsResponse();
        when(defaultViewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(defaultViewsDao.save(any(DefaultViews.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(DefaultViewsRequest.class), eq(DefaultViews.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(DefaultViews.class), eq(DefaultViewsResponse.class))).thenReturn(viewsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.update(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(viewsResponse), responseEntity);

    }

    @Test
    void testUpdateWithNullId() {

        DefaultViewsRequest request = new DefaultViewsRequest(); // Provide necessary data for request
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        DefaultViewsResponse viewsResponse = new DefaultViewsResponse();
        when(defaultViewsDao.findById(any())).thenReturn(Optional.empty());

        var e = assertThrows(DataRetrievalFailureException.class, () -> defaultViewsService.update(commonRequestModel));

    }

    @Test
    void testUpdateException() {
        DefaultViewsRequest request = new DefaultViewsRequest(); // Provide necessary data for request
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        DefaultViewsResponse viewsResponse = new DefaultViewsResponse();
        when(defaultViewsDao.findById(any())).thenReturn(Optional.of(views));
        doThrow(new RuntimeException()).when(defaultViewsDao).save(any());
        when(jsonHelper.convertValue(any(DefaultViewsRequest.class), eq(DefaultViews.class))).thenReturn(views);

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.update(commonRequestModel);

    }

    @Test
    void testList() {
           
        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        List<DefaultViews> viewsList = List.of(DefaultViews.builder().build()); // Provide necessary data for views list
        when(defaultViewsDao.findAll()).thenReturn(viewsList);
        DefaultViewsResponse viewsResponse = new DefaultViewsResponse();
        when(jsonHelper.convertValue(any(), eq(DefaultViewsResponse.class))).thenReturn(viewsResponse);
        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.list(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListFailure() {

        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(defaultViewsDao).findAll();
        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.list(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListAsync() throws Exception {
           
        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        List<DefaultViews> viewsList = List.of(DefaultViews.builder().build()); // Provide necessary data for views list
        when(defaultViewsDao.findAll()).thenReturn(viewsList);
        when(jsonHelper.convertValue(any(), eq(DefaultViewsResponse.class))).thenReturn(new DefaultViewsResponse());

        CompletableFuture<ResponseEntity<IRunnerResponse>> future = defaultViewsService.listAsync(commonRequestModel);
        ResponseEntity<IRunnerResponse> responseEntity = future.get();

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListAsyncFailure() throws Exception {

        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(defaultViewsDao).findAll();
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = defaultViewsService.listAsync(commonRequestModel);
        ResponseEntity<IRunnerResponse> responseEntity = future.get();

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDelete() {
           
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        views.setId(1L);
        when(defaultViewsDao.findById(any())).thenReturn(Optional.of(views));

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDeleteWithException() {

        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        doThrow(new RuntimeException()).when(defaultViewsDao).findById(any());

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDeleteWithNullId() {

        CommonGetRequest request = CommonGetRequest.builder().id(null).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        when(defaultViewsDao.findById(any())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());


    }

    @Test
    void testRetrieveById() {
           
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        when(defaultViewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(jsonHelper.convertValue(any(), eq(DefaultViewsResponse.class))).thenReturn(new DefaultViewsResponse());

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveByIdWithIncludeColumns() {

        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        request.setIncludeColumns(Arrays.asList("a"));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        DefaultViews views = new DefaultViews(); // Provide necessary data for views
        when(defaultViewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(jsonHelper.convertValue(any(), eq(DefaultViewsResponse.class))).thenReturn(new DefaultViewsResponse());

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveByIdWithException() {
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(defaultViewsDao).findById(any());
        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveById_NullRequestId() {
           
        CommonGetRequest request = CommonGetRequest.builder().build(); // Provide necessary data for request
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveById_ViewNotFound() {
           
        CommonGetRequest request = CommonGetRequest.builder().build(); // Provide necessary data for request
        request.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        when(defaultViewsDao.findById(anyLong())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = defaultViewsService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity.getBody());
    }

}