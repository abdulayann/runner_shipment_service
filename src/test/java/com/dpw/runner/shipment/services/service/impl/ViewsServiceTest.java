package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IDefaultViewsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IViewsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.ViewsRequest;
import com.dpw.runner.shipment.services.dto.response.ViewsResponse;
import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.entity.Views;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
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
class ViewsServiceTest {

    @Mock
    private IViewsDao viewsDao;

    @Mock
    private IDefaultViewsDao defaultViewsDao;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private ViewsService viewsService;

    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
    }

    @Test
    void testCreate() {
           
        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.findByCreatedByAndEntityAndIsDefault(anyString(), any())).thenReturn(Optional.of(views));
        when(viewsDao.save(views)).thenReturn(views);
        when(jsonHelper.convertValue(any(ViewsRequest.class), eq(Views.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(Views.class), eq(ViewsResponse.class))).thenReturn(viewsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testCreate_Exception1() {

        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        request.setName("name");
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.findAllByUsername(anyString())).thenReturn(List.of("name"));

        assertThrows(ValidationException.class, () ->viewsService.create(commonRequestModel));
    }

    @Test
    void testCreateException() {

        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.save(views)).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(ViewsRequest.class), eq(Views.class))).thenReturn(views);

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testUpdate() {
           
        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        request.setId(10L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        views.setCreatedBy("user");
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(viewsDao.findByCreatedByAndEntityAndIsDefault(anyString(), any())).thenReturn(Optional.of(views));
        when(viewsDao.save(any(Views.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(ViewsRequest.class), eq(Views.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(Views.class), eq(ViewsResponse.class))).thenReturn(viewsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.update(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(viewsResponse), responseEntity);

    }

    @Test
    void testUpdate_Exception1() {

        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        request.setId(10L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        views.setCreatedBy("user1");
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));

        assertThrows(ValidationException.class, () -> viewsService.update(commonRequestModel));

    }

    @Test
    void testUpdate_Exception2() {

        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        request.setId(10L);
        request.setName("test");
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        views.setCreatedBy("user");
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(viewsDao.findAllByUsername(anyString())).thenReturn(List.of("test"));

        assertThrows(ValidationException.class, () -> viewsService.update(commonRequestModel));

    }

    @Test
    void testUpdate_Exception() {

        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        request.setId(10L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        views.setCreatedBy("user");
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(viewsDao.save(any(Views.class))).thenThrow(new RuntimeException());

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.update(commonRequestModel);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());

    }

    @Test
    void testUpdateWithNullId() {

        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.findById(any())).thenReturn(Optional.empty());

        var e = assertThrows(DataRetrievalFailureException.class, () -> viewsService.update(commonRequestModel));

    }

    @Test
    void TestUpdateWithOldView() {
        ViewsRequest request = new ViewsRequest(); // Provide necessary data for request
        request.setIsDefault(true);
        request.setId(10L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        views.setCreatedBy("user");
        views.setId(1L);
        ViewsResponse viewsResponse = new ViewsResponse();
        DefaultViews defaultView = new DefaultViews();
        defaultView.setDefaultViewId(2L);
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(viewsDao.save(any(Views.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(ViewsRequest.class), eq(Views.class))).thenReturn(views);

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.update(commonRequestModel);
    }

    @Test
    void list() {

        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        request.setFilterCriteria(List.of(FilterCriteria.builder().
                criteria(Criteria.builder()
                        .fieldName(Constants.CREATED_BY)
                        .operator("=")
                        .value(UserContext.getUser().Username)
                        .build()).
                build()));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        ResponseEntity<IRunnerResponse> responseEntity = viewsService.list(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void list2() {
        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        request.setFilterCriteria(List.of(FilterCriteria.builder().
                innerFilter(List.of(FilterCriteria.builder().build()))
                .criteria(Criteria.builder()
                        .fieldName(Constants.CREATED_BY)
                        .operator("=")
                        .value(UserContext.getUser().Username)
                        .build()).
                build()));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        ResponseEntity<IRunnerResponse> responseEntity = viewsService.list(commonRequestModel);

        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testList() {
           
        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        List<Views> viewsList = List.of(Views.builder().build()); // Provide necessary data for views list
        when(viewsDao.findAll()).thenReturn(viewsList);
        ViewsResponse viewsResponse = new ViewsResponse();
        when(jsonHelper.convertValue(any(), eq(ViewsResponse.class))).thenReturn(viewsResponse);
        ResponseEntity<IRunnerResponse> responseEntity = viewsService.list(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListFailure() {

        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(viewsDao).findAll();
        ResponseEntity<IRunnerResponse> responseEntity = viewsService.list(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListAsync() throws Exception {
           
        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        List<Views> viewsList = List.of(Views.builder().build()); // Provide necessary data for views list
        when(viewsDao.findAll()).thenReturn(viewsList);
        when(jsonHelper.convertValue(any(), eq(ViewsResponse.class))).thenReturn(new ViewsResponse());

        CompletableFuture<ResponseEntity<IRunnerResponse>> future = viewsService.listAsync(commonRequestModel);
        ResponseEntity<IRunnerResponse> responseEntity = future.get();

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListAsyncFailure() throws Exception {

        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(viewsDao).findAll();
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = viewsService.listAsync(commonRequestModel);
        ResponseEntity<IRunnerResponse> responseEntity = future.get();

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDelete() {
           
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        views.setId(1L);
        when(viewsDao.findById(any())).thenReturn(Optional.of(views));

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDeleteWithException() {

        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDeleteWithNullId() {

        CommonGetRequest request = CommonGetRequest.builder().id(null).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        when(viewsDao.findById(any())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());


    }

    @Test
    void testRetrieveById() {
           
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(jsonHelper.convertValue(any(), eq(ViewsResponse.class))).thenReturn(new ViewsResponse());

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveByIdWithIncludeColumns() {

        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        request.setIncludeColumns(Arrays.asList("a"));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(jsonHelper.convertValue(any(), eq(ViewsResponse.class))).thenReturn(new ViewsResponse());

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveByIdWithException() {
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(viewsDao).findById(any());
        ResponseEntity<IRunnerResponse> responseEntity = viewsService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveById_NullRequestId() {
           
        CommonGetRequest request = CommonGetRequest.builder().build(); // Provide necessary data for request
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveById_ViewNotFound() {
           
        CommonGetRequest request = CommonGetRequest.builder().build(); // Provide necessary data for request
        request.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        when(viewsDao.findById(anyLong())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity.getBody());
    }

}