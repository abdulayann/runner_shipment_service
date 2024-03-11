package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IDefaultViewsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IViewsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.ViewsRequest;
import com.dpw.runner.shipment.services.dto.response.ViewsResponse;
import com.dpw.runner.shipment.services.entity.DefaultViews;
import com.dpw.runner.shipment.services.entity.Views;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
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
        when(viewsDao.save(views)).thenReturn(views);
        when(jsonHelper.convertValue(any(ViewsRequest.class), eq(Views.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(Views.class), eq(ViewsResponse.class))).thenReturn(viewsResponse);

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
        ViewsResponse viewsResponse = new ViewsResponse();
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(viewsDao.save(any(Views.class))).thenReturn(views);
        when(defaultViewsDao.findByUsername(anyString())).thenReturn(Optional.empty());
        when(jsonHelper.convertValue(any(ViewsRequest.class), eq(Views.class))).thenReturn(views);
        when(jsonHelper.convertValue(any(Views.class), eq(ViewsResponse.class))).thenReturn(viewsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = viewsService.update(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(viewsResponse), responseEntity);

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
    void testDelete() {
           
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Views views = new Views(); // Provide necessary data for views
        when(viewsDao.findById(anyLong())).thenReturn(Optional.of(views));
        when(defaultViewsDao.findByDefaultViewId(anyLong())).thenReturn(Optional.of(new DefaultViews()));

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
    void testRetrieveById_NullRequest() {
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(null);

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