package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.NotesResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.ResponseEntity;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NotesServiceTest {

    @Mock
    private INotesDao notesDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;

    @InjectMocks
    private NotesService notesService;

    @Mock
    private ShipmentService shipmentService;

    @Mock
    private ConsolidationService consolidationService;

    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
    }

    @Test
    void testCreate() {
           
        NotesRequest request = new NotesRequest(); // Provide necessary data for request
        request.setEntityId(123L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        NotesResponse notesResponse = new NotesResponse();
        when(notesDao.save(notes)).thenReturn(notes);
        when(jsonHelper.convertValue(any(NotesRequest.class), eq(Notes.class))).thenReturn(notes);
        when(jsonHelper.convertValue(any(Notes.class), eq(NotesResponse.class))).thenReturn(notesResponse);

        ResponseEntity<IRunnerResponse> responseEntity = notesService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testCreateEntityIdNull() {

        NotesRequest request = new NotesRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        NotesResponse notesResponse = new NotesResponse();

        ResponseEntity<IRunnerResponse> responseEntity = notesService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testCreateEntityGuid() {

        NotesRequest request = new NotesRequest(); // Provide necessary data for request
        request.setEntityGuid(UUID.randomUUID().toString());
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        NotesResponse notesResponse = new NotesResponse();

        ResponseEntity<IRunnerResponse> responseEntity = notesService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @ParameterizedTest
    @ValueSource(strings = {Constants.SHIPMENT, Constants.CONSOLIDATION})
    void testCreateEntityGuidAndEntityType(String entityType) {

        NotesRequest request = new NotesRequest(); // Provide necessary data for request
        request.setEntityGuid(UUID.randomUUID().toString());
        request.setEntityType(entityType);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        NotesResponse notesResponse = new NotesResponse();
        if(entityType.equalsIgnoreCase(Constants.SHIPMENT)) {
            when(shipmentService.getIdFromGuid(any())).thenReturn(ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().build()));
        } else {
            when(consolidationService.getIdFromGuid(any())).thenReturn(ResponseHelper.buildSuccessResponse(ConsolidationDetailsResponse.builder().build()));
        }

        ResponseEntity<IRunnerResponse> responseEntity = notesService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testCreateException() {

        NotesRequest request = new NotesRequest();// Provide necessary data for request
        request.setEntityId(123L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        when(notesDao.save(notes)).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(NotesRequest.class), eq(Notes.class))).thenReturn(notes);

        ResponseEntity<IRunnerResponse> responseEntity = notesService.create(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testUpdate() throws RunnerException {
           
        NotesRequest request = new NotesRequest(); // Provide necessary data for request
        request.setId(10L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        NotesResponse notesResponse = new NotesResponse();
        when(notesDao.findById(anyLong())).thenReturn(Optional.of(notes));
        when(notesDao.save(any(Notes.class))).thenReturn(notes);
        when(jsonHelper.convertValue(any(NotesRequest.class), eq(Notes.class))).thenReturn(notes);
        when(jsonHelper.convertValue(any(Notes.class), eq(NotesResponse.class))).thenReturn(notesResponse);

        ResponseEntity<IRunnerResponse> responseEntity = notesService.update(commonRequestModel);

        assertEquals(ResponseHelper.buildSuccessResponse(notesResponse), responseEntity);

    }

    @Test
    void testUpdateWithNullId() {

        NotesRequest request = new NotesRequest(); // Provide necessary data for request
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        when(notesDao.findById(any())).thenReturn(Optional.empty());

        var e = assertThrows(DataRetrievalFailureException.class, () -> notesService.update(commonRequestModel));

    }

    @Test
    void testUpdateException() throws RunnerException {
        NotesRequest request = new NotesRequest(); // Provide necessary data for request
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        when(notesDao.findById(any())).thenReturn(Optional.of(notes));
        doThrow(new RuntimeException()).when(notesDao).save(any());
        when(jsonHelper.convertValue(any(NotesRequest.class), eq(Notes.class))).thenReturn(notes);

        ResponseEntity<IRunnerResponse> responseEntity = notesService.update(commonRequestModel);

    }

    @Test
    void testList() {
           
        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        List<Notes> notesList = List.of(Notes.builder().build()); // Provide necessary data for notes list
        Page<Notes> notesPage = new PageImpl<>(notesList);
        when(notesDao.findAll(any(), any())).thenReturn(notesPage);
        NotesResponse notesResponse = new NotesResponse();
        when(jsonHelper.convertValue(any(), eq(NotesResponse.class))).thenReturn(notesResponse);
        ResponseEntity<IRunnerResponse> responseEntity = notesService.list(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListFailure() {

        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(notesDao).findAll(any(), any());
        ResponseEntity<IRunnerResponse> responseEntity = notesService.list(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListAsync() throws Exception {
           
        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        List<Notes> notesList = List.of(Notes.builder().build()); // Provide necessary data for notes list
        Page<Notes> notesPage = new PageImpl<>(notesList);
        when(notesDao.findAll(any(), any())).thenReturn(notesPage);
        when(jsonHelper.convertValue(any(), eq(NotesResponse.class))).thenReturn(new NotesResponse());

        CompletableFuture<ResponseEntity<IRunnerResponse>> future = notesService.listAsync(commonRequestModel);
        ResponseEntity<IRunnerResponse> responseEntity = future.get();

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testListAsyncFailure() throws Exception {

        ListCommonRequest request = new ListCommonRequest(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(notesDao).findAll(any(), any());
        CompletableFuture<ResponseEntity<IRunnerResponse>> future = notesService.listAsync(commonRequestModel);
        ResponseEntity<IRunnerResponse> responseEntity = future.get();

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDelete() {
           
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        notes.setId(1L);
        when(notesDao.findById(any())).thenReturn(Optional.of(notes));

        ResponseEntity<IRunnerResponse> responseEntity = notesService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDeleteWithException() {

        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(notesDao).findById(any());

        ResponseEntity<IRunnerResponse> responseEntity = notesService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());

    }

    @Test
    void testDeleteWithNullId() {

        CommonGetRequest request = CommonGetRequest.builder().id(null).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        when(notesDao.findById(any())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = notesService.delete(commonRequestModel);

        assertNotNull(responseEntity.getBody());


    }

    @Test
    void testRetrieveById() {
           
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        when(notesDao.findById(anyLong())).thenReturn(Optional.of(notes));
        when(jsonHelper.convertValue(any(), eq(NotesResponse.class))).thenReturn(new NotesResponse());

        ResponseEntity<IRunnerResponse> responseEntity = notesService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveByIdWithIncludeColumns() {

        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        request.setIncludeColumns(Arrays.asList("a"));
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        Notes notes = new Notes(); // Provide necessary data for notes
        when(notesDao.findById(anyLong())).thenReturn(Optional.of(notes));
        when(jsonHelper.convertValue(any(), eq(NotesResponse.class))).thenReturn(new NotesResponse());

        ResponseEntity<IRunnerResponse> responseEntity = notesService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveByIdWithException() {
        CommonGetRequest request = CommonGetRequest.builder().id(10L).build(); // Provide necessary data for request
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        doThrow(new RuntimeException()).when(notesDao).findById(any());
        ResponseEntity<IRunnerResponse> responseEntity = notesService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveById_NullRequestId() {
           
        CommonGetRequest request = CommonGetRequest.builder().build(); // Provide necessary data for request
        request.setId(null);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);

        ResponseEntity<IRunnerResponse> responseEntity = notesService.retrieveById(commonRequestModel);
        assertNotNull(responseEntity.getBody());
    }

    @Test
    void testRetrieveById_NoteNotFound() {
           
        CommonGetRequest request = CommonGetRequest.builder().build(); // Provide necessary data for request
        request.setId(1L);
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().build();
        commonRequestModel.setData(request);
        when(notesDao.findById(anyLong())).thenReturn(Optional.empty());

        ResponseEntity<IRunnerResponse> responseEntity = notesService.retrieveById(commonRequestModel);

        assertNotNull(responseEntity.getBody());
    }

}