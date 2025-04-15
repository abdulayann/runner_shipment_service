package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.*;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NotesDaoTest {

    private static JsonTestUtility jsonTestUtility;
    private static Notes testData;

    @InjectMocks
    private NotesDao notesDao;

    @Mock
    private INotesRepository notesRepository;

    @Mock
    private ValidatorUtility validatorUtility;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IAuditLogService auditLogService;


    private static ObjectMapper objectMapper;

    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        testData = jsonTestUtility.getTestNoteData();
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        Notes notes = new Notes();
        Mockito.when(notesRepository.save(Mockito.any())).thenReturn(notes);
        Notes notes1 = notesDao.save(Mockito.any());
        assert(notes == notes1);
    }

    @Test
    void saveAll() {
        List<Notes> notes = new ArrayList<>();
        Mockito.when(notesRepository.saveAll(Mockito.any())).thenReturn(notes);
        List<Notes> notes1 = notesDao.saveAll(notes);
        assert(notes.size() == notes1.size());
    }

    @Test
    void findAllWithSpec() {
        Specification<Notes> spec = null;
        Pageable pageable = null;
        List<Notes> noteList = new ArrayList<>();
        Page<Notes> notesList = new PageImpl<>(noteList);
        Mockito.when(notesRepository.findAll(spec, pageable)).thenReturn(notesList);
        Page<Notes> notes = notesDao.findAll(spec, pageable);
        assert(notesList.getTotalElements() == notes.getTotalElements());
    }

    @Test
    void findById() {
        Notes notes = new Notes();
        notes.setId(1L);
        Long id = 1L;
        Mockito.when(notesRepository.findById(Mockito.any())).thenReturn(Optional.of(notes));
        Optional<Notes> notes1 = notesDao.findById(id);
        assert(Objects.equals(notes.getId(), notes1.get().getId()));
    }

    @Test
    void delete() {
        Notes notes = new Notes();
        notesDao.delete(notes);
    }

    @Test
    void findByEntityIdAndEntityType() {
        Long entityId = 1L;
        String entityType = "Shipment";
        List<Notes> notes = new ArrayList<>();
        Mockito.when(notesRepository.findByEntityIdAndEntityType(Mockito.any(), Mockito.any())).thenReturn(notes);
        List<Notes> notes1 = notesDao.findByEntityIdAndEntityType(entityId, entityType);
        assert(notes.size() == notes1.size());
    }

    @Test
    void updateEntityFromOtherEntity() {
        testData.setId(1L);
        Notes savedNote = testData;
        when(notesRepository.findByEntityIdAndEntityType(any(), any())).thenReturn(List.of(savedNote));
        try {
            var result = notesDao.updateEntityFromOtherEntity(List.of(testData) , 1L , "Shipment");
            assertNotNull(result);
        } catch(Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityWithException() throws RunnerException {
        doThrow(new RuntimeException()).when(notesRepository).findByEntityIdAndEntityType(any(), any());
        try {
            var e = assertThrows(RunnerException.class, () -> notesDao.updateEntityFromOtherEntity(List.of(testData), 1L, "Shipment"));
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityWithOldEntityList() {
        testData.setId(1L);

        when(notesRepository.findById(1L)).thenReturn(Optional.of(testData));

        try {
            var result = notesDao.updateEntityFromOtherEntity(
                    List.of(testData), 1L, "Shipment", List.of(testData));
            assertNotNull(result);
        } catch(Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityWithOldEntityListDeletesOldNotes() throws JsonProcessingException {
        testData.setId(2L);
        testData.setGuid(UUID.randomUUID());
        Notes oldNote = objectMapper.convertValue(testData, Notes.class);
        oldNote.setId(1L);
        oldNote.setGuid(UUID.randomUUID());

        when(notesRepository.findById(2L)).thenReturn(Optional.of(oldNote));
        when(notesRepository.save(testData)).thenReturn(testData);
        when(notesRepository.save(oldNote)).thenReturn(oldNote);
        when(jsonHelper.convertToJson(oldNote)).thenReturn(objectMapper.writeValueAsString(oldNote));

        try {
            var result = notesDao.updateEntityFromOtherEntity(
                    List.of(testData), 1L, "Shipment", List.of(testData, oldNote));
            assertNotNull(result);
        } catch(Exception e) {
            fail();
        }
    }

    @Test
    void updateEntityFromOtherEntityWithOldEntityListThrowsException() {
        testData.setId(1L);

        var e = assertThrows(RunnerException.class, () ->
                notesDao.updateEntityFromOtherEntity(
                        List.of(testData), 1L, "Shipment", List.of(testData)));


        assertEquals(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE, e.getMessage());
    }


    @Test
    void saveEntityFromOtherEntity() throws JsonProcessingException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long noteId = 1L;
        testData.setId(noteId);

        when(notesRepository.findById(noteId)).thenReturn(Optional.of(testData));
        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(testData));
        when(notesRepository.save(testData)).thenReturn(testData);

        var result = notesDao.saveEntityFromOtherEntity(List.of(testData) , 1L , "Shipment");

        verify(auditLogService, atLeast(1)).addAuditLog(any());
    }

    @Test
    void saveEntityFromOtherEntityWithOldEntityMap() throws JsonProcessingException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long noteId = 1L;
        testData.setId(noteId);

        Map<Long, Notes> oldEntityMap = new HashMap<>();
        oldEntityMap.put(testData.getId(), testData);

        when(jsonHelper.convertToJson(any())).thenReturn(objectMapper.writeValueAsString(testData));
        when(notesRepository.saveAll(anyList())).thenReturn(List.of(testData));

        var result = notesDao.saveEntityFromOtherEntity(List.of(testData), 1L, "Shipment", oldEntityMap);

    }

    @Test
    void saveEntityFromOtherEntityWithOldEntityMapThrowsException() throws JsonProcessingException, RunnerException, NoSuchFieldException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Long noteId = 1L;
        testData.setId(noteId);

        var e = assertThrows(DataRetrievalFailureException.class, () ->
                notesDao.saveEntityFromOtherEntity(List.of(testData), 1L, "Shipment", new HashMap<>()));

    }



}