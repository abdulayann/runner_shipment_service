package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.*;


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

}