//package com.dpw.runner.shipment.services.dao.impl;
//
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
//import com.dpw.runner.shipment.services.dto.request.UsersDto;
//import com.dpw.runner.shipment.services.entity.Notes;
//import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.mockito.Mockito;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.data.domain.Page;
//import org.springframework.data.domain.PageImpl;
//import org.springframework.data.domain.Pageable;
//import org.springframework.data.jpa.domain.Specification;
//
//import java.util.*;
//
//@ExtendWith(MockitoExtension.class)
//class NotesDaoTest {
//
//    @Mock
//    private INotesRepository notesRepository;
//
//    @InjectMocks
//    private NotesDao notesDao;
//
//    @BeforeEach
//    void setUp() {
//        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
//    }
//
//    @Test
//    void save() {
//        Notes notes = new Notes();
//        Mockito.when(notesRepository.save(Mockito.any())).thenReturn(notes);
//        Notes notes1 = notesDao.save(Mockito.any());
//        assert(notes == notes1);
//    }
//
//    @Test
//    void saveAll() {
//        List<Notes> notes = new ArrayList<>();
//        Mockito.when(notesRepository.saveAll(Mockito.any())).thenReturn(notes);
//        List<Notes> notes1 = notesDao.saveAll(Mockito.any());
//        assert(notes.size() == notes1.size());
//    }
//
//    @Test
//    void findAllWithSpec() {
//        Specification<Notes> spec = null;
//        Pageable pageable = null;
//        List<Notes> noteList = new ArrayList<>();
//        Page<Notes> notesList = new PageImpl<>(noteList);
//        Mockito.when(notesRepository.findAll(spec, pageable)).thenReturn(notesList);
//        Page<Notes> notes = notesDao.findAll(spec, pageable);
//        assert(notesList.getTotalElements() == notes.getTotalElements());
//    }
//
//    @Test
//    void findById() {
//        Notes notes = new Notes();
//        notes.setId(1L);
//        Long id = 1L;
//        Mockito.when(notesRepository.findById(Mockito.any())).thenReturn(Optional.of(notes));
//        Optional<Notes> notes1 = notesDao.findById(id);
//        assert(Objects.equals(notes.getId(), notes1.get().getId()));
//    }
//
//    @Test
//    void delete() {
//        Notes notes = new Notes();
//        notesDao.delete(notes);
//    }
//
//    @Test
//    void findByEntityIdAndEntityType() {
//        Long entityId = 1L;
//        String entityType = "Events";
//        List<Notes> notes = new ArrayList<>();
//        Mockito.when(notesRepository.findByEntityIdAndEntityType(Mockito.any(), Mockito.any())).thenReturn(notes);
//        List<Notes> notes1 = notesDao.findByEntityIdAndEntityType(entityId, entityType);
//        assert(notes.size() == notes1.size());
//    }
//}

package com.dpw.runner.shipment.services.dao.impl;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.fasterxml.jackson.core.JsonProcessingException;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
@ContextConfiguration(classes = {NotesDao.class})
@ExtendWith(SpringExtension.class)
class NotesDaoTest {
    @MockBean
    private IAuditLogService iAuditLogService;
    @MockBean
    private INotesRepository iNotesRepository;
    @MockBean
    private JsonHelper jsonHelper;
    @Autowired
    private NotesDao notesDao;
    /**
     * Method under test: {@link NotesDao#save(Notes)}
     */
    @Test
    void testSave() {
        // Arrange
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        when(iNotesRepository.save(Mockito.<Notes>any())).thenReturn(notes);
        Notes notes2 = new Notes();
        notes2.setAssignedTo("alice.liddell@example.org");
        notes2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes2.setEntityId(1L);
        notes2.setEntityType("Entity Type");
        notes2.setGuid(UUID.randomUUID());
        notes2.setId(1L);
        notes2.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setInsertUserDisplayName("Insert User Display Name");
        notes2.setInsertUserId("42");
        notes2.setIsActive(true);
        notes2.setIsDeleted(true);
        notes2.setIsPublic(true);
        notes2.setLabel("Label");
        notes2.setTenantId(1);
        notes2.setText("Text");
        notes2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setUpdatedBy("2020-03-01");
        // Act
        Notes actualSaveResult = notesDao.save(notes2);
        // Assert
        verify(iNotesRepository).save(isA(Notes.class));
        LocalTime expectedToLocalTimeResult = actualSaveResult.getUpdatedAt().toLocalTime();
        assertSame(expectedToLocalTimeResult, actualSaveResult.getInsertDate().toLocalTime());
    }
    /**
     * Method under test: {@link NotesDao#save(Notes)}
     */
    @Test
    void testSave2() {
        // Arrange
        when(iNotesRepository.save(Mockito.<Notes>any())).thenThrow(new DataRetrievalFailureException("Msg"));
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        // Act and Assert
        assertThrows(DataRetrievalFailureException.class, () -> notesDao.save(notes));
        verify(iNotesRepository).save(isA(Notes.class));
    }
    /**
     * Method under test: {@link NotesDao#saveAll(List)}
     */
    @Test
    void testSaveAll() {
        // Arrange
        ArrayList<Notes> notesList = new ArrayList<>();
        when(iNotesRepository.saveAll(Mockito.<Iterable<Notes>>any())).thenReturn(notesList);
        // Act
        List<Notes> actualSaveAllResult = notesDao.saveAll(new ArrayList<>());
        // Assert
        verify(iNotesRepository).saveAll(isA(Iterable.class));
        assertTrue(actualSaveAllResult.isEmpty());
        assertSame(notesList, actualSaveAllResult);
    }
    /**
     * Method under test: {@link NotesDao#saveAll(List)}
     */
    @Test
    void testSaveAll2() {
        // Arrange
        ArrayList<Notes> notesList = new ArrayList<>();
        when(iNotesRepository.saveAll(Mockito.<Iterable<Notes>>any())).thenReturn(notesList);
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        ArrayList<Notes> notesList2 = new ArrayList<>();
        notesList2.add(notes);
        // Act
        List<Notes> actualSaveAllResult = notesDao.saveAll(notesList2);
        // Assert
        verify(iNotesRepository).saveAll(isA(Iterable.class));
        assertTrue(actualSaveAllResult.isEmpty());
        assertSame(notesList, actualSaveAllResult);
    }
    /**
     * Method under test: {@link NotesDao#saveAll(List)}
     */
    @Test
    void testSaveAll3() {
        // Arrange
        ArrayList<Notes> notesList = new ArrayList<>();
        when(iNotesRepository.saveAll(Mockito.<Iterable<Notes>>any())).thenReturn(notesList);
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        Notes notes2 = new Notes();
        notes2.setAssignedTo("Assigned To");
        notes2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setCreatedBy("Created By");
        notes2.setEntityId(2L);
        notes2.setEntityType("42");
        notes2.setGuid(UUID.randomUUID());
        notes2.setId(2L);
        notes2.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setInsertUserDisplayName("42");
        notes2.setInsertUserId("Insert User Id");
        notes2.setIsActive(false);
        notes2.setIsDeleted(false);
        notes2.setIsPublic(false);
        notes2.setLabel("42");
        notes2.setTenantId(2);
        notes2.setText("42");
        notes2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setUpdatedBy("2020/03/01");
        ArrayList<Notes> notesList2 = new ArrayList<>();
        notesList2.add(notes2);
        notesList2.add(notes);
        // Act
        List<Notes> actualSaveAllResult = notesDao.saveAll(notesList2);
        // Assert
        verify(iNotesRepository).saveAll(isA(Iterable.class));
        assertTrue(actualSaveAllResult.isEmpty());
        assertSame(notesList, actualSaveAllResult);
    }
    /**
     * Method under test: {@link NotesDao#saveAll(List)}
     */
    @Test
    void testSaveAll4() {
        // Arrange
        when(iNotesRepository.saveAll(Mockito.<Iterable<Notes>>any())).thenThrow(new DataRetrievalFailureException("Msg"));
        // Act and Assert
        assertThrows(DataRetrievalFailureException.class, () -> notesDao.saveAll(new ArrayList<>()));
        verify(iNotesRepository).saveAll(isA(Iterable.class));
    }
    /**
     * Method under test: {@link NotesDao#findAll(Specification, Pageable)}
     */
    @Test
    void testFindAll() {
        // Arrange
        PageImpl<Notes> pageImpl = new PageImpl<>(new ArrayList<>());
        when(iNotesRepository.findAll(Mockito.<Specification<Notes>>any(), Mockito.<Pageable>any())).thenReturn(pageImpl);
        // Act
        Page<Notes> actualFindAllResult = notesDao.findAll(null, null);
        // Assert
        verify(iNotesRepository).findAll((Specification<Notes>) isNull(), (Pageable) isNull());
        assertTrue(actualFindAllResult.toList().isEmpty());
        assertSame(pageImpl, actualFindAllResult);
    }
    /**
     * Method under test: {@link NotesDao#findAll(Specification, Pageable)}
     */
    @Test
    void testFindAll2() {
        // Arrange
        when(iNotesRepository.findAll(Mockito.<Specification<Notes>>any(), Mockito.<Pageable>any()))
                .thenThrow(new DataRetrievalFailureException("Msg"));
        // Act and Assert
        assertThrows(DataRetrievalFailureException.class, () -> notesDao.findAll(null, null));
        verify(iNotesRepository).findAll((Specification<Notes>) isNull(), (Pageable) isNull());
    }
    /**
     * Method under test: {@link NotesDao#findById(Long)}
     */
    @Test
    void testFindById() {
        // Arrange
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        Optional<Notes> ofResult = Optional.of(notes);
        when(iNotesRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        // Act
        Optional<Notes> actualFindByIdResult = notesDao.findById(1L);
        // Assert
        verify(iNotesRepository).findById(eq(1L));
        Notes getResult = actualFindByIdResult.get();
        LocalTime expectedToLocalTimeResult = getResult.getUpdatedAt().toLocalTime();
        assertSame(expectedToLocalTimeResult, getResult.getInsertDate().toLocalTime());
    }
    /**
     * Method under test: {@link NotesDao#findById(Long)}
     */
    @Test
    void testFindById2() {
        // Arrange
        when(iNotesRepository.findById(Mockito.<Long>any())).thenThrow(new DataRetrievalFailureException("Msg"));
        // Act and Assert
        assertThrows(DataRetrievalFailureException.class, () -> notesDao.findById(1L));
        verify(iNotesRepository).findById(eq(1L));
    }
    /**
     * Method under test: {@link NotesDao#delete(Notes)}
     */
    @Test
    void testDelete() {
        // Arrange
        doNothing().when(iNotesRepository).delete(Mockito.<Notes>any());
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        // Act
        notesDao.delete(notes);
        // Assert that nothing has changed
        verify(iNotesRepository).delete(isA(Notes.class));
        assertEquals("1970-01-01", notes.getInsertDate().toLocalDate().toString());
        assertEquals("1970-01-01", notes.getCreatedAt().toLocalDate().toString());
        assertEquals("1970-01-01", notes.getUpdatedAt().toLocalDate().toString());
        assertEquals("2020-03-01", notes.getUpdatedBy());
        assertEquals("42", notes.getInsertUserId());
        assertEquals("Entity Type", notes.getEntityType());
        assertEquals("Insert User Display Name", notes.getInsertUserDisplayName());
        assertEquals("Jan 1, 2020 8:00am GMT+0100", notes.getCreatedBy());
        assertEquals("Label", notes.getLabel());
        assertEquals("Text", notes.getText());
        assertEquals("alice.liddell@example.org", notes.getAssignedTo());
        assertEquals(1, notes.getTenantId().intValue());
        assertEquals(1L, notes.getEntityId().longValue());
        assertEquals(1L, notes.getId().longValue());
        assertEquals(2, notes.getGuid().variant());
        assertTrue(notes.getIsActive());
        assertTrue(notes.getIsPublic());
        assertTrue(notes.getIsDeleted());
    }
    /**
     * Method under test: {@link NotesDao#delete(Notes)}
     */
    @Test
    void testDelete2() {
        // Arrange
        doThrow(new DataRetrievalFailureException("Msg")).when(iNotesRepository).delete(Mockito.<Notes>any());
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        // Act and Assert
        assertThrows(DataRetrievalFailureException.class, () -> notesDao.delete(notes));
        verify(iNotesRepository).delete(isA(Notes.class));
    }
    /**
     * Method under test: {@link NotesDao#findByEntityIdAndEntityType(Long, String)}
     */
    @Test
    void testFindByEntityIdAndEntityType() {
        // Arrange
        ArrayList<Notes> notesList = new ArrayList<>();
        when(iNotesRepository.findByEntityIdAndEntityType(Mockito.<Long>any(), Mockito.<String>any()))
                .thenReturn(notesList);
        // Act
        List<Notes> actualFindByEntityIdAndEntityTypeResult = notesDao.findByEntityIdAndEntityType(1L, "Entity Type");
        // Assert
        verify(iNotesRepository).findByEntityIdAndEntityType(eq(1L), eq("Entity Type"));
        assertTrue(actualFindByEntityIdAndEntityTypeResult.isEmpty());
        assertSame(notesList, actualFindByEntityIdAndEntityTypeResult);
    }
    /**
     * Method under test: {@link NotesDao#findByEntityIdAndEntityType(Long, String)}
     */
    @Test
    void testFindByEntityIdAndEntityType2() {
        // Arrange
        when(iNotesRepository.findByEntityIdAndEntityType(Mockito.<Long>any(), Mockito.<String>any()))
                .thenThrow(new DataRetrievalFailureException("Msg"));
        // Act and Assert
        assertThrows(DataRetrievalFailureException.class, () -> notesDao.findByEntityIdAndEntityType(1L, "Entity Type"));
        verify(iNotesRepository).findByEntityIdAndEntityType(eq(1L), eq("Entity Type"));
    }
    /**
     * Method under test:
     * {@link NotesDao#updateEntityFromOtherEntity(List, Long, String)}
     */
    @Test
    void testUpdateEntityFromOtherEntity() throws RunnerException {
        // Arrange
        when(iNotesRepository.findAll(Mockito.<Specification<Notes>>any(), Mockito.<Pageable>any()))
                .thenReturn(new PageImpl<>(new ArrayList<>()));
        // Act
        List<Notes> actualUpdateEntityFromOtherEntityResult = notesDao.updateEntityFromOtherEntity(new ArrayList<>(), 1L,
                "Entity Type");
        // Assert
        verify(iNotesRepository).findAll(isA(Specification.class), isA(Pageable.class));
        assertTrue(actualUpdateEntityFromOtherEntityResult.isEmpty());
    }
    /**
     * Method under test:
     * {@link NotesDao#updateEntityFromOtherEntity(List, Long, String)}
     */
    @Test
    void testUpdateEntityFromOtherEntity2() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<Notes>>any())).thenReturn(notes);
        when(jsonHelper.convertToJson(Mockito.<Notes>any())).thenReturn("Convert To Json");
        Notes notes2 = new Notes();
        notes2.setAssignedTo("alice.liddell@example.org");
        notes2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes2.setEntityId(1L);
        notes2.setEntityType("entityId");
        notes2.setGuid(UUID.randomUUID());
        notes2.setId(1L);
        notes2.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setInsertUserDisplayName("entityId");
        notes2.setInsertUserId("42");
        notes2.setIsActive(true);
        notes2.setIsDeleted(true);
        notes2.setIsPublic(true);
        notes2.setLabel("entityId");
        notes2.setTenantId(1);
        notes2.setText("entityId");
        notes2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setUpdatedBy("2020-03-01");
        ArrayList<Notes> content = new ArrayList<>();
        content.add(notes2);
        PageImpl<Notes> pageImpl = new PageImpl<>(content);
        doNothing().when(iNotesRepository).delete(Mockito.<Notes>any());
        when(iNotesRepository.findAll(Mockito.<Specification<Notes>>any(), Mockito.<Pageable>any())).thenReturn(pageImpl);
        // Act
        List<Notes> actualUpdateEntityFromOtherEntityResult = notesDao.updateEntityFromOtherEntity(new ArrayList<>(), 1L,
                "Entity Type");
        // Assert
        verify(jsonHelper).convertToJson(isA(Notes.class));
        verify(jsonHelper).readFromJson(eq("Convert To Json"), isA(Class.class));
        verify(iNotesRepository).findAll(isA(Specification.class), isA(Pageable.class));
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(iNotesRepository).delete(isA(Notes.class));
        assertTrue(actualUpdateEntityFromOtherEntityResult.isEmpty());
    }

    /**
     * Method under test:
     * {@link NotesDao#updateEntityFromOtherEntity(List, Long, String, List)}
     */
    @Test
    void testUpdateEntityFromOtherEntity5() throws RunnerException {
        // Arrange
        ArrayList<Notes> notesList = new ArrayList<>();
        // Act and Assert
        assertTrue(notesDao.updateEntityFromOtherEntity(notesList, 1L, "Entity Type", new ArrayList<>()).isEmpty());
    }
    /**
     * Method under test:
     * {@link NotesDao#updateEntityFromOtherEntity(List, Long, String, List)}
     */
    @Test
    void testUpdateEntityFromOtherEntity6() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<Notes>>any())).thenReturn(notes);
        when(jsonHelper.convertToJson(Mockito.<Notes>any())).thenReturn("Convert To Json");
        Notes notes2 = new Notes();
        notes2.setAssignedTo("alice.liddell@example.org");
        notes2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes2.setEntityId(1L);
        notes2.setEntityType("Entity Type");
        notes2.setGuid(UUID.randomUUID());
        notes2.setId(1L);
        notes2.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setInsertUserDisplayName("Insert User Display Name");
        notes2.setInsertUserId("42");
        notes2.setIsActive(true);
        notes2.setIsDeleted(true);
        notes2.setIsPublic(true);
        notes2.setLabel("Label");
        notes2.setTenantId(1);
        notes2.setText("Text");
        notes2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setUpdatedBy("2020-03-01");
        Optional<Notes> ofResult = Optional.of(notes2);
        Notes notes3 = new Notes();
        notes3.setAssignedTo("alice.liddell@example.org");
        notes3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes3.setEntityId(1L);
        notes3.setEntityType("Entity Type");
        notes3.setGuid(UUID.randomUUID());
        notes3.setId(1L);
        notes3.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes3.setInsertUserDisplayName("Insert User Display Name");
        notes3.setInsertUserId("42");
        notes3.setIsActive(true);
        notes3.setIsDeleted(true);
        notes3.setIsPublic(true);
        notes3.setLabel("Label");
        notes3.setTenantId(1);
        notes3.setText("Text");
        notes3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes3.setUpdatedBy("2020-03-01");
        when(iNotesRepository.save(Mockito.<Notes>any())).thenReturn(notes3);
        when(iNotesRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        Notes notes4 = new Notes();
        notes4.setAssignedTo("alice.liddell@example.org");
        notes4.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes4.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes4.setEntityId(1L);
        notes4.setEntityType("Entity Type");
        notes4.setGuid(UUID.randomUUID());
        notes4.setId(1L);
        notes4.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes4.setInsertUserDisplayName("Insert User Display Name");
        notes4.setInsertUserId("42");
        notes4.setIsActive(true);
        notes4.setIsDeleted(true);
        notes4.setIsPublic(true);
        notes4.setLabel("Label");
        notes4.setTenantId(1);
        notes4.setText("Text");
        notes4.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes4.setUpdatedBy("2020-03-01");
        ArrayList<Notes> notesList = new ArrayList<>();
        notesList.add(notes4);
        // Act
        List<Notes> actualUpdateEntityFromOtherEntityResult = notesDao.updateEntityFromOtherEntity(notesList, 1L,
                "Entity Type", new ArrayList<>());
        // Assert
        verify(jsonHelper).convertToJson(isA(Notes.class));
        verify(jsonHelper).readFromJson(eq("Convert To Json"), isA(Class.class));
        verify(iNotesRepository).findById(eq(1L));
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(iNotesRepository).save(isA(Notes.class));
        assertEquals(1, actualUpdateEntityFromOtherEntityResult.size());
    }
    /**
     * Method under test:
     * {@link NotesDao#saveEntityFromOtherEntity(List, Long, String)}
     */
    @Test
    void testSaveEntityFromOtherEntity() {
        // Arrange, Act and Assert
        assertTrue(notesDao.saveEntityFromOtherEntity(new ArrayList<>(), 1L, "Entity Type").isEmpty());
    }
    /**
     * Method under test:
     * {@link NotesDao#saveEntityFromOtherEntity(List, Long, String)}
     */
    @Test
    void testSaveEntityFromOtherEntity2() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<Notes>>any())).thenReturn(notes);
        when(jsonHelper.convertToJson(Mockito.<Notes>any())).thenReturn("Convert To Json");
        Notes notes2 = new Notes();
        notes2.setAssignedTo("alice.liddell@example.org");
        notes2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes2.setEntityId(1L);
        notes2.setEntityType("Entity Type");
        notes2.setGuid(UUID.randomUUID());
        notes2.setId(1L);
        notes2.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setInsertUserDisplayName("Insert User Display Name");
        notes2.setInsertUserId("42");
        notes2.setIsActive(true);
        notes2.setIsDeleted(true);
        notes2.setIsPublic(true);
        notes2.setLabel("Label");
        notes2.setTenantId(1);
        notes2.setText("Text");
        notes2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setUpdatedBy("2020-03-01");
        Optional<Notes> ofResult = Optional.of(notes2);
        Notes notes3 = new Notes();
        notes3.setAssignedTo("alice.liddell@example.org");
        notes3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes3.setEntityId(1L);
        notes3.setEntityType("Entity Type");
        notes3.setGuid(UUID.randomUUID());
        notes3.setId(1L);
        notes3.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes3.setInsertUserDisplayName("Insert User Display Name");
        notes3.setInsertUserId("42");
        notes3.setIsActive(true);
        notes3.setIsDeleted(true);
        notes3.setIsPublic(true);
        notes3.setLabel("Label");
        notes3.setTenantId(1);
        notes3.setText("Text");
        notes3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes3.setUpdatedBy("2020-03-01");
        when(iNotesRepository.save(Mockito.<Notes>any())).thenReturn(notes3);
        when(iNotesRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        Notes notes4 = new Notes();
        notes4.setAssignedTo("alice.liddell@example.org");
        notes4.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes4.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes4.setEntityId(1L);
        notes4.setEntityType("Entity Type");
        notes4.setGuid(UUID.randomUUID());
        notes4.setId(1L);
        notes4.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes4.setInsertUserDisplayName("Insert User Display Name");
        notes4.setInsertUserId("42");
        notes4.setIsActive(true);
        notes4.setIsDeleted(true);
        notes4.setIsPublic(true);
        notes4.setLabel("Label");
        notes4.setTenantId(1);
        notes4.setText("Text");
        notes4.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes4.setUpdatedBy("2020-03-01");
        ArrayList<Notes> notesRequests = new ArrayList<>();
        notesRequests.add(notes4);
        // Act
        List<Notes> actualSaveEntityFromOtherEntityResult = notesDao.saveEntityFromOtherEntity(notesRequests, 1L,
                "Entity Type");
        // Assert
        verify(jsonHelper).convertToJson(isA(Notes.class));
        verify(jsonHelper).readFromJson(eq("Convert To Json"), isA(Class.class));
        verify(iNotesRepository).findById(eq(1L));
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(iNotesRepository).save(isA(Notes.class));
        assertEquals(1, actualSaveEntityFromOtherEntityResult.size());
    }
    /**
     * Method under test:
     * {@link NotesDao#saveEntityFromOtherEntity(List, Long, String, Map)}
     */
    @Test
    void testSaveEntityFromOtherEntity4() {
        // Arrange
        ArrayList<Notes> notesList = new ArrayList<>();
        when(iNotesRepository.saveAll(Mockito.<Iterable<Notes>>any())).thenReturn(notesList);
        ArrayList<Notes> notesRequests = new ArrayList<>();
        // Act
        List<Notes> actualSaveEntityFromOtherEntityResult = notesDao.saveEntityFromOtherEntity(notesRequests, 1L,
                "Entity Type", new HashMap<>());
        // Assert
        verify(iNotesRepository).saveAll(isA(Iterable.class));
        assertTrue(actualSaveEntityFromOtherEntityResult.isEmpty());
        assertSame(notesList, actualSaveEntityFromOtherEntityResult);
    }
    /**
     * Method under test:
     * {@link NotesDao#saveEntityFromOtherEntity(List, Long, String, Map)}
     */
    @Test
    void testSaveEntityFromOtherEntity5() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        ArrayList<Notes> notesList = new ArrayList<>();
        notesList.add(notes);
        when(iNotesRepository.saveAll(Mockito.<Iterable<Notes>>any())).thenReturn(notesList);
        ArrayList<Notes> notesRequests = new ArrayList<>();
        // Act
        List<Notes> actualSaveEntityFromOtherEntityResult = notesDao.saveEntityFromOtherEntity(notesRequests, 1L,
                "Entity Type", new HashMap<>());
        // Assert
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(iNotesRepository).saveAll(isA(Iterable.class));
        assertEquals(1, actualSaveEntityFromOtherEntityResult.size());
        assertSame(notesList, actualSaveEntityFromOtherEntityResult);
    }
    /**
     * Method under test:
     * {@link NotesDao#saveEntityFromOtherEntity(List, Long, String, Map)}
     */
    @Test
    void testSaveEntityFromOtherEntity6() {
        // Arrange
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("SHIPMENT");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("SHIPMENT");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("SHIPMENT");
        notes.setTenantId(1);
        notes.setText("SHIPMENT");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        ArrayList<Notes> notesRequests = new ArrayList<>();
        notesRequests.add(notes);
        // Act and Assert
        assertThrows(DataRetrievalFailureException.class,
                () -> notesDao.saveEntityFromOtherEntity(notesRequests, 1L, "Entity Type", new HashMap<>()));
    }
    /**
     * Method under test:
     * {@link NotesDao#saveEntityFromOtherEntity(List, Long, String, Map)}
     */
    @Test
    void testSaveEntityFromOtherEntity7() {
        // Arrange
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("SHIPMENT");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("SHIPMENT");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("SHIPMENT");
        notes.setTenantId(1);
        notes.setText("SHIPMENT");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        Notes notes2 = new Notes();
        notes2.setAssignedTo("Notes is null for Id {}");
        notes2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setCreatedBy("Notes is null for Id {}");
        notes2.setEntityId(2L);
        notes2.setEntityType("Failed to fetch data for given constraint.");
        notes2.setGuid(UUID.randomUUID());
        notes2.setId(2L);
        notes2.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setInsertUserDisplayName("Failed to fetch data for given constraint.");
        notes2.setInsertUserId("Notes is null for Id {}");
        notes2.setIsActive(false);
        notes2.setIsDeleted(false);
        notes2.setIsPublic(false);
        notes2.setLabel("Failed to fetch data for given constraint.");
        notes2.setTenantId(2);
        notes2.setText("Failed to fetch data for given constraint.");
        notes2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes2.setUpdatedBy("2020/03/01");
        ArrayList<Notes> notesRequests = new ArrayList<>();
        notesRequests.add(notes2);
        notesRequests.add(notes);
        // Act and Assert
        assertThrows(DataRetrievalFailureException.class,
                () -> notesDao.saveEntityFromOtherEntity(notesRequests, 1L, "Entity Type", new HashMap<>()));
    }
    /**
     * Method under test:
     * {@link NotesDao#saveEntityFromOtherEntity(List, Long, String, Map)}
     */
    @Test
    void testSaveEntityFromOtherEntity8() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        ArrayList<Notes> notesList = new ArrayList<>();
        notesList.add(notes);
        when(iNotesRepository.saveAll(Mockito.<Iterable<Notes>>any())).thenReturn(notesList);
        ArrayList<Notes> notesRequests = new ArrayList<>();
        // Act
        List<Notes> actualSaveEntityFromOtherEntityResult = notesDao.saveEntityFromOtherEntity(notesRequests, 1L,
                "SHIPMENT", new HashMap<>());
        // Assert
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(iNotesRepository).saveAll(isA(Iterable.class));
        assertEquals(1, actualSaveEntityFromOtherEntityResult.size());
        assertSame(notesList, actualSaveEntityFromOtherEntityResult);
    }
    /**
     * Method under test:
     * {@link NotesDao#saveEntityFromOtherEntity(List, Long, String, Map)}
     */
    @Test
    void testSaveEntityFromOtherEntity9() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doThrow(new RunnerException("SHIPMENT")).when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());
        Notes notes = new Notes();
        notes.setAssignedTo("alice.liddell@example.org");
        notes.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        notes.setEntityId(1L);
        notes.setEntityType("Entity Type");
        notes.setGuid(UUID.randomUUID());
        notes.setId(1L);
        notes.setInsertDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setInsertUserDisplayName("Insert User Display Name");
        notes.setInsertUserId("42");
        notes.setIsActive(true);
        notes.setIsDeleted(true);
        notes.setIsPublic(true);
        notes.setLabel("Label");
        notes.setTenantId(1);
        notes.setText("Text");
        notes.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        notes.setUpdatedBy("2020-03-01");
        ArrayList<Notes> notesList = new ArrayList<>();
        notesList.add(notes);
        when(iNotesRepository.saveAll(Mockito.<Iterable<Notes>>any())).thenReturn(notesList);
        ArrayList<Notes> notesRequests = new ArrayList<>();
        // Act
        List<Notes> actualSaveEntityFromOtherEntityResult = notesDao.saveEntityFromOtherEntity(notesRequests, 1L,
                "Entity Type", new HashMap<>());
        // Assert
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(iNotesRepository).saveAll(isA(Iterable.class));
        assertEquals(1, actualSaveEntityFromOtherEntityResult.size());
        assertSame(notesList, actualSaveEntityFromOtherEntityResult);
    }
}
