package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Notes;
import com.nimbusds.jose.util.Pair;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

//@ExtendWith({MockitoExtension.class, SpringExtension.class})
@RunWith(SpringRunner.class)
@TestPropertySource("classpath:application-test.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@Execution(CONCURRENT)
class NotesDaoTest {

    @Container
    private static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");

    static {
        postgresContainer = new PostgreSQLContainer("postgres:15-alpine")
                .withDatabaseName("integration-tests-db")
                .withUsername("sa")
                .withPassword("sa");
        postgresContainer.start();
    }

    @Autowired
    private INotesDao dao;

    @BeforeAll
    static void beforeAll() {
        postgresContainer.start();
    }

    @AfterAll
    static void afterAll() {
        postgresContainer.stop();
    }

    @DynamicPropertySource
    static void dynamicConfiguration(DynamicPropertyRegistry registry){
        registry.add("spring.datasource.url", postgresContainer::getJdbcUrl);
        registry.add("spring.datasource.username", postgresContainer::getUsername);
        registry.add("spring.datasource.password", postgresContainer::getPassword);
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
    }

    @Test
    public void testSave() {
        Notes notes = new Notes();
        notes.setText("Text note");
//        when(notesRepository.save(notes)).thenReturn(notes);

        Notes savedNotes = dao.save(notes);

        assertEquals(notes, savedNotes);
//        Mockito.verify(notesRepository).save(notes);
    }

    @Test
    public void testSaveAll() {
        List<Notes> notesList = new ArrayList<>();
        notesList.add(Notes.builder().text("SampleNote").build());
//        when(notesRepository.saveAll(notesList)).thenReturn(notesList);

        List<Notes> savedNotesList = dao.saveAll(notesList);

        assertEquals(notesList.size(), savedNotesList.size());
//        Mockito.verify(notesRepository,times(1)).saveAll(notesList);
    }

    @Test
    public void testFindAll() {
        Notes notes = new Notes();
        notes.setText("sample note1");
        dao.save(notes);
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", 1 , "=");
        Pair<Specification<Notes>, Pageable> pair = fetchData(listCommonRequest, Notes.class);
        Page<Notes> retrievedPage = dao.findAll(pair.getLeft(), pair.getRight());
        assertFalse(retrievedPage.isEmpty());
        assertTrue(!retrievedPage.getContent().isEmpty());
        assertTrue(retrievedPage.getContent().get(0).equals(notes));
    }

    @Test
    public void testFindById() {
        Long id = 1L;
        Notes notes = new Notes();
        notes.setText("sample note1");
        dao.save(notes);

        Optional<Notes> retrievedNotes = dao.findById(id);

        assertTrue(retrievedNotes.isPresent());
        assertEquals(notes, retrievedNotes.get());
    }

    @Test
    public void testDelete() {
        Notes notes = new Notes();

        dao.delete(notes);

//        Mockito.verify(notesRepository).delete(notes);
    }

    @Test
    public void testFindByEntityIdAndEntityType() {
        Long entityId = 1L;
        String entityType = "type";
        List<Notes> notesList = new ArrayList<>();
        // Add some Notes objects to the list

//        when(notesRepository.findByEntityIdAndEntityType(entityId, entityType)).thenReturn(notesList);

        List<Notes> retrievedNotesList = dao.findByEntityIdAndEntityType(entityId, entityType);

        assertEquals(notesList.size(), retrievedNotesList.size());
//        Mockito.verify(notesRepository).findByEntityIdAndEntityType(entityId, entityType);
    }
}