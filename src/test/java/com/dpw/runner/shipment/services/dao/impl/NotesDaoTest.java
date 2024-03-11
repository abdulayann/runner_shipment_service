package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Notes;
import com.dpw.runner.shipment.services.repository.interfaces.INotesRepository;
import org.junit.Test;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

//@ExtendWith({MockitoExtension.class, SpringExtension.class})
@RunWith(SpringRunner.class)
@TestPropertySource("classpath:application-test.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
public class NotesDaoTest {

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

    @MockBean
    private INotesRepository notesRepository;

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
        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
    }

    @Test
    public void testSave() {
        Notes notes = new Notes();
        when(notesRepository.save(notes)).thenReturn(notes);

        Notes savedNotes = dao.save(notes);

        assertEquals(notes, savedNotes);
        Mockito.verify(notesRepository).save(notes);
    }

    @Test
    public void testSaveAll() {
        List<Notes> notesList = new ArrayList<>();
        notesList.add(Notes.builder().text("SampleNote").build());
        when(notesRepository.saveAll(notesList)).thenReturn(notesList);

        List<Notes> savedNotesList = dao.saveAll(notesList);

        assertEquals(notesList.size(), savedNotesList.size());
        Mockito.verify(notesRepository,times(1)).saveAll(notesList);
    }

    @Test
    public void testFindAll() {
        Pageable pageable = PageRequest.of(0, 10, Sort.by("id").ascending());
        Page<Notes> page = Page.empty(pageable); // Create an empty page for testing
//        when(notesRepository.findAll(null, pageable)).thenReturn(page);

        Page<Notes> retrievedPage = dao.findAll(null, pageable);

        assertTrue(retrievedPage.isEmpty());
    }

    @Test
    public void testFindById() {
        Long id = 1L;
        Notes notes = new Notes();
        when(notesRepository.findById(id)).thenReturn(Optional.of(notes));

        Optional<Notes> retrievedNotes = dao.findById(id);

        assertTrue(retrievedNotes.isPresent());
        assertEquals(notes, retrievedNotes.get());
        Mockito.verify(notesRepository).findById(id);
    }

    @Test
    public void testDelete() {
        Notes notes = new Notes();

        dao.delete(notes);

        Mockito.verify(notesRepository).delete(notes);
    }

    @Test
    public void testFindByEntityIdAndEntityType() {
        Long entityId = 1L;
        String entityType = "type";
        List<Notes> notesList = new ArrayList<>();
        // Add some Notes objects to the list

        when(notesRepository.findByEntityIdAndEntityType(entityId, entityType)).thenReturn(notesList);

        List<Notes> retrievedNotesList = dao.findByEntityIdAndEntityType(entityId, entityType);

        assertEquals(notesList.size(), retrievedNotesList.size());
        Mockito.verify(notesRepository).findByEntityIdAndEntityType(entityId, entityType);
    }
}