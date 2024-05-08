//package com.dpw.runner.shipment.services;
//
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
//import com.dpw.runner.shipment.services.dao.interfaces.INotesDao;
//import com.dpw.runner.shipment.services.dto.request.UsersDto;
//import com.dpw.runner.shipment.services.entity.Notes;
//import org.junit.Test;
//import org.junit.jupiter.api.AfterAll;
//import org.junit.jupiter.api.BeforeAll;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.runner.RunWith;
//import org.mockito.MockitoAnnotations;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.test.context.DynamicPropertyRegistry;
//import org.springframework.test.context.DynamicPropertySource;
//import org.springframework.test.context.TestPropertySource;
//import org.springframework.test.context.junit4.SpringRunner;
//import org.testcontainers.containers.PostgreSQLContainer;
//import org.testcontainers.junit.jupiter.Container;
//import org.testcontainers.junit.jupiter.Testcontainers;
//
//import java.util.ArrayList;
//import java.util.Collections;
//import java.util.HashMap;
//import java.util.Optional;
//
//import static org.junit.jupiter.api.Assertions.assertTrue;
//
////@ExtendWith({MockitoExtension.class, SpringExtension.class})
//@RunWith(SpringRunner.class)
//@TestPropertySource("classpath:application-test.properties")
//@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
//@Testcontainers
//public class TestContainerTest {
//
//    @Container
//    private static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");
//
//    static {
//        postgresContainer = new PostgreSQLContainer("postgres:15-alpine")
//                .withDatabaseName("integration-tests-db")
//                .withUsername("sa")
//                .withPassword("sa");
//        postgresContainer.start();
//    }
//
//    @Autowired
//    private INotesDao dao;
//
//    @BeforeAll
//    static void beforeAll() {
//        postgresContainer.start();
//    }
//
//    @AfterAll
//    static void afterAll() {
//        postgresContainer.stop();
//    }
//
//    @DynamicPropertySource
//    static void dynamicConfiguration(DynamicPropertyRegistry registry){
//        registry.add("spring.datasource.url", postgresContainer::getJdbcUrl);
//        registry.add("spring.datasource.username", postgresContainer::getUsername);
//        registry.add("spring.datasource.password", postgresContainer::getPassword);
//    }
//
//    @BeforeEach
//    void setUp() {
//        MockitoAnnotations.openMocks(this);
//        UserContext.setUser(UsersDto.builder().Username("user").build()); // Set up a mock user for testing
//    }
//
//
//    @Test
//    public void testFetchByQuery_request3() throws Exception {
//        Notes notes = new Notes();
//        TenantContext.setCurrentTenant(2);
//        UserContext.setUser(UsersDto.builder().Username("user").Permissions(new HashMap<>()).build()); // Set up a mock user for testing
//        notes.setText("runner testcontainer test");
//        dao.save(notes);
//        Optional<Notes> savedNote = dao.findById(1L);
//        assertTrue(savedNote.get().getText().equals("runner testcontainer test"));
//        assertTrue(savedNote.isPresent());
//        ArrayList<Integer> ar = new ArrayList<>();
//        Collections.sort(ar , (a , b) -> b.compareTo(a));
//    }
//
//}