package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

@RunWith(SpringRunner.class)
@ExtendWith(MockitoExtension.class)
@TestPropertySource("classpath:application-test.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@Execution(CONCURRENT)
class CustomerBookingDaoTest {

    @Container
    private static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");
    private static JsonTestUtility jsonTestUtility;

    static {
        postgresContainer = new PostgreSQLContainer("postgres:15-alpine")
                .withDatabaseName("integration-tests-db")
                .withUsername("sa")
                .withPassword("sa");
        postgresContainer.start();
    }

    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @BeforeAll
    static void beforeAll() throws IOException {
        postgresContainer.start();
        jsonTestUtility = new JsonTestUtility();
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
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        Parties mockParty = Parties.builder().orgCode("ORG123").addressCode("ADDR123").build();
        Parties mockParty2 = Parties.builder().orgCode("ORG323").addressCode("ADER123").build();
        Parties mockParty3 = Parties.builder().orgCode("ORG325").addressCode("ADEE123").build();

        CarrierDetails carrierDetails = CarrierDetails.builder().origin("origin").destination("destination")
                .originPort("origninPort").destinationPort("destinationPort").build();

        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setBookingDate(LocalDateTime.now());
        customerBooking.setBookingNumber("BKN33123");
        customerBooking.setBookingStatus(BookingStatus.READY_FOR_SHIPMENT);
        customerBooking.setTransportType(Constants.TRANSPORT_MODE_SEA);
        customerBooking.setServiceMode("service mode");
        customerBooking.setBookingCharges(List.of(new BookingCharges()));
        customerBooking.setCustomer(mockParty);
        customerBooking.setConsignee(mockParty2);
        customerBooking.setConsignor(mockParty3);
        customerBooking.setIncoTerms("Inco Terms");
        customerBooking.setCargoType(Constants.CARGO_TYPE_FCL);
        customerBooking.setCarrierDetails(carrierDetails);

        // Test
        CustomerBooking savedCustomerBooking = customerBookingDao.save(customerBooking);

        // Assert
        assertNotNull(savedCustomerBooking.getId());
    }

    // create tc for save by using input that fails all validations one by one to inc coverage
}