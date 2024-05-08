//package com.dpw.runner.shipment.services.dao.impl;
//
//import com.dpw.runner.shipment.services.Kafka.Dto.AwbShipConsoleDto;
//import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
//import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
//import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
//import com.dpw.runner.shipment.services.commons.constants.Constants;
//import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
//import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
//import com.dpw.runner.shipment.services.dto.request.UsersDto;
//import com.dpw.runner.shipment.services.dto.request.awb.AwbOCIInfo;
//import com.dpw.runner.shipment.services.dto.request.awb.AwbOtherChargesInfo;
//import com.dpw.runner.shipment.services.dto.request.awb.AwbShipmentInfo;
//import com.dpw.runner.shipment.services.dto.response.AwbResponse;
//import com.dpw.runner.shipment.services.entity.Awb;
//import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
//import com.dpw.runner.shipment.services.entity.ShipmentDetails;
//import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
//import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
//import com.dpw.runner.shipment.services.helper.JsonTestUtility;
//import com.dpw.runner.shipment.services.helpers.JsonHelper;
//import org.junit.jupiter.api.AfterAll;
//import org.junit.jupiter.api.BeforeAll;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.junit.jupiter.api.parallel.Execution;
//import org.junit.runner.RunWith;
//import org.mockito.MockitoAnnotations;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.boot.test.mock.mockito.MockBean;
//import org.springframework.test.context.DynamicPropertyRegistry;
//import org.springframework.test.context.DynamicPropertySource;
//import org.springframework.test.context.TestPropertySource;
//import org.springframework.test.context.junit4.SpringRunner;
//import org.testcontainers.containers.PostgreSQLContainer;
//import org.testcontainers.junit.jupiter.Container;
//import org.testcontainers.junit.jupiter.Testcontainers;
//
//import java.io.IOException;
//import java.util.*;
//
//import static org.junit.jupiter.api.Assertions.*;
//import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
//import static org.mockito.ArgumentMatchers.*;
//import static org.mockito.Mockito.*;
//
//
//@RunWith(SpringRunner.class)
//@ExtendWith(MockitoExtension.class)
//@TestPropertySource("classpath:application-test.properties")
//@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
//@Testcontainers
//@Execution(CONCURRENT)
//class AwbDaoTest {
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
//    private AwbDao awbDao;
//
//    @MockBean
//    private IShipmentDao shipmentDao;
//    @MockBean
//    private KafkaProducer producer;
//    @MockBean
//    private JsonHelper jsonHelper;
//    @MockBean
//    private IConsolidationDetailsDao consolidationDetailsDao;
//
//    private static JsonTestUtility jsonTestUtility;
//    private static Awb mockAwb;
//
//
//
//    @BeforeAll
//    static void beforeAll() throws IOException {
//        postgresContainer.start();
//        jsonTestUtility = new JsonTestUtility();
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
//        TenantContext.setCurrentTenant(1);
//        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
//        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
//        mockAwb = jsonTestUtility.getTestHawb();
//    }
//
//    @Test
//    public void test_saveNewAwbShipmentInfo() {
//        try {
//            Awb savedAwb = awbDao.save(mockAwb);
//            // Assert that the savedAwb is not null and contains expected values
//            assertNotNull(savedAwb);
//            assertEquals(1L, savedAwb.getId());
//            assertNotNull(savedAwb.getGuid());
//        } catch (RunnerException e) {
//            // Handle the exception if necessary
//            fail("Exception occurred: " + e.getMessage());
//        }
//    }
//
//    @Test
//    public void testSaveNewAwbShipmentInfoThrowsExceptionWhenDuplicateOciInfo() {
//        try {
//            Awb awb = mockAwb;
//            AwbOCIInfo awbOCIInfo = AwbOCIInfo.builder().informationIdentifier(1).tradeIdentificationCode(1).build();
//            AwbOCIInfo duplicateAwbOciInfo = AwbOCIInfo.builder().informationIdentifier(1).tradeIdentificationCode(1).build();
//            awb.setAwbOciInfo(List.of(awbOCIInfo, duplicateAwbOciInfo));
//            Awb savedAwb = awbDao.save(awb);
//
//        } catch (RunnerException e) {
//            // Handle the exception if necessary
//            Set<String> errors = new HashSet<>();
//            errors.add(AwbConstants.DUPLICATE_PAIR_AWB_OCI_INFO_VALIDATION);
//            String errorMessage = errors.toString();
//            assertEquals(errorMessage, e.getMessage());
//        }
//    }
//
//    @Test
//    public void testSaveNewAwbShipmentInfoThrowsExceptionWhenIataDescriptionLengthGreaterThan3() {
//        try {
//            Awb awb = mockAwb;
//            AwbOtherChargesInfo awbOtherChargesInfo = AwbOtherChargesInfo.builder().iataDescription("ABCD").build();
//            awb.setAwbOtherChargesInfo(List.of(awbOtherChargesInfo));
//            Awb savedAwb = awbDao.save(awb);
//
//        } catch (RunnerException e) {
//            // Handle the exception if necessary
//            Set<String> errors = new HashSet<>();
//            errors.add(AwbConstants.IATA_DESCRIPTION_FIELD_VALIDATION);
//            String errorMessage = errors.toString();
//            assertEquals(errorMessage, e.getMessage());
//        }
//    }
//
//    @Test
//    void testPushToKafka_WithShipmentId() {
//        Awb awb = new Awb();
//        awb.setShipmentId(1L);
//        awb.setAwbShipmentInfo(AwbShipmentInfo.builder().entityType(Constants.DMAWB).build());
//
//        AwbResponse awbResponse = new AwbResponse();
//        AwbShipConsoleDto awbShipConsoleDto = new AwbShipConsoleDto();
//        awbResponse.setAwbKafkaEntity(awbShipConsoleDto);
//
//        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
//        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(awbResponse);
//        when(jsonHelper.convertValue(any(), eq(AwbShipConsoleDto.class))).thenReturn(awbShipConsoleDto);
//        when(producer.getKafkaResponse(eq(awbResponse), anyBoolean())).thenReturn(new KafkaResponse());
//
//        awbDao.pushToKafka(awb, true);
//
////        verify(producer, atLeast(1)).getKafkaResponse(any(), anyBoolean());
////        verify(producer, times(1)).produceToKafka(any(), any(), any());
//    }
//
//    @Test
//    void testPushToKafka_WithConsolidationId() {
//        Awb awb = new Awb();
//        awb.setConsolidationId(1L);
//        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));
//        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(new AwbResponse());
//        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());
//
//        awbDao.pushToKafka(awb, false);
//
////        verify(jsonHelper).convertValue(any(), eq(AwbResponse.class));
////        verify(producer).getKafkaResponse(any(), eq(false));
////        verify(producer).produceToKafka(any(), any(), any());
//    }
//
//    @Test
//    void testPushToKafka_WithInvalidShipmentId() {
//        Awb awb = new Awb();
//        awb.setShipmentId(1L);
//        when(shipmentDao.findById(any())).thenReturn(Optional.empty());
//
//        awbDao.pushToKafka(awb, true);
//
//        verifyNoInteractions(producer);
//    }
//
//    @Test
//    void testPushToKafka_WithInvalidConsolidationId() {
//        Awb awb = new Awb();
//        awb.setConsolidationId(1L);
//        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());
//
//        awbDao.pushToKafka(awb, false);
//
//        verifyNoInteractions(producer);
//    }
//
//    @Test
//    void testPushToKafka_WithNullTenantId() {
//        Awb awb = new Awb();
//        awb.setTenantId(null);
//        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(new AwbResponse());
//        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());
//
//        awbDao.pushToKafka(awb, false);
//
////        verify(jsonHelper).convertValue(any(), eq(AwbResponse.class));
////        verify(producer).getKafkaResponse(any(), eq(false));
////        verify(producer).produceToKafka(any(), any(), any());
//    }
//
//    @Test
//    void testPushToKafka_WithException() {
//        Awb awb = new Awb();
//        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenThrow(new RuntimeException());
//
//        awbDao.pushToKafka(awb, false);
//
//        verifyNoInteractions(producer);
//    }
//
//}
