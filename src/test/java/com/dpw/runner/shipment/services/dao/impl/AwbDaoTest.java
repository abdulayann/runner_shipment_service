package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.Kafka.Dto.AwbShipConsoleDto;
import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbOCIInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbOtherChargesInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbShipmentInfo;
import com.dpw.runner.shipment.services.dto.response.AwbAirMessagingResponse;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.PrintType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.utils.AwbUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AwbDaoTest {

    @InjectMocks
    private AwbDao awbDao;

    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private KafkaProducer producer;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private IAwbRepository awbRepository;
    @Mock
    private AwbUtility awbUtility;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private IEventDao eventDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static Awb mockAwb;
    private static ShipmentDetails testShipment;
    private static ConsolidationDetails testConsol;
    private static Awb testMawb;



    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapperTest = JsonTestUtility.getMapper();
    }


    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());

        mockAwb = jsonTestUtility.getTestHawb();
        testShipment = jsonTestUtility.getTestShipment();

        testConsol = jsonTestUtility.getJson("MAWB_CONSOLIDATION", ConsolidationDetails.class);
        testMawb = jsonTestUtility.getTestMawb();
    }

    @Test
    void test_saveNewAwbShipmentInfo() {
        try {

            Awb savedAwb = objectMapperTest.convertValue(mockAwb, Awb.class);
            savedAwb.setId(1L);
            when(awbRepository.save(any(Awb.class))).thenReturn(savedAwb);

            mockAwb.setId(null);
            Awb response = awbDao.save(mockAwb);

            // Assert that the savedAwb is not null and contains expected values
            assertNotNull(response);
            assertEquals(1L, response.getId());
            assertNotNull(response.getGuid());
        } catch (RunnerException e) {
            // Handle the exception if necessary
            fail("Exception occurred: " + e.getMessage());
        }
    }

    @Test
    void testSaveNewAwbShipmentInfoThrowsExceptionWhenDuplicateOciInfo() {
        try {
            Awb awb = mockAwb;
            AwbOCIInfo awbOCIInfo = AwbOCIInfo.builder().informationIdentifier(1).tradeIdentificationCode(1).build();
            AwbOCIInfo duplicateAwbOciInfo = AwbOCIInfo.builder().informationIdentifier(1).tradeIdentificationCode(1).build();
            awb.setAwbOciInfo(List.of(awbOCIInfo, duplicateAwbOciInfo));
            Awb savedAwb = awbDao.save(awb);

        } catch (RunnerException e) {
            // Handle the exception if necessary
            Set<String> errors = new HashSet<>();
            errors.add(AwbConstants.DUPLICATE_PAIR_AWB_OCI_INFO_VALIDATION);
            String errorMessage = errors.toString();
            assertEquals(errorMessage, e.getMessage());
        }
    }

    @Test
    void testSaveNewAwbShipmentInfoThrowsExceptionWhenIataDescriptionLengthGreaterThan3() {
        try {
            Awb awb = mockAwb;
            AwbOtherChargesInfo awbOtherChargesInfo = AwbOtherChargesInfo.builder().iataDescription("ABCD").build();
            awb.setAwbOtherChargesInfo(List.of(awbOtherChargesInfo));
            Awb savedAwb = awbDao.save(awb);

        } catch (RunnerException e) {
            // Handle the exception if necessary
            Set<String> errors = new HashSet<>();
            errors.add(AwbConstants.IATA_DESCRIPTION_FIELD_VALIDATION);
            String errorMessage = errors.toString();
            assertEquals(errorMessage, e.getMessage());
        }
    }

    @Test
    void testPushToKafka_WithShipmentId() {
        Awb awb = new Awb();
        awb.setShipmentId(1L);
        awb.setAwbShipmentInfo(AwbShipmentInfo.builder().entityType(Constants.DMAWB).build());

        AwbResponse awbResponse = new AwbResponse();
        AwbShipConsoleDto awbShipConsoleDto = new AwbShipConsoleDto();
        awbResponse.setAwbKafkaEntity(awbShipConsoleDto);

        when(shipmentDao.findById(any())).thenReturn(Optional.of(new ShipmentDetails()));
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(awbResponse);
        when(jsonHelper.convertValue(any(), eq(AwbShipConsoleDto.class))).thenReturn(awbShipConsoleDto);
        when(producer.getKafkaResponse(eq(awbResponse), anyBoolean())).thenReturn(new KafkaResponse());

        awbDao.pushToKafka(awb, true);

//        verify(producer, atLeast(1)).getKafkaResponse(any(), anyBoolean());
//        verify(producer, times(1)).produceToKafka(any(), any(), any());
    }

    @Test
    void testPushToKafka_WithConsolidationId() {
        Awb awb = new Awb();
        awb.setConsolidationId(1L);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(new AwbResponse());
        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());

        awbDao.pushToKafka(awb, false);

//        verify(jsonHelper).convertValue(any(), eq(AwbResponse.class));
//        verify(producer).getKafkaResponse(any(), eq(false));
//        verify(producer).produceToKafka(any(), any(), any());
    }

    @Test
    void testPushToKafka_WithInvalidShipmentId() {
        Awb awb = new Awb();
        awb.setShipmentId(1L);
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());

        awbDao.pushToKafka(awb, true);

        verifyNoInteractions(producer);
    }

    @Test
    void testPushToKafka_WithInvalidConsolidationId() {
        Awb awb = new Awb();
        awb.setConsolidationId(1L);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());

        awbDao.pushToKafka(awb, false);

        verifyNoInteractions(producer);
    }

    @Test
    void testPushToKafka_WithNullTenantId() {
        Awb awb = new Awb();
        awb.setTenantId(null);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(new AwbResponse());
//        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());

        awbDao.pushToKafka(awb, false);

//        verify(jsonHelper).convertValue(any(), eq(AwbResponse.class));
//        verify(producer).getKafkaResponse(any(), eq(false));
//        verify(producer).produceToKafka(any(), any(), any());
    }

    @Test
    void testPushToKafka_WithException() {
        Awb awb = new Awb();
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenThrow(new RuntimeException());

        awbDao.pushToKafka(awb, false);

        verifyNoInteractions(producer);
    }

    @Test
    void testFindAll() {

        Specification<Awb> spec = null; // Replace with your dummy specification
        Pageable pageable = null;

        // Mock the behavior of awbRepository.findAll()
        Page<Awb> mockPage = null; // Replace with your dummy Page<Awb>
        when(awbRepository.findAll(spec, pageable)).thenReturn(mockPage);

        // Call the method from the service
        Page<Awb> result = awbDao.findAll(spec, pageable);

        assertEquals(mockPage, result);
    }

    @Test
    void testFindById() {
        Long id = 1L;
        Optional<Awb> optionalResponse = Optional.of(mockAwb);

        when(awbRepository.findById(id)).thenReturn(optionalResponse);

        // Call the method from the service
        var result = awbDao.findById(id);

        assertEquals(optionalResponse, result);
    }

    @Test
    void testFindByGuid() {
        UUID guid = UUID.randomUUID();
        Optional<Awb> optionalResponse = Optional.of(mockAwb);

        when(awbRepository.findByGuid(guid)).thenReturn(optionalResponse);

        // Call the method from the service
        var result = awbDao.findByGuid(guid);

        assertEquals(optionalResponse, result);
    }

    @Test
    void testFindByShipmentId() {
        Long shipmentId = 1L;
        List<Awb> listResponse = List.of(mockAwb);

        when(awbRepository.findByShipmentId(shipmentId)).thenReturn(listResponse);

        // Call the method from the service
        var result = awbDao.findByShipmentId(shipmentId);

        assertEquals(listResponse, result);
    }

    @Test
    void testFindByConsolidationId() {
        Long consolidationId = 1L;
        List<Awb> listResponse = List.of(testMawb);

        when(awbRepository.findByConsolidationId(consolidationId)).thenReturn(listResponse);

        // Call the method from the service
        var result = awbDao.findByConsolidationId(consolidationId);

        assertEquals(listResponse, result);
    }

    @Test
    void testFindByIssuingAgent() {
        String issuingAgent = "testAgent";
        List<Awb> listResponse = List.of(testMawb);

        when(awbRepository.findByIssuingAgent(issuingAgent)).thenReturn(listResponse);

        // Call the method from the service
        var result = awbDao.findByIssuingAgent(issuingAgent);

        assertEquals(listResponse, result);
    }

    @Test
    void testFindByAwbNumber() {
        List<String> awbNumbers = List.of(testMawb.getAwbNumber());
        List<Awb> listResponse = List.of(testMawb);

        when(awbRepository.findByAwbNumber(awbNumbers)).thenReturn(listResponse);

        // Call the method from the service
        var result = awbDao.findByAwbNumber(awbNumbers);

        assertEquals(listResponse, result);
    }

    @Test
    void testFindByAwbNumberAndIssuingAgent() {
        String issuingAgent = "testAgent";
        List<String> awbNumbers = List.of(testMawb.getAwbNumber());
        List<Awb> listResponse = List.of(testMawb);

        when(awbRepository.findByAwbNumberAndIssuingAgent(awbNumbers, issuingAgent)).thenReturn(listResponse);

        // Call the method from the service
        var result = awbDao.findByAwbNumberAndIssuingAgent(awbNumbers, issuingAgent);

        assertEquals(listResponse, result);
    }

    @Test
    void testSaveAll() {
        List<Awb> inputAwbList = List.of(mockAwb, testMawb);
        when(awbRepository.saveAll(inputAwbList)).thenReturn(inputAwbList);

        // Test
        var res = awbDao.saveAll(inputAwbList);

        assertEquals(inputAwbList, res);
    }

    @Test
    void testAirMessagingIntegrationForMawb() {
        Long consolidationId = 1L;
        String reportType = ReportConstants.MAWB;
        Boolean fromShipment = false;

        testConsol.setShipmentsList(List.of(testShipment));

        AwbAirMessagingResponse mockAirMessagingResponse = new AwbAirMessagingResponse();

        // Mock
        when(awbRepository.findByConsolidationId(consolidationId)).thenReturn(List.of(testMawb));
        when(awbRepository.findByShipmentId(testShipment.getId())).thenReturn(List.of(mockAwb));
        when(consolidationDetailsDao.findById(consolidationId)).thenReturn(Optional.of(testConsol));
        when(awbUtility.createAirMessagingRequestForConsole(any(), any())).thenReturn(mockAirMessagingResponse);
        when(awbUtility.createAirMessagingRequestForShipment(any(), any())).thenReturn(mockAirMessagingResponse);

        // Test
        try {
            awbDao.airMessagingIntegration(consolidationId, reportType, fromShipment);
        } catch (Exception e) {
            fail("Unexpected error occured", e);
        }

    }

    @Test
    void testAirMessagingIntegrationForDMawb() {
        Long shipmentId = 1L;
        String reportType = ReportConstants.MAWB;
        Boolean fromShipment = true;

        AwbAirMessagingResponse mockAirMessagingResponse = new AwbAirMessagingResponse();

        // Mock
        when(awbRepository.findByShipmentId(shipmentId)).thenReturn(List.of(mockAwb));
        when(shipmentDao.findById(shipmentId)).thenReturn(Optional.of(testShipment));
        when(awbUtility.createAirMessagingRequestForShipment(any(), any())).thenReturn(mockAirMessagingResponse);

        // Test
        try {
            awbDao.airMessagingIntegration(shipmentId, reportType, fromShipment);
        } catch (Exception e) {
            fail("Unexpected error occured", e);
        }

    }

    @Test
    void testUpdateAirMessageStatusFromShipmentId() {
        Long id = 1L;
        String status = "SUCCESS";

        int responseCount = 2;
        when(awbRepository.updateAirMessageStatusFromShipmentId(id, status)).thenReturn(responseCount);

        var res = awbDao.updateAirMessageStatusFromShipmentId(id, status);

        assertEquals(responseCount, res);
    }

    @Test
    void testUpdateAirMessageStatusFromConsolidationId() {
        Long id = 1L;
        String status = "SUCCESS";

        int responseCount = 2;
        when(awbRepository.updateAirMessageStatusFromConsolidationId(id, status)).thenReturn(responseCount);

        var res = awbDao.updateAirMessageStatusFromConsolidationId(id, status);

        assertEquals(responseCount, res);
    }

    @Test
    void testUpdatePrintTypeFromConsolidationId() {
        Long id = 1L;
        int responseCount = 1;
        var mockAWB = new Awb();
        mockAWB.setPrintType(PrintType.DRAFT_PRINTED);
        when(awbRepository.findByConsolidationId(anyLong())).thenReturn(List.of(mockAWB));

        when(awbRepository.updatePrintTypeFromConsolidationId(id, PrintType.ORIGINAL_PRINTED.name())).thenReturn(responseCount);

        var res = awbDao.updatePrintTypeFromConsolidationId(id, PrintType.ORIGINAL_PRINTED.name());

        assertEquals(responseCount, res);
    }

    @Test
    void testUpdatePrintTypeFromConsolidationId2() {
        Long id = 1L;
        int responseCount = 0;
        var mockAWB = new Awb();
        mockAWB.setPrintType(PrintType.ORIGINAL_PRINTED);

        when(awbRepository.findByConsolidationId(anyLong())).thenReturn(List.of(mockAWB));
        var res = awbDao.updatePrintTypeFromConsolidationId(id, PrintType.ORIGINAL_PRINTED.name());

        assertEquals(responseCount, res);
    }
    @Test
    void testUpdatePrintTypeFromShipmentId() {
        Long id = 1L;
        int responseCount = 0;

        var mockAWB = new Awb();
        mockAWB.setPrintType(PrintType.ORIGINAL_PRINTED);

        when(awbRepository.findByShipmentId(anyLong())).thenReturn(List.of(mockAWB));
        var res = awbDao.updatePrintTypeFromShipmentId(id, PrintType.ORIGINAL_PRINTED.name());

        assertEquals(responseCount, res);
    }

    @Test
    void testUpdatePrintTypeFromShipmentId2() {
        Long id = 1L;
        int responseCount = 1;

        var mockAWB = new Awb();
        mockAWB.setPrintType(PrintType.DRAFT_PRINTED);

        when(awbRepository.updatePrintTypeFromShipmentId(id, PrintType.ORIGINAL_PRINTED.name())).thenReturn(responseCount);
        when(awbRepository.findByShipmentId(anyLong())).thenReturn(List.of(mockAWB));
        var res = awbDao.updatePrintTypeFromShipmentId(id, PrintType.ORIGINAL_PRINTED.name());

        assertEquals(responseCount, res);
    }


    @Test
    void testFindAllLinkedAwbsForHawb() {
        UUID inputGuid = UUID.randomUUID();
        Long shipmentId = 1L, consolId = 1L;

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(shipmentId);
        consoleShipmentMapping.setConsolidationId(consolId);

        // Mock
        when(awbRepository.findAwbByGuidByQuery(inputGuid)).thenReturn(mockAwb);
        when(consoleShipmentMappingDao.findByShipmentIdByQuery(any())).thenReturn(List.of(consoleShipmentMapping));

        // Test
        awbDao.findAllLinkedAwbs(inputGuid);

    }

    @Test
    void testFindAllLinkedAwbsForMawb() {
        UUID inputGuid = UUID.randomUUID();
        Long shipmentId = 1L, consolId = 1L;

        ConsoleShipmentMapping consoleShipmentMapping = new ConsoleShipmentMapping();
        consoleShipmentMapping.setShipmentId(shipmentId);
        consoleShipmentMapping.setConsolidationId(consolId);

        // Mock
        when(awbRepository.findAwbByGuidByQuery(inputGuid)).thenReturn(testMawb);
        when(consoleShipmentMappingDao.findByConsolidationIdByQuery(any())).thenReturn(List.of(consoleShipmentMapping));
        when(awbRepository.findByShipmentIdByQuery(shipmentId)).thenReturn(List.of(mockAwb));

        // Test
        awbDao.findAllLinkedAwbs(inputGuid);

    }

    @Test
    void findAllLinkedAwbsReturnsEmptyListByDefault() {
        UUID inputGuid = UUID.randomUUID();
        when(awbRepository.findAwbByGuidByQuery(inputGuid)).thenReturn(null);

        var res = awbDao.findAllLinkedAwbs(inputGuid);

        assertEquals(Collections.emptyList(), res);
    }

}
