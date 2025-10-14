package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.TrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.CustomerBookingDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.v3.ShippingInstructionUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CUSTOMER_BOOKING_TO_OMS_SYNC;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CUSTOMER_BOOKING_TO_PLATFORM_SYNC;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class PushToDownstreamServiceTest {

    @InjectMocks
    private PushToDownstreamService pushToDownstreamService;

    @Mock
    private ContainerV3Service containerV3Service;

    @Mock
    private ConsolidationV3Service consolidationV3Service;

    @Mock
    private KafkaProducer producer;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ShipmentDao shipmentDao;

    @Mock
    private TrackingServiceAdapter trackingServiceAdapter;

    @Mock
    private DependentServiceHelper dependentServiceHelper;

    @Mock
    private V1ServiceImpl v1Service;

    @Mock
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Mock
    private CustomerBookingDao customerBookingDao;

    @Mock
    private IPickupDeliveryDetailsService pickupDeliveryDetailsService;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private ShippingInstructionUtil shippingInstructionUtil;

    @Mock
    private LogsHistoryService logsHistoryService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static Containers testContainer;
    private static ConsolidationDetails testConsolidation;
    private static ShipmentDetails testShipment;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
        testContainer = jsonTestUtility.getTestContainer();
        testConsolidation = jsonTestUtility.getTestConsolidation();
        testShipment = jsonTestUtility.getTestShipment();
    }

    @Test
    void testPushContainerData() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        assertThrows(ValidationException.class, () -> pushToDownstreamService.pushContainerData(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushContainerData1() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        when(containerV3Service.findByIdIn(List.of(123L))).thenReturn(List.of(testContainer));
        assertDoesNotThrow(() -> pushToDownstreamService.pushContainerData(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationData() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        assertThrows(ValidationException.class, () -> pushToDownstreamService.pushConsolidationData(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationData1() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        when(consolidationV3Service.findById(123L)).thenReturn(Optional.of(testConsolidation));
        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationData(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationData2() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        testConsolidation.setShipmentsList(Set.of(testShipment));
        testConsolidation.setTenantId(123);
        when(consolidationV3Service.findById(123L)).thenReturn(Optional.of(testConsolidation));
        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationData(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationData3() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        testConsolidation.setShipmentsList(Set.of(testShipment));
        when(consolidationV3Service.findById(123L)).thenReturn(Optional.of(testConsolidation));
        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(testShipment));
        when(trackingServiceAdapter.mapEventDetailsForTracking(any(), any(), any(), any())).thenReturn(new UniversalTrackingPayload.UniversalEventsPayload());
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationData(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationData4() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        testShipment.setId(null);
        testConsolidation.setShipmentsList(Set.of(testShipment));
        testConsolidation.setTenantId(123);
        when(consolidationV3Service.findById(123L)).thenReturn(Optional.of(testConsolidation));
        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationData(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationDataToTracking() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        assertThrows(ValidationException.class, () -> pushToDownstreamService.pushConsolidationDataToTracking(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationDataToTracking1() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        when(consolidationV3Service.findById(any())).thenReturn(Optional.of(testConsolidation));
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationDataToTracking(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationDataToTracking2() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        when(consolidationV3Service.findById(any())).thenReturn(Optional.of(testConsolidation));
        when(trackingServiceAdapter.checkIfConsolContainersExist(any())).thenReturn(true);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(testShipment));
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationDataToTracking(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testPushConsolidationDataToTracking3() {
        testConsolidation.setTenantId(1);
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        when(consolidationV3Service.findById(any())).thenReturn(Optional.of(testConsolidation));
        when(trackingServiceAdapter.checkIfAwbExists(any())).thenReturn(true);
        when(shipmentDao.findShipmentsByIds(any())).thenReturn(List.of(testShipment));
        when(trackingServiceAdapter.mapConsoleDataToTrackingServiceData(any(), any())).thenReturn(new UniversalTrackingPayload());
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationDataToTracking(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.SHIPMENT);
        assertThrows(ValidationException.class, () -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess1() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.SHIPMENT);
        when(shipmentDao.findShipmentByIdWithQuery(any())).thenReturn(Optional.of(testShipment));
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess2() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.CONTAINER);
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess3() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.CONSOLIDATION);
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess4() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.CUSTOMER_BOOKING);
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess5() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(List.of(new PushToDownstreamEventDto.Triggers(123L, Constants.SHIPMENT, "")));
        pushToDownstreamEventDto.setParentEntityName(Constants.SHIPMENT);
        assertThrows(ValidationException.class, () -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess6() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.CONTAINER);
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setSourceInfo(Constants.CONTAINER_AFTER_SAVE);
        pushToDownstreamEventDto.setMeta(meta);
        assertThrows(ValidationException.class, () -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess7() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.CONSOLIDATION);
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setSourceInfo(Constants.CONSOLIDATION_AFTER_SAVE);
        pushToDownstreamEventDto.setMeta(meta);
        assertThrows(ValidationException.class, () -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess8() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.CONSOLIDATION);
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setSourceInfo(Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING);
        pushToDownstreamEventDto.setMeta(meta);
        assertThrows(ValidationException.class, () -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }
    @Test
    void testProcess9() {
        PushToDownstreamEventDto pushToDownstreamEventDto = new PushToDownstreamEventDto();
        pushToDownstreamEventDto.setParentEntityId(123L);
        pushToDownstreamEventDto.setTriggers(new ArrayList<>());
        pushToDownstreamEventDto.setParentEntityName(Constants.TRANSPORT_INSTRUCTION);
        PickupDeliveryDetails pickupDeliveryDetails = new PickupDeliveryDetails();
        pickupDeliveryDetails.setId(1l);
        pickupDeliveryDetails.setShipmentId(1l);
        doNothing().when(pickupDeliveryDetailsService).processDownStreamConsumerData(anyList(), any(),any(),any());
        when(pickupDeliveryDetailsService.findById(anyLong())).thenReturn(Optional.of(pickupDeliveryDetails));
        when(pickupDeliveryDetailsService.findByShipmentId(anyLong())).thenReturn(List.of(pickupDeliveryDetails));
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void syncContainerWithCommonContainer_success_whenConsolidationExists() {
        Long consolidationId = 123L;
        Integer tenantId = 1;
        String transactionId = "TXN-123";

        PushToDownstreamEventDto eventDto = new PushToDownstreamEventDto();
        eventDto.setParentEntityId(consolidationId);
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setTenantId(tenantId);
        eventDto.setMeta(meta);

        testConsolidation.setId(consolidationId);

        when(consolidationV3Service.findById(consolidationId))
                .thenReturn(Optional.of(testConsolidation));
        doNothing().when(shippingInstructionUtil).syncCommonContainersByConsolId(consolidationId);
        assertDoesNotThrow(() ->
                pushToDownstreamService.syncContainerWithCommonContainer(eventDto, transactionId)
        );
        verify(consolidationV3Service).findById(consolidationId);
        verify(shippingInstructionUtil).syncCommonContainersByConsolId(consolidationId);
    }

    @Test
    void syncContainerWithCommonContainer_shouldThrow_whenConsolidationNotFound() {
        Long consolidationId = 999L;
        Integer tenantId = 1;
        String transactionId = "TXN-456";

        PushToDownstreamEventDto eventDto = new PushToDownstreamEventDto();
        eventDto.setParentEntityId(consolidationId);
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setTenantId(tenantId);
        eventDto.setMeta(meta);

        when(consolidationV3Service.findById(consolidationId))
                .thenReturn(Optional.empty());

        // Act & Assert
        ValidationException exception = assertThrows(ValidationException.class, () ->
                pushToDownstreamService.syncContainerWithCommonContainer(eventDto, transactionId)
        );

        assertThat(exception.getMessage())
                .contains("[InternalKafkaConsume] Consolidation: " + consolidationId)
                .contains(transactionId)
                .contains("not found");

        verify(consolidationV3Service).findById(consolidationId);
        verify(shippingInstructionUtil, Mockito.never()).syncCommonContainersByConsolId(any());
    }

    @Test
    void syncContainerWithCommonContainer_handlesNullMeta() {
        // Arrange
        Long consolidationId = 100L;
        String transactionId = "TXN-NULL";

        PushToDownstreamEventDto eventDto = new PushToDownstreamEventDto();
        eventDto.setParentEntityId(consolidationId);
        eventDto.setMeta(null);

        // Act & Assert
        assertThrows(NullPointerException.class, () ->
                pushToDownstreamService.syncContainerWithCommonContainer(eventDto, transactionId)
        );

        verifyNoInteractions(consolidationV3Service, shippingInstructionUtil);
    }

    @Test
    void pushConsolidationDataToService_callsPushConsolidationDataToTracking_whenSourceIsAfterSaveToTracking() {
        Long consolidationId = 123L;
        Integer tenantId = 1;
        String transactionId = "TXN-TRACKING";

        PushToDownstreamEventDto eventDto = new PushToDownstreamEventDto();
        eventDto.setParentEntityId(consolidationId);
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setTenantId(tenantId);
        meta.setSourceInfo(Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING);
        eventDto.setMeta(meta);

        testConsolidation.setId(consolidationId);

        when(consolidationV3Service.findById(consolidationId))
                .thenReturn(Optional.of(testConsolidation));
        assertDoesNotThrow(() ->
                pushToDownstreamService.pushConsolidationDataToService(eventDto, transactionId)
        );
        verify(consolidationV3Service).findById(consolidationId);
    }

    @Test
    void pushConsolidationDataToService_doesNothing_whenSourceInfoDoesNotMatch() {
        // Arrange
        Long consolidationId = 123L;
        Integer tenantId = 1;
        String transactionId = "TXN-NO-MATCH";

        PushToDownstreamEventDto eventDto = new PushToDownstreamEventDto();
        eventDto.setParentEntityId(consolidationId);
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setTenantId(tenantId);
        meta.setSourceInfo("UNKNOWN_SOURCE");
        eventDto.setMeta(meta);
        assertDoesNotThrow(() ->
                pushToDownstreamService.pushConsolidationDataToService(eventDto, transactionId)
        );

        verifyNoInteractions(consolidationV3Service, producer, shippingInstructionUtil);
    }

    @Test
    void pushConsolidationDataToService_asyncCompletesEvenIfMainFails() {
        // Arrange
        Long consolidationId = 666L;
        Integer tenantId = 1;
        String transactionId = "TXN-MAIN-FAIL";

        PushToDownstreamEventDto eventDto = new PushToDownstreamEventDto();
        eventDto.setParentEntityId(consolidationId);
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setTenantId(tenantId);
        meta.setSourceInfo(Constants.CONSOLIDATION_AFTER_SAVE);
        eventDto.setMeta(meta);

        when(consolidationV3Service.findById(consolidationId))
                .thenReturn(Optional.empty()); // This will cause pushConsolidationData to throw

        // Act
        assertThrows(ValidationException.class, () ->
                pushToDownstreamService.pushConsolidationDataToService(eventDto, transactionId)
        );

        // Assert
        verify(consolidationV3Service).findById(consolidationId);
    }

    @Test
    void testProcess_CUSTOMER_BOOKING_TO_PLATFORM_SYNC() {
        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(1L)
                .parentEntityName(Constants.CUSTOMER_BOOKING)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .tenantId(100)
                        .sourceInfo(CUSTOMER_BOOKING_TO_PLATFORM_SYNC)
                        .isCreate(true).build())
                .build();

        assertThrows(ValidationException.class, () -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess_CUSTOMER_BOOKING_TO_OMS_SYNC() {
        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(1L)
                .parentEntityName(Constants.CUSTOMER_BOOKING)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .tenantId(100)
                        .sourceInfo(CUSTOMER_BOOKING_TO_OMS_SYNC)
                        .isCreate(true).build())
                .build();

        assertThrows(ValidationException.class, () -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
    }

    @Test
    void testProcess_CUSTOMER_BOOKING_TO_OMS_SYNC_shouldProduceToKafka() {
        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(1L)
                .parentEntityName(Constants.CUSTOMER_BOOKING)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .tenantId(100)
                        .sourceInfo(CUSTOMER_BOOKING_TO_OMS_SYNC)
                        .isCreate(true).build())
                .build();

        CustomerBooking fakeCustomerBooking = CustomerBooking.builder().build();
        fakeCustomerBooking.setOrderManagementId("ORD_NUM_00001");
        fakeCustomerBooking.setOrderManagementNumber("ORD_MGMT_00001");
        fakeCustomerBooking.setBookingStatus(BookingStatus.CANCELLED);
        fakeCustomerBooking.setBookingNumber("BOOKING_NUM_00001");
        fakeCustomerBooking.setGuid(UUID.randomUUID());
        fakeCustomerBooking.setTenantId(100);

        when(customerBookingDao.findById(anyLong())).thenReturn(Optional.of(fakeCustomerBooking));

        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
        verify(producer).produceToKafka(any(), any(), any());
    }

    private PushToDownstreamEventDto createDtoWithSource(String source) {
        PushToDownstreamEventDto dto = new PushToDownstreamEventDto();
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setSourceInfo(source);
        dto.setMeta(meta);
        dto.setParentEntityId(123L);
        return dto;
    }

    @Test
    void whenSourceIsAfterSave_shouldCallPushAndAsyncSync() throws Exception {
        PushToDownstreamEventDto message = createDtoWithSource(Constants.CONSOLIDATION_AFTER_SAVE);
        String txId = "tx-1";

        // spy the real instance that has @InjectMocks dependencies initialized
        PushToDownstreamService spyService = Mockito.spy(pushToDownstreamService);

        // stub the synchronous push to do nothing (so it won't throw)
        doNothing().when(spyService).pushConsolidationData(eq(message), eq(txId));

        // for the async sync method, count down latch when called
        CountDownLatch latch = new CountDownLatch(1);
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(spyService).syncContainerWithCommonContainer(eq(message), eq(txId));

        spyService.pushConsolidationDataToService(message, txId);
        boolean asyncRan = latch.await(2, TimeUnit.SECONDS);

        assertTrue(asyncRan, "Expected syncContainerWithCommonContainer to be invoked asynchronously");
        verify(spyService, times(1)).pushConsolidationData(eq(message), eq(txId));
        verify(spyService, times(1)).syncContainerWithCommonContainer(eq(message), eq(txId));
    }

    @Test
    void whenSourceIsAfterSaveToTracking_shouldCallPushToTrackingOnly() {
        PushToDownstreamEventDto message = createDtoWithSource(Constants.CONSOLIDATION_AFTER_SAVE_TO_TRACKING);
        String txId = "tx-2";

        PushToDownstreamService spyService = Mockito.spy(pushToDownstreamService);
        doNothing().when(spyService).pushConsolidationDataToTracking(eq(message), eq(txId));
        spyService.pushConsolidationDataToService(message, txId);
        verify(spyService, times(1)).pushConsolidationDataToTracking(eq(message), eq(txId));
        verify(spyService, never()).pushConsolidationData(any(), anyString());
        verify(spyService, never()).syncContainerWithCommonContainer(any(), anyString());
    }

    @Test
    void pushCustomerBookingDataToPlatform_shouldThrow_whenCustomerBookingNotFound() {
        Long bookingId = 1L;
        String txId = "TXN-CB-1";
        PushToDownstreamEventDto dto = new PushToDownstreamEventDto();
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setTenantId(100);
        dto.setMeta(meta);
        dto.setParentEntityId(bookingId);

        when(customerBookingDao.findById(bookingId)).thenReturn(Optional.empty());

        ValidationException ex = assertThrows(ValidationException.class,
                () -> pushToDownstreamService.pushCustomerBookingDataToPlatform(dto, txId));
        assertTrue(ex.getMessage().contains("Customer Booking: " + bookingId));
        verify(customerBookingDao).findById(bookingId);
        verifyNoInteractions(bookingIntegrationsUtility);
    }

    @Test
    void pushCustomerBookingDataToPlatform_shouldCallCreateBookingInPlatform_whenCustomerBookingExists() {
        Long bookingId = 2L;
        String txId = "TXN-CB-2";
        PushToDownstreamEventDto dto = new PushToDownstreamEventDto();
        PushToDownstreamEventDto.Meta meta = new PushToDownstreamEventDto.Meta();
        meta.setTenantId(200);
        dto.setMeta(meta);
        dto.setParentEntityId(bookingId);

        CustomerBooking fakeBooking = new CustomerBooking();
        fakeBooking.setTenantId(200);
        fakeBooking.setId(bookingId);

        when(customerBookingDao.findById(bookingId)).thenReturn(Optional.of(fakeBooking));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        doNothing().when(bookingIntegrationsUtility).createBookingInPlatform(any());

        assertDoesNotThrow(() -> pushToDownstreamService.pushCustomerBookingDataToPlatform(dto, txId));

        verify(customerBookingDao).findById(bookingId);
        verify(bookingIntegrationsUtility, times(1)).createBookingInPlatform(fakeBooking);
    }

    @Test
    void createLogHistoryForShipment_shouldCallLogsHistoryService() {
        String payload = "{ \"shipment\": \"data\" }";
        Long shipmentId = 10L;
        UUID guid = UUID.randomUUID();

        doNothing().when(logsHistoryService)
                .createLogHistory(any());
        assertDoesNotThrow(() ->
                pushToDownstreamService.createLogHistoryForShipment(payload, shipmentId, guid));

        verify(logsHistoryService, times(1)).createLogHistory(argThat(req ->
                req.getEntityId() == shipmentId.longValue()
                        && Objects.equals(req.getEntityGuid(), guid)
                        && Objects.equals(req.getEntityType(), Constants.SHIPMENT)
                        && Objects.equals(req.getEntityPayload(), payload)
        ));
    }

    @Test
    void createLogHistoryForShipment_shouldHandleExceptionGracefully() {
        String payload = "{ \"shipment\": \"data\" }";
        Long shipmentId = 20L;
        UUID guid = UUID.randomUUID();

        doThrow(new RuntimeException("DB connection failed"))
                .when(logsHistoryService)
                .createLogHistory(any());

        assertDoesNotThrow(() ->
                pushToDownstreamService.createLogHistoryForShipment(payload, shipmentId, guid));

        verify(logsHistoryService).createLogHistory(any());
    }
}
