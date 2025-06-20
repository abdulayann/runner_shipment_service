package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.impl.TrackingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.CustomerBookingDao;
import com.dpw.runner.shipment.services.dao.impl.ShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.trackingservice.UniversalTrackingPayload;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import com.dpw.runner.shipment.services.service.v1.impl.V1ServiceImpl;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

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
        assertDoesNotThrow(() -> pushToDownstreamService.pushContainerData(pushToDownstreamEventDto, "123"));
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
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationData(pushToDownstreamEventDto, "123"));
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
        assertDoesNotThrow(() -> pushToDownstreamService.pushConsolidationDataToTracking(pushToDownstreamEventDto, "123"));
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
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
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
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
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
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
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
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
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
        assertDoesNotThrow(() -> pushToDownstreamService.process(pushToDownstreamEventDto, "123"));
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
}
