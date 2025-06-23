package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.awb.AwbShipmentInfo;
import com.dpw.runner.shipment.services.dto.response.AwbResponse;
import com.dpw.runner.shipment.services.dto.response.TIKafkaEventResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.AwbShipConsoleDto;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.verifyNoInteractions;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class KafkaAsyncServiceTest {

    @InjectMocks
    private KafkaAsyncService kafkaAsyncService;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private KafkaProducer producer;

    @Test
    void testPushToKafkaAwb_WithShipmentId() {
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

        kafkaAsyncService.pushToKafkaAwb(awb, true);

        verify(producer, atLeast(1)).getKafkaResponse(any(), anyBoolean());
        verify(producer, times(1)).produceToKafka(any(), any(), any());
    }

    @Test
    void testPushToKafkaAwb_WithConsolidationId() {
        Awb awb = new Awb();
        awb.setConsolidationId(1L);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(new AwbResponse());
        when(producer.getKafkaResponse(any(), anyBoolean())).thenReturn(new KafkaResponse());

        kafkaAsyncService.pushToKafkaAwb(awb, false);

        verify(jsonHelper).convertValue(any(), eq(AwbResponse.class));
        verify(producer).getKafkaResponse(any(), eq(false));
        verify(producer).produceToKafka(any(), any(), any());
    }

    @Test
    void testPushToKafkaAwb_WithInvalidShipmentId() {
        Awb awb = new Awb();
        awb.setShipmentId(1L);
        when(shipmentDao.findById(any())).thenReturn(Optional.empty());

        kafkaAsyncService.pushToKafkaAwb(awb, true);

        verifyNoInteractions(producer);
    }

    @Test
    void testPushToKafkaAwb_WithInvalidConsolidationId() {
        Awb awb = new Awb();
        awb.setConsolidationId(1L);
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.empty());

        kafkaAsyncService.pushToKafkaAwb(awb, false);

        verifyNoInteractions(producer);
    }

    @Test
    void testPushToKafkaAwb_WithNullTenantId() {
        Awb awb = new Awb();
        awb.setTenantId(null);
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenReturn(new AwbResponse());

        kafkaAsyncService.pushToKafkaAwb(awb, false);

        verify(jsonHelper).convertValue(any(), eq(AwbResponse.class));
    }

    @Test
    void testPushToKafkaAwb_WithException() {
        Awb awb = new Awb();
        when(jsonHelper.convertValue(any(), eq(AwbResponse.class))).thenThrow(new RuntimeException());

        kafkaAsyncService.pushToKafkaAwb(awb, false);

        verifyNoInteractions(producer);
    }

    @Test
    void pushToKafkaTI() {
        List<IRunnerResponse> pickupDeliveryDetails = List.of(mock(IRunnerResponse.class));
        boolean isCreate = true;
        Long shipmentId = 123L;

        TIKafkaEventResponse tiKafkaEventResponse = new TIKafkaEventResponse();
        tiKafkaEventResponse.setShipmentId(shipmentId);
        tiKafkaEventResponse.setPickupDeliveryDetails(pickupDeliveryDetails);

        KafkaResponse kafkaResponse = new KafkaResponse();
        String kafkaMessage = "{\"mocked\": \"message\"}";

        when(producer.getKafkaResponse(any(), eq(isCreate))).thenReturn(kafkaResponse);
        when(jsonHelper.convertToJson(kafkaResponse)).thenReturn(kafkaMessage);

        kafkaAsyncService.pushToKafkaTI(pickupDeliveryDetails, isCreate, shipmentId);

        verify(producer).getKafkaResponse(any(), eq(true));
    }

    @Test
    void pushToKafkaTI_shouldHandleExceptionGracefully() {
        List<IRunnerResponse> pickupDeliveryDetails = List.of(mock(IRunnerResponse.class));
        boolean isCreate = false;
        Long shipmentId = 456L;

        when(producer.getKafkaResponse(any(), eq(isCreate))).thenThrow(new RuntimeException("Kafka Error"));

        assertDoesNotThrow(() -> kafkaAsyncService.pushToKafkaTI(pickupDeliveryDetails, isCreate, shipmentId));
        verify(producer, never()).produceToKafka(anyString(), anyString(), anyString());
    }
}
