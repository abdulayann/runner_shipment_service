package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.inttra.InttraEventDto;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.service.interfaces.IVerifiedGrossMassService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.text.StringEscapeUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.stereotype.Service;

import java.time.Instant;


@Service
@Slf4j
@Generated
public class InttraConsumer {

    private final ObjectMapper objectMapper;
    private final IV1Service v1Service;
    private final ICarrierBookingService carrierBookingService;
    private final IVerifiedGrossMassService verifiedGrossMassService;

    @Autowired
    InttraConsumer(ObjectMapper objectMapper, IV1Service v1Service, ICarrierBookingService carrierBookingService, IVerifiedGrossMassService verifiedGrossMassService) {
        this.objectMapper = objectMapper;
        this.v1Service = v1Service;
        this.carrierBookingService = carrierBookingService;
        this.verifiedGrossMassService = verifiedGrossMassService;
    }

    @KafkaListener(
            topics = {"#{'${bridge.inttra.messages.kafka}'}"},
            groupId = "#{'${bridge.inttra.messages.kafka.consumer-group}'}",
            autoStartup = "#{'${bridge.inttra.consumer-auto-startup}'}",
            containerFactory = "bridgeServiceContainerFactory")
    public void consume(@Payload String message,
                        @Header(KafkaHeaders.RECEIVED_TOPIC) String topic,
                        @Header(KafkaHeaders.RECEIVED_PARTITION_ID) int partition,
                        @Header(KafkaHeaders.OFFSET) long offset,
                        @Header(KafkaHeaders.RECEIVED_TIMESTAMP) long receivedTimestamp,
                        @Header(KafkaHeaders.RECEIVED_MESSAGE_KEY) String transactionId,
                        Acknowledgment acknowledgment) {

        logKafkaMessageInfo(message, topic, partition, offset, receivedTimestamp, transactionId);
        try {
            InttraEventDto inttraEventDto = objectMapper.readValue(StringEscapeUtils.unescapeJson(message), InttraEventDto.class);
            if (CarrierBookingConstants.CARRIER_BOOKING.equals(inttraEventDto.getEntityType())) {
                carrierBookingService.updateCarrierDataToBooking(inttraEventDto.getCarrierBooking());
            } else if (VerifiedGrossMassConstants.VERIFIED_GROSS_MASS.equals(inttraEventDto.getEntityType())) {
                verifiedGrossMassService.updateVgmStatus(inttraEventDto.getVgm());
            }
            log.info("{} entityType: {}| Passed", inttraEventDto.getEntityType(), LoggerEvent.BRIDGE_SERVICE_INTTRA_INTEGRATION);
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.BRIDGE_SERVICE_INTTRA_INTEGRATION, message, ex.getLocalizedMessage());
        } finally {
            v1Service.clearAuthContext();
            acknowledgment.acknowledge();
        }
    }

    private void logKafkaMessageInfo(
            String kafkaMessage,
            String topic,
            int partition,
            long offset,
            long receivedTimestamp,
            String transactionId) {
        log.info("{} Received message from topic: {}", LoggerEvent.BRIDGE_SERVICE_INTTRA_INTEGRATION, topic);
        log.info("{} Partition: {}", LoggerEvent.BRIDGE_SERVICE_INTTRA_INTEGRATION, partition);
        log.info("{} Offset: {}", LoggerEvent.BRIDGE_SERVICE_INTTRA_INTEGRATION, offset);
        log.info("{} Received Timestamp: {}", LoggerEvent.BRIDGE_SERVICE_INTTRA_INTEGRATION, Instant.ofEpochMilli(receivedTimestamp));
        log.info("{} Message: {}", LoggerEvent.BRIDGE_SERVICE_INTTRA_INTEGRATION, kafkaMessage);
        log.info("{} Transaction Id: {}", LoggerEvent.BRIDGE_SERVICE_INTTRA_INTEGRATION, transactionId);
    }
}