package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.kafka.dto.AirMessagingEventDto;
import com.dpw.runner.shipment.services.kafka.dto.AirMessagingStatusDto;
import com.dpw.runner.shipment.services.repository.interfaces.IGenericQueryRepository;
import com.dpw.runner.shipment.services.utils.AwbUtility;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import javax.mail.MessagingException;
import java.io.IOException;
import java.util.Objects;


@Service
@Slf4j
@Generated
public class AirMessagingConsumer {
    @Autowired
    private IGenericQueryRepository genericQueryRepository;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private AwbUtility awbUtility;

    @KafkaListener(
            topics = {"#{'${air.messaging.event.kafka.queue}'}"},
            autoStartup = "#{'${air.messaging.event.kafka.consumer-auto-startup}'}",
            groupId = "#{'${air.messaging.event.kafka.subs}'}")
    public void consume(ConsumerRecord<String, String> message) {
        try {
            log.info("{} | Air event message: {}", LoggerEvent.KAFKA_AIR_MESSAGING_EVENT, message);
            message.headers().forEach(header -> {
                if (Objects.equals(header.key(), Constants.TYPE)) {
                    String value = new String(header.value());
                    if (Objects.equals(value.toLowerCase(), Constants.STATUS)) {
                        try {
                            AirMessagingStatusDto obj = objectMapper.readValue(message.value(), AirMessagingStatusDto.class);
                            awbUtility.createStatusUpdateForAirMessaging(obj);
                        } catch (RunnerException | MessagingException | IOException e) {
                            throw new RuntimeException(e);
                        }

                    } else if (Objects.equals(value.toLowerCase(), Constants.EVENT)) {
                        try {
                            AirMessagingEventDto obj = objectMapper.readValue(message.value(), AirMessagingEventDto.class);
                            awbUtility.createEventUpdateForAirMessaging(obj);
                        } catch (JsonProcessingException | RunnerException e) {
                            throw new RuntimeException(e);
                        }
                    }
                }
            });
            log.info("Passed");
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_AIR_MESSAGING_EVENT, message, ex.getLocalizedMessage());
        }
    }
}
