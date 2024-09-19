package com.dpw.runner.shipment.services.Kafka.Consumer;

import com.dpw.runner.shipment.services.Kafka.Dto.DocumentDto;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;


@Service
@Slf4j
@Generated
public class DocumentMasterConsumer {
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Value("${document.master.integration}")
    private boolean documentMasterEnabled;

    @KafkaListener(topics = {"#{'${document.master.kafka.event}'}"}, groupId = "#{'${document.master.kafka.subs}'}")
    public void consume(String message)
    {
        try {
            log.info("{} | event message: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, message);
            DocumentDto object = objectMapper.readValue(message, DocumentDto.class);

            if (Objects.nonNull(object) && Objects.nonNull(object.getData()) && Objects.nonNull(object.getAction()) && Boolean.TRUE.equals(documentMasterEnabled)) {
                bookingIntegrationsUtility.documentUploadEvent(object);
            }
            log.info("{} | Passed", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT);
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, message, ex.getLocalizedMessage());
        }
    }
}