package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.DocumentDto;
import com.dpw.runner.shipment.services.utils.BookingIntegrationsUtility;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.text.StringEscapeUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Objects;


@Service
@Slf4j
@Generated
public class DocumentMasterConsumer {

    private ObjectMapper objectMapper;
    private BookingIntegrationsUtility bookingIntegrationsUtility;

    @Autowired
    DocumentMasterConsumer(ObjectMapper objectMapper, BookingIntegrationsUtility bookingIntegrationsUtility) {
        this.objectMapper = objectMapper;
        this.bookingIntegrationsUtility = bookingIntegrationsUtility;
    }

//    @KafkaListener(topics = {"#{'${document.master.kafka.event}'}"}, groupId = "#{'${document.master.kafka.subs}'}")
    public void consume(String message)
    {
        try {
            log.info("{} | event message: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, message);
            DocumentDto object = objectMapper.readValue(StringEscapeUtils.unescapeJson(message), DocumentDto.class);

            if (Objects.nonNull(object) && Objects.nonNull(object.getData()) && Objects.nonNull(object.getAction())) {
                bookingIntegrationsUtility.documentUploadEvent(object);
            }
            log.info("{} | Passed", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT);
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_DOCUMENT_MASTER_EVENT, message, ex.getLocalizedMessage());
        }
    }
}