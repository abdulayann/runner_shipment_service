package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.BillingInvoiceDto;
import com.dpw.runner.shipment.services.kafka.dto.BillingInvoiceDto.InvoiceDto;
import com.dpw.runner.shipment.services.kafka.dto.BillingInvoiceDto.InvoiceDto.AccountReceivableDto.BillDto;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.List;

@Service
@Slf4j
@Generated
public class BillingCommonEventConsumer {

    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private IEventService eventService;
    @Autowired
    private RetryTemplate retryTemplate;

    // @KafkaListener(
    //         groupId = "#{'${bill.common-event.kafka.group-id}'}",
    //         topics = "#{'${bill.common-event.kafka.queue}'}",
    //         autoStartup = "#{'${bill.common-event.kafka.consumer-auto-startup}'}",
    //         containerFactory = "billingCommonEventKafkaListenerContainerFactory")
    public void consume(
            @Payload String message,
            @Header(KafkaHeaders.RECEIVED_TOPIC) String topic,
            @Header(KafkaHeaders.RECEIVED_PARTITION_ID) int partition,
            @Header(KafkaHeaders.OFFSET) long offset,
            @Header(KafkaHeaders.RECEIVED_TIMESTAMP) long receivedTimestamp,
            Acknowledgment acknowledgment) {

        logKafkaMessageInfo(message, topic, partition, offset, receivedTimestamp);

        try {
            retryTemplate.execute(retryContext -> {

                // Deserialize the incoming message into BillingInvoiceDto
                BillingInvoiceDto billingInvoiceDto = objectMapper.readValue(message, BillingInvoiceDto.class);

                // Additional validation to ensure payload is not null
                if (billingInvoiceDto != null
                        && billingInvoiceDto.getTenantCode() != null
                        && billingInvoiceDto.getPayload() != null
                        && billingInvoiceDto.getPayload().getAccountReceivable() != null
                        && billingInvoiceDto.getPayload().getAccountReceivable().getBills() != null
                        && billingInvoiceDto.getPayload().getAccountReceivable().getBillCharges() != null) {

                    InvoiceDto invoiceDto = billingInvoiceDto.getPayload();
                    List<BillDto> billDtoList = invoiceDto.getAccountReceivable().getBills();
                    // Remove any BillDto that does not match the required module type
                    billDtoList.removeIf(billDto -> !Constants.SHIPMENT.equalsIgnoreCase(billDto.getModuleTypeCode()));

                    eventService.processUpstreamBillingCommonEventMessage(billingInvoiceDto);

                } else {
                    log.error("{} | Invalid Billing Common event: Payload is null or invalid", LoggerEvent.KAFKA_BILLING_COMMON_EVENT);
                }
                return null;
            });
        } catch (Exception ex) {
            log.error("{} | Exception occurred while processing message: {} with exception: {}", LoggerEvent.KAFKA_BILLING_COMMON_EVENT, message,
                    ex.getMessage(), ex);
        } finally {
            acknowledgment.acknowledge();
        }
    }

    private void logKafkaMessageInfo(
            String kafkaMessage,
            String topic,
            int partition,
            long offset,
            long receivedTimestamp) {
        log.info("{} Received message from topic: {}", LoggerEvent.KAFKA_BILLING_COMMON_EVENT,topic);
        log.info("{} Partition: {}", LoggerEvent.KAFKA_BILLING_COMMON_EVENT,partition);
        log.info("{} Offset: {}", LoggerEvent.KAFKA_BILLING_COMMON_EVENT,offset);
        log.info("{} Received Timestamp: {}", LoggerEvent.KAFKA_BILLING_COMMON_EVENT,Instant.ofEpochMilli(receivedTimestamp));
        log.info("{} Message: {}", LoggerEvent.KAFKA_BILLING_COMMON_EVENT, kafkaMessage);
    }

}
