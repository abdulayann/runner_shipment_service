package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.kafka.dto.BillDto;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.repository.interfaces.IGenericQueryRepository;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.UUID;


@Service
@Slf4j
@Generated
public class BillConsumer {
    @Autowired
    private IGenericQueryRepository genericQueryRepository;
    @Autowired
    private ObjectMapper objectMapper;

    @KafkaListener(
            topics = {"#{'${bill.event.kafka.queue}'}"},
            autoStartup = "#{'${bill.event.kafka.consumer-auto-startup}'}",
            groupId = "#{'${bill.event.kafka.subs}'}")
    public void consume(String message)
    {
        try {
            log.info("{} | Bill event message: {}", LoggerEvent.KAFKA_BILL_EVENT, message);
            BillDto obj = objectMapper.readValue(message, BillDto.class);
            if(!Objects.isNull(obj.getPayload()) && Objects.equals(obj.getPayload().getModuleTypeCode(), Constants.SHIPMENT) && !Objects.isNull(obj.getPayload().getModuleId())) {
                genericQueryRepository.saveJobStatusByGuid(UUID.fromString(obj.getPayload().getModuleId()), obj.getPayload().getJobStatus(), obj.getPayload().getFileStatus().name());
            }
            log.info("Passed");
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_BILL_EVENT, message, ex.getLocalizedMessage());
        }
    }
}
