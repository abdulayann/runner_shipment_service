package com.dpw.runner.shipment.services.Kafka.Consumer;

import com.dpw.runner.shipment.services.Kafka.Dto.BillDto;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;


@Service
@Slf4j
public class BillConsumer {
    @Autowired
    private IShipmentRepository shipmentRepository;
    @Autowired
    private ObjectMapper objectMapper;

    @KafkaListener(topics = {"#{'${bill.event.kafka.queue}'}"}, groupId = "#{'${bill.event.kafka.subs}'}")
    public void consume(String message)
    {
        try {
            log.info("Bill event message :" + message);
            BillDto obj = objectMapper.readValue(message, BillDto.class);
            if(Objects.equals(obj.getModuleTypeCode(), "SHIPMENT") && obj.getModuleId() != null)
                shipmentRepository.saveJobStatusByGuid(obj.getModuleId(), obj.getJobStatus());
            log.info("Passed");
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_BILL_EVENT, message, ex.getLocalizedMessage());
        }
    }
}
