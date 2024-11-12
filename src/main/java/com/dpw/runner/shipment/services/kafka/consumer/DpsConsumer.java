package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@Generated
public class DpsConsumer {
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private IDpsEventService dpsEventService;

    @KafkaListener(topics = {"#{'${dps.kafka.topic-name}'}"}, groupId = "#{'${dps.kafka.group-id}'}")
    public void consume(String message)
    {
        try {
            log.info("{} | event message: {}", LoggerEvent.KAFKA_DPS, message);
            DpsDto dpsDto = objectMapper.readValue(message, DpsDto.class);

            if (Objects.nonNull(dpsDto) && dpsDto.getEntityType().equalsIgnoreCase(DpsEntityType.SHIPMENT.name())) {
                dpsEventService.saveDpsEvent(dpsDto);
            }
            log.info("{} | Passed", LoggerEvent.KAFKA_DPS);
        } catch (Exception ex) {
            log.error("DPS ERROR -- Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_DPS, message, ex.getLocalizedMessage());
        }
    }


}
