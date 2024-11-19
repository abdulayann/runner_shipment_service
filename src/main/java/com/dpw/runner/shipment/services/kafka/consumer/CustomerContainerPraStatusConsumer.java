package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.CustomerContainerDto;
import com.dpw.runner.shipment.services.repository.interfaces.IConsolidationRepository;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
@Generated
public class CustomerContainerPraStatusConsumer {

    private final IConsolidationRepository consolidationRepository;

    private final IContainerRepository containerRepository;

    private final ObjectMapper objectMapper;

    public CustomerContainerPraStatusConsumer(IConsolidationRepository consolidationRepository,
                                              IContainerRepository containerRepository, ObjectMapper objectMapper) {
        this.consolidationRepository = consolidationRepository;
        this.containerRepository = containerRepository;
        this.objectMapper = objectMapper;
    }

    @KafkaListener(topics = {"#{'${custom.service.container.event.kafka.queue}'}"}, groupId = "#{'${custom.service.container.event.kafka.subs}'}")
    public void consume(String message)
    {
        try {
            log.info("{} | Custom event message: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, message);
            CustomerContainerDto obj = objectMapper.readValue(message, CustomerContainerDto.class);
            if(!Objects.isNull(obj) && !Objects.isNull(obj.getPayload()))
            {
                CustomerContainerDto.Container payload = obj.getPayload();
                Long consolidationId = consolidationRepository.findIdByGuid(payload.getConsolidationGuid());
                if(ObjectUtils.isNotEmpty(consolidationId)){
                    containerRepository.savePraStatus(payload.getPraStatus().toString(), payload.getContainerGuid(), consolidationId);
                    log.info("Passed");
                }
                else
                    log.info("Consolidation Data is not present for event: {} for message: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, message);
            }
            else
                log.info("Empty event from Customer Service for event: {} for message: {} ", LoggerEvent.CUSTOM_SERVICE_EVENT, message);
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.CUSTOM_SERVICE_EVENT, message, ex.getLocalizedMessage());
        }
    }
}
