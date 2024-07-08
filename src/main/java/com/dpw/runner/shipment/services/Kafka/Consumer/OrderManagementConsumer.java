package com.dpw.runner.shipment.services.Kafka.Consumer;

import com.dpw.runner.shipment.services.Kafka.Dto.OrderManageDto;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.repository.interfaces.IOrderManagementRepository;
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
public class OrderManagementConsumer {
    @Autowired
    private IOrderManagementRepository orderManagementRepository;
    @Autowired
    private ObjectMapper objectMapper;

    @KafkaListener(topics = {"#{'${order.management.event.kafka.queue}'}"}, groupId = "#{'${order.management.event.kafka.subs}'}")
    public void consume(String message)
    {
        try {
            log.info("{} | Order Management event message: {}", LoggerEvent.ORDER_MANAGEMENT_EVENT, message);
            OrderManageDto obj = objectMapper.readValue(message, OrderManageDto.class);
            if(!Objects.isNull(obj.getPayload()) && Objects.equals(obj.getPayload().getModuleGuid(), Constants.BOOKING) && !Objects.isNull(obj.getPayload().getModuleId()))
                orderManagementRepository.saveOrderIdAndOrderManagementNumber(UUID.fromString(obj.getPayload().getModuleGuid()), obj.getPayload().getOrderManagementId(), obj.getPayload().getOrderManagementNumber());
            log.info("Passed");
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.ORDER_MANAGEMENT_EVENT, message, ex.getLocalizedMessage());
        }
    }
}
