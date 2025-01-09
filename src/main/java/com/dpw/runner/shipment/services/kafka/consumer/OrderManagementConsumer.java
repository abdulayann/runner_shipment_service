package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.kafka.dto.BookingOrderUpdateDTO;
import com.dpw.runner.shipment.services.repository.interfaces.IOrderManagementRepository;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
@Generated
public class OrderManagementConsumer {
    @Autowired
    private IOrderManagementRepository orderManagementRepository;
    @Autowired
    private ObjectMapper objectMapper;

    //TODO:bookingseparation:Mayank - remove this once booking service go live
//    @KafkaListener(topics = {"#{'${order.management.event.kafka.queue}'}"}, groupId = "#{'${order.management.event.kafka.subs}'}")
    public void consume(String message)
    {
        try {
            log.info("{} | Order Management event message: {}", LoggerEvent.ORDER_MANAGEMENT_EVENT, message);
            BookingOrderUpdateDTO obj = objectMapper.readValue(message, BookingOrderUpdateDTO.class);
            if(!Objects.isNull(obj) && !Objects.isNull(obj.getData()))
            {
                BookingOrderUpdateDTO.BookingLinkDelinkOrder bookingLinkDelinkOrder = obj.getData();
                if(bookingLinkDelinkOrder.getBookingId() != null && bookingLinkDelinkOrder.getTenantId() != null)
                {
                    if(Objects.equals(obj.getEvent(), "LINK"))
                    {
                        orderManagementRepository.saveOrderIdAndOrderManagementNumber(bookingLinkDelinkOrder.getBookingId(), bookingLinkDelinkOrder.getTenantId(), bookingLinkDelinkOrder.getOrderId(), bookingLinkDelinkOrder.getOrderNumber());
                    }
                    else if(Objects.equals(obj.getEvent(), "DELINK"))
                    {
                        orderManagementRepository.saveOrderIdAndOrderManagementNumber(bookingLinkDelinkOrder.getBookingId(), bookingLinkDelinkOrder.getTenantId(), null, null);
                    }
                }
            }
            log.info("Passed");
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.ORDER_MANAGEMENT_EVENT, message, ex.getLocalizedMessage());
        }
    }
}
