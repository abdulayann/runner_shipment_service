package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class ShippingInstructionEventDto implements Serializable {
    private String siId;
    private String messageStatus;
    private String status;
    private String bookingNumber;
    private String freightForwarderReference;
    private String comments;
    private String commentType;
}
