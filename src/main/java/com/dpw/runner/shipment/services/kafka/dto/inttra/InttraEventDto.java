package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class InttraEventDto implements Serializable {
    private String entityType;
    private InttraCarrierBookingEventDto carrierBooking;
    private VgmEventDto vgm;
}
