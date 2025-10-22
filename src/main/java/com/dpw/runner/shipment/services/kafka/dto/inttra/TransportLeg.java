package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class TransportLeg implements Serializable {
    private String sequenceNumber;
    private String stage;
    private String carrierScac;
    private String mode;
    private String means;
    private String vesselName;
    private String lloydsCode;
    private String vesselFlagCountry;
    private String serviceName;
    private String conveyanceNumber;
    private Location startLocation;
    private Location endLocation;
    private String meansDescription;
}