package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class Reference implements Serializable {
    private String referenceType;
    private String referenceValue;
}
