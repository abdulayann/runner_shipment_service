package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class EquipmentSizeCode implements Serializable {
    private String sizeCodeType;
    private String sizeCodeValue;
    private String sizeCodeDescription;
}
