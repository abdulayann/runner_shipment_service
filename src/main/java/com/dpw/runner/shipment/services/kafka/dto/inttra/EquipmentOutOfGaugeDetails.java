package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
@Getter
@Setter
public class EquipmentOutOfGaugeDetails implements Serializable {
    private Dimension length;
    private Dimension width;
    private Dimension height;
}
