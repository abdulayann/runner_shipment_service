package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class DateInfo implements Serializable {
    private String dateValue;
    private String dateFormat;
}
