package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class Haulage implements Serializable {
    private String arrangement;
    private List<HaulagePoint> points;
}
