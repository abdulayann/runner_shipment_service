package com.dpw.runner.shipment.services.dto.response;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class RoutingLegWarning implements Serializable {
    private String eta;
    private String etd;
}
