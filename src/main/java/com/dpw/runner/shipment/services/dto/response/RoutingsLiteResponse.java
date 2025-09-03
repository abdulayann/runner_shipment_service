package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class RoutingsLiteResponse implements Serializable {
    private String originPortLocCode;
    private String destinationPortLocCode;
    private String pol;
    private String pod;
    private RoutingCarriage carriage;
}
