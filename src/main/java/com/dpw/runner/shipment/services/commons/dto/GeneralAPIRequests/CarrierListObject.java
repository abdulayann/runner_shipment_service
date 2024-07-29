package com.dpw.runner.shipment.services.commons.dto.GeneralAPIRequests;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CarrierListObject {
    private String type;
    private Object listObject;
    private Boolean isList;
}
