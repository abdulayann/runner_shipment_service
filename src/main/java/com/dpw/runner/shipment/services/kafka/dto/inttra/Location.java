package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class Location implements Serializable {
    private String locationType;
    private String identifierType;
    private String identifierValue;
    private String city;
    private String subdivision;
    private String country;
    private String locationSystemLiteral;
    private List<LocationDate> locationDates;
    private String locationResolvedAlias;
}