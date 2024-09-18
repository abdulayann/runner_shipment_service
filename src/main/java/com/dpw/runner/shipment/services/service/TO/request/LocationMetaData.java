package com.dpw.runner.shipment.services.service.TO.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
@Data
public class LocationMetaData {

    public String name;
    public String locCode;
    public String countyCode;
    public String iataCode;
}
