package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class UnlocationsResponse {
    @JsonProperty("Id")
    private int id;
    @JsonProperty("Country")
    private String country;
    @JsonProperty("Name")
    private String name;
    @JsonProperty("PortName")
    private String portName;
    @JsonProperty("LocCode")
    private String locCode;
    @JsonProperty("NameWoDiacritics")
    private String nameWoDiacritics;
    @JsonProperty("IATACode")
    private String iataCode;
    @JsonProperty("LocationsReferenceGUID")
    private String locationsReferenceGUID;
    @JsonProperty("lookupDescAir")
    private String lookupDescAir;
    @JsonProperty("lookupDescSea")
    private String lookupDescSea;
}
