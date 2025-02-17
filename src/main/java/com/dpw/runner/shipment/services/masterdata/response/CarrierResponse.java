package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierResponse {
    @JsonProperty("ItemValue")
    public String itemValue;
    @JsonProperty("ItemDescription")
    public String itemDescription;
    @JsonProperty("Email")
    public String email;
    @JsonProperty("CarrierContactPerson")
    public String carrierContactPerson;
    @JsonProperty("ValuenDesc")
    public String valuenDesc;
    @JsonProperty("Cascade")
    public String cascade;
    @JsonProperty("Identifier1")
    public String identifier1;
    @JsonProperty("Identifier2")
    public String identifier2;
    @JsonProperty("Identifier3")
    public String identifier3;
    @JsonProperty("TransportCodeDescription")
    public String transportCodeDescription;
    @JsonProperty("IATACode")
    public String iATACode;
    @JsonProperty("AirlineCode")
    public String airlineCode;
    @JsonProperty("HasSeaPort")
    public Boolean hasSeaPort;
    @JsonProperty("HasAirPort")
    public Boolean hasAirPort;
    @JsonProperty("AirLinePrefixValue")
    public String airLinePrefixValue;
    @JsonProperty("Id")
    private int id;

}
