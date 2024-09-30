package com.dpw.runner.booking.services.masterdata.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierMasterData implements Serializable {
    @JsonProperty("Id")
    private int id;
    @JsonProperty("ItemValue")
    private String itemValue;
    @JsonProperty("ItemDescription")
    private String itemDescription;
    @JsonProperty("Email")
    private String email;
    @JsonProperty("CarrierContactPerson")
    private String carrierContactPerson;
    @JsonProperty("ValuenDesc")
    private String valuenDesc;
    @JsonProperty("Cascade")
    private String cascade;
    @JsonProperty("Identifier1")
    private String identifier1;
    @JsonProperty("Identifier2")
    private String identifier2;
    @JsonProperty("Identifier3")
    private String identifier3;
    @JsonProperty("TransportCodeDescription")
    private String transportCodeDescription;
    @JsonProperty("IATACode")
    private String iataCode;
    @JsonProperty("AirlineCode")
    private String airlineCode;
    @JsonProperty("HasSeaPort")
    private boolean hasSeaPort;
    @JsonProperty("HasAirPort")
    private boolean hasAirPort;
    @JsonProperty("AirLinePrefixValue")
    private String airLinePrefixValue;
    @JsonProperty("CarriersLogo")
    private String carriersLogo;
    @JsonProperty("ValuenDescAir")
    private String valuenDescAir;
    @JsonProperty("TaxConfigId")
    private int taxConfigId;
    @JsonProperty("DefaultOrgId")
    private int defaultOrgId;
    @JsonProperty("TypeOfPayLoad")
    private String typeOfPayLoad;

    @JsonProperty("CarrierSettings")
    private String carrierSettings;
}
