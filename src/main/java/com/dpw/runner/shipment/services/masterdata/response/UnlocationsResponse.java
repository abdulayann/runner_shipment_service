package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Objects;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnlocationsResponse implements Serializable {
    @JsonProperty("Id")
    private int id;
    @JsonProperty("Country")
    private String country;
    @JsonProperty("Name")
    private String name;
    @JsonProperty("PortName")
    private String portName;
    @JsonProperty("AirPortName")
    private String airPortName;
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
    @JsonProperty("CountryName")
    private String countryName;
    @JsonProperty("CountryThreeDigitCode")
    private String CountryThreeDigitCode;
    @JsonProperty("HasSeaPort")
    private Boolean hasSeaPort;
    @JsonProperty("HasAirport")
    private Boolean hasAirport;

    public String getCountry() {
        return (!Objects.isNull(country) && country.length() == 2 && !Objects.isNull(CountryThreeDigitCode) ? CountryThreeDigitCode : country);
    }
}
