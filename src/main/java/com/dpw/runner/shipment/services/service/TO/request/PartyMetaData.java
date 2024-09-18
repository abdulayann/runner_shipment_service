package com.dpw.runner.shipment.services.service.TO.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
@Data
public class PartyMetaData {
    public String city;
    public String country;
    public String currency;
    public String number;
    public String expiry;
}
