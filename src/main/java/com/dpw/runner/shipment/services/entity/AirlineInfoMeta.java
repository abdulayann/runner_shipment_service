package com.dpw.runner.shipment.services.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class AirlineInfoMeta {

    public String iata;
    public String airlinePrefix;
}
