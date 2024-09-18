package com.dpw.runner.shipment.services.service.TO.response;


import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DescartesBaseResponse {

    @JsonProperty("Response")
    private DescartesResponse response;
}
