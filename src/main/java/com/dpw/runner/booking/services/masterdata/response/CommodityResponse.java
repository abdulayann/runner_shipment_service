package com.dpw.runner.booking.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class CommodityResponse {
    @JsonProperty("Id")
    private int id;
    @JsonProperty("Code")
    private String code;
    @JsonProperty("Description")
    private String description;
    @JsonProperty("CommodityDescriptionWithHSCode")
    private String commodityDescriptionWithHSCode;

}
