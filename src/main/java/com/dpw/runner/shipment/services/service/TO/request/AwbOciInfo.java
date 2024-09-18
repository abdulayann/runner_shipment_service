package com.dpw.runner.shipment.services.service.TO.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbOciInfo {
    public String informationIdentifier;
    public String tradeIdentificationCode;
    public String tradeIdentificationComment;
}
