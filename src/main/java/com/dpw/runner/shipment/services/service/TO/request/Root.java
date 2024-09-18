package com.dpw.runner.shipment.services.service.TO.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class Root implements Serializable {
    public String event;
    public Object data;
}
