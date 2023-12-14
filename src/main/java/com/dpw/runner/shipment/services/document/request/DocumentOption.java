package com.dpw.runner.shipment.services.document.request;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class DocumentOption implements Serializable {
    private String type;
    private Boolean upload;
}
