package com.dpw.runner.shipment.services.document.request;

import com.dpw.runner.shipment.services.utils.Generated;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter @Generated
public class DocumentOption implements Serializable {
    private String type;
    private Boolean upload;
}
