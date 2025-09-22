package com.dpw.runner.shipment.services.dto.v3.response;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class ExportExcelResponse implements Serializable {
    private boolean isEmailSent;
}
