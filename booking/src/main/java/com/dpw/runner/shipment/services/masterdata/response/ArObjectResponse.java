package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ArObjectResponse {
    @JsonProperty("InvoiceDate")
    private LocalDateTime invoiceDate;
}
