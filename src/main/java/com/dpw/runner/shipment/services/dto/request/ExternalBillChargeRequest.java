package com.dpw.runner.shipment.services.dto.request;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ExternalBillChargeRequest implements Serializable {
    @NotEmpty(message = "extRefId can not be empty")
    private String extRefId;
    private String guid;
    private boolean postARInvoice;
    private boolean postAPInvoice;
    private APMetaData apMetaData;
    private BillChargesRequest billChargeRequest;
}