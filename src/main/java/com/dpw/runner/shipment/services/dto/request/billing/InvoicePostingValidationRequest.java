package com.dpw.runner.shipment.services.dto.request.billing;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotEmpty;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class InvoicePostingValidationRequest {

    @NotEmpty(message = "Shipment guids cannot be null or empty")
    private List<String> shipmentGuids;
}
