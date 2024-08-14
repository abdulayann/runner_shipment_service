package com.dpw.runner.shipment.services.dto.request.billing;

import java.util.List;
import javax.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class InvoicePostingValidationRequest {

    @NotEmpty(message = "Shipment guids cannot be null or empty")
    private List<String> shipmentGuids;
}
