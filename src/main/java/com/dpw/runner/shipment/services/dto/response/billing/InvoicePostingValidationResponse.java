package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class InvoicePostingValidationResponse implements IRunnerResponse {

    private String shipmentGuid;
    private List<ModuleValidationFieldType> missingFields;
}
