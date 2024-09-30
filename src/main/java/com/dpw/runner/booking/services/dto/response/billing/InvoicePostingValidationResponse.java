package com.dpw.runner.booking.services.dto.response.billing;

import com.dpw.runner.booking.services.commons.enums.ModuleValidationFieldType;
import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class InvoicePostingValidationResponse implements IRunnerResponse {

    private String shipmentGuid;
    private List<ModuleValidationFieldType> missingFields;
}
