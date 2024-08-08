package com.dpw.runner.shipment.services.dto.response.billing;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.utils.Generated;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Generated
public class BillingBaseResponse implements IRunnerResponse {

    private List<String> errors;
    private String metadata;
    private String message;
    private boolean success;
}
