package com.dpw.runner.shipment.services.dto.patchRequest;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.openapitools.jackson.nullable.JsonNullable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CarrierPatchRequest  extends CommonRequest implements IRunnerRequest {
    private JsonNullable<String> shippingLine;
    private JsonNullable<String> vessel;
    private JsonNullable<String> voyage;
    private JsonNullable<String> flightNumber;
    private JsonNullable<String> aircraftType;
}
