package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
@SuppressWarnings("java:S1948")
public class MasterDataDescriptionResponse implements IRunnerResponse {
    private String fieldName;
    private Object fieldValue;
    private String itemDescription;
}
