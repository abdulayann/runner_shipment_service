package com.dpw.runner.booking.services.dto.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class MasterDataDescriptionResponse implements IRunnerResponse {
    private String fieldName;
    private Object fieldValue;
    private String itemDescription;
}
