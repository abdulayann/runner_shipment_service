package com.dpw.runner.booking.services.dto.v1.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class CreditTaskDetailsResponse implements IRunnerResponse {
    private boolean success;
    private String message;
}
