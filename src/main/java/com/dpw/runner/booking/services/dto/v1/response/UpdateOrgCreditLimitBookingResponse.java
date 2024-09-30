package com.dpw.runner.booking.services.dto.v1.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class UpdateOrgCreditLimitBookingResponse implements IRunnerResponse {
    @JsonProperty("Success")
    private Boolean success;
    @JsonProperty("CustomerIdentifier")
    private String customerIdentifier;
    @JsonProperty("Error")
    private String error;
}
