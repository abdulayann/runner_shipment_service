package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SuppressWarnings("java:S1948")
public class CheckCreditBalanceFusionRequest implements IRunnerRequest {
    @JsonProperty("req_Params")
    private ReqParams req_Params;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ReqParams implements IRunnerRequest{
        @JsonProperty("Calling_System")
        private String Calling_System;
        @JsonProperty("Site_number")
        private String Site_number;
        @JsonProperty("Account_number")
        private String Account_number;
        @JsonProperty("Bu_id")
        private String Bu_id;
    }
}
