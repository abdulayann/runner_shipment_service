package com.dpw.runner.shipment.services.dto.response.npm;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@SuppressWarnings("java:S1948")
public class NPMContractsResponse implements IRunnerResponse {
    @JsonProperty("contracts")
    private List<NPMContractResponse> contracts;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class NPMContractResponse {
        @JsonProperty("contract_id")
        private String contract_id;
        @JsonProperty("parent_contract_id")
        private String parent_contract_id;
        @JsonProperty("origin")
        private String origin;
        @JsonProperty("destination")
        private String destination;
        @JsonProperty("product_type")
        private String product_type;
        @JsonProperty("origin_name")
        private String origin_name;
        @JsonProperty("destination_name")
        private String destination_name;
        @JsonProperty("meta")
        private ListContractResponse.Meta meta;
        @JsonProperty("valid_till")
        private LocalDateTime validTill;
        @JsonProperty("dg_class")
        private List<String> dgClass;
        @JsonProperty("dg_un_num")
        private List<String> dgUnNum;
        @JsonProperty("load_types")
        private List<String> loadTypes;
    }
}

